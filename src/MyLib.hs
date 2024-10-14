{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-stg-from-core -ddump-cmm -ddump-to-file #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

module MyLib where

import Array (StrictSmallArray)
import Array qualified
import Control.DeepSeq (NFData (..))
import Data.Bits hiding (shift)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.Maybe qualified as Maybe
import Data.Primitive
import Data.Primitive.Contiguous (Contiguous, ContiguousU, Element)
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Word (Word64)
import GHC.Exts qualified as Exts
import GHC.IsList (IsList (..))
import Numeric (showBin)
import Storage (ArrayOf, Storage (..), StrictStorage (..))
import Prelude hiding (lookup)
import Collision qualified

#define BIT_PARTITION_SIZE 5
#define HASH_CODE_LENGTH (1 `unsafeShiftL` BIT_PARTITION_SIZE)
#define BIT_PARTITION_MASK (HASH_CODE_LENGTH - 1)

type MapBL = Map Boxed Lazy

type MapBB = Map Boxed (Strict Boxed)

type MapBU = Map Boxed (Strict Unboxed)

type MapUL = Map Unboxed Lazy

type MapUB = Map Unboxed (Strict Boxed)

type MapUU = Map Unboxed (Strict Unboxed)

pattern EmptyMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  Map keyStorage valStorage k v
{-# INLINE EmptyMap #-}
pattern EmptyMap <- (matchMap -> (# (# #) | | #))
  where
    EmptyMap = emptyMap

pattern SingletonMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  k -> v -> Map keyStorage valStorage k v
{-# INLINE SingletonMap #-}
pattern SingletonMap k v <- (matchMap -> (# | (# k, v #) | #))
  where
    SingletonMap = singletonMap

pattern ManyMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  Word -> MapNode keyStorage valStorage k v -> Map keyStorage valStorage k v
{-# INLINE ManyMap #-}
pattern ManyMap mapsize node <- (matchMap -> (# | | (# mapsize, node #) #))
  where
    ManyMap = manyMap

{-# COMPLETE EmptyMap, SingletonMap, ManyMap #-}

{-# INLINE MapNode #-}
pattern MapNode :: (MapRepr keyStorage valStorage k v) => Bitmap -> (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> StrictSmallArray (MapNode keyStorage valStorage k v) -> MapNode keyStorage valStorage k v
pattern MapNode bitmap keys vals children <- (unpackNode -> (# bitmap, keys, vals, children #))
  where
    MapNode bitmap keys vals children = packNode (# bitmap, keys, vals, children #)

pattern CollisionNode :: (MapRepr keyStorage valStorage k v) => (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> MapNode keyStorage valStorage k v
{-# INLINE CollisionNode #-}
pattern CollisionNode keys vals <- (unpackNode -> (# (== 0) -> True, keys, vals, _ #))
  where
    CollisionNode keys vals = packNode (# 0, keys, vals, Contiguous.empty #)

pattern CompactNode :: (MapRepr keyStorage valStorage k v) => Bitmap -> (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> StrictSmallArray (MapNode keyStorage valStorage k v) -> MapNode keyStorage valStorage k v
{-# INLINE CompactNode #-}
pattern CompactNode bitmap keys vals children <- (unpackNode -> (# (isNonZeroBitmap -> (# | bitmap #)), keys, vals, children #))
  where
    CompactNode bitmap keys vals children = packNode (# bitmap, keys, vals, children #)

{-# INLINE isNonZeroBitmap #-}
isNonZeroBitmap :: Bitmap -> (# (# #) | Bitmap #)
isNonZeroBitmap 0 = (# (# #) | #)
isNonZeroBitmap b = (# | b #)

{-# COMPLETE CollisionNode, CompactNode #-}

{-# COMPLETE MapNode #-}

-- | A CHAMP-based Hashmap.
--
-- This is implemented as a typeclass containing a data family called `Map`
-- rather than a plain datatype,
-- to ensure GHC can unbox all intermediate polymorphic constructors
-- (that depend on the concrete types of `keyStorage` and `valStorage`).
--
-- Conceptually, you can think of it as:
--
-- ```
-- data Map (keys :: StrictStorage) (vals :: Storage) k v = EmptyMap | SingletonMap !k v | ManyMap (MapNode k v)
--
-- data MapNode keys vals k v
--    = CollisionNode !(ArrayOf (Strict keys)) !(ArrayOf vals)
--    | CompactNode !Bitmap !Bitmap !(ArrayOf (Strict keys)) !(ArrayOf vals) !(StrictSmallArray (MapNode keys vals k v))
-- ```
-- with the following tricks:
-- - We only store a single 64-bit bitmap (taking up one word) rather than two separate 32-bit bitmaps,
--  and use its lower/higher 32 bits using masking and shifting instead, saving one word per map node.
-- - There is no special `CollisionNode` variant.
--  Instead, we disambiguate using the special bitmap value '0'
--  which can never occur in a valid CompactNode as they are never empty.
-- - As mentioned above, we make sure that GHC unpacks the intermediate array boxes,
--  so we store the `SmallArray#` resp `ByteArray#` pointers directly.
--  This results in one word saved for the outer map and three more words saved per map node.
-- - Finally, we make sure that internal map nodes are stored as `UnliftedType` (AKA `TYPE 'BoxedRep 'Unlifted`)
--  inside their array, as we're always strict in the tree-spine of the CHAMP map.
--  This means GHC will skip any thunk-forcing code whenever reading/recursing
class (ContiguousU (ArrayOf (Strict keyStorage)), ContiguousU (ArrayOf (valStorage)), Element (ArrayOf (Strict keyStorage)) k, Element (ArrayOf valStorage) v) => MapRepr (keyStorage :: StrictStorage) (valStorage :: Storage) k v where
  data Map keyStorage valStorage k v
  data MapNode keyStorage valStorage k v
  packNode :: (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, StrictSmallArray (MapNode keyStorage valStorage k v) #) -> MapNode keyStorage valStorage k v
  unpackNode :: MapNode keyStorage valStorage k v -> (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, StrictSmallArray (MapNode keyStorage valStorage k v) #)
  manyMap :: Word -> MapNode keyStorage valStorage k v -> Map keyStorage valStorage k v
  emptyMap :: Map keyStorage valStorage k v
  singletonMap :: k -> v -> Map keyStorage valStorage k v
  matchMap :: Map keyStorage valStorage k v -> (# (# #) | (# k, v #) | (# Word, MapNode keyStorage valStorage k v #) #)

#define MAP_NODE_NAME(name) MapNode/**/_/**/name

#define MAP_NODE_FIELDS(keystorage, valstorage) \
     {-# UNPACK #-} !Bitmap \
     {-# UNPACK #-} !((ArrayOf (Strict (keystorage))) k) \
     {-# UNPACK #-} !((ArrayOf (valstorage)) v) \
     {-# UNPACK #-} !(StrictSmallArray (MapNode (keystorage) (valstorage) k v))

#define map_repr_instance(name, keystorage, valstorage, constraints)                                                     \
instance constraints => MapRepr (keystorage) (valstorage) k v where                                                      \
{ {-# INLINE unpackNode #-}                                                                                              \
; unpackNode (MAP_NODE_NAME(name) b keys vals children) = (# b, keys, vals, children #)                                  \
; {-# INLINE packNode #-}                                                                                                \
; packNode (# b, keys, vals, children #) = (MAP_NODE_NAME(name) b keys vals children)                                    \
; {-# INLINE emptyMap #-}                                                                                                \
; emptyMap = EmptyMap_/**/name                                                                                           \
; {-# INLINE singletonMap #-}                                                                                            \
; singletonMap !k v = SingletonMap_/**/name k v                                                                          \
; {-# INLINE manyMap #-}                                                                                                 \
; manyMap mapsize (MAP_NODE_NAME(name) b keys vals children) = ManyMap_/**/name mapsize b keys vals children             \
; {-# INLINE matchMap #-}                                                                                                \
; matchMap = \case {                                                                                                     \
; EmptyMap_/**/name -> (# (# #) | | #)                                                                                   \
; SingletonMap_/**/name k v -> (#  | (# k, v #) | #)                                                                     \
; ManyMap_/**/name mapsize b keys vals children -> (# | | (# mapsize, MAP_NODE_NAME(name) b keys vals children #) #) }   \
; data MapNode (keystorage) (valstorage) k v = MAP_NODE_NAME(name) MAP_NODE_FIELDS(keystorage, valstorage)               \
; data Map (keystorage) (valstorage) k v                                                                                 \
  = EmptyMap_/**/name                                                                                                    \
  | SingletonMap_/**/name !k v                                                                                           \
  | ManyMap_/**/name !Word MAP_NODE_FIELDS(keystorage, valstorage)                                                       \
}

map_repr_instance (Boxed_Lazy, Boxed, Lazy, ())
map_repr_instance (Boxed_Boxed, Boxed, Strict Boxed, ())
map_repr_instance (Boxed_Unboxed, Boxed, Strict Unboxed, (Prim v))

map_repr_instance (Unboxed_Lazy, Unboxed, Lazy, (Prim k))
map_repr_instance (Unboxed_Boxed, Unboxed, Strict Boxed, (Prim k))
map_repr_instance (Unboxed_Unboxed, Unboxed, Strict Unboxed, (Prim k, Prim v))

someFunc = undefined

{-# INLINE null #-}
null :: (MapRepr keys vals k v) => Map keys vals k v -> Bool
null EmptyMap = True
null _ = False

size :: (MapRepr keys vals k v) => Map keys vals k v -> Int
{-# INLINE size #-}
size EmptyMap = 0
size (SingletonMap _k _v) = 1
size (ManyMap s _) = fromIntegral s

{-# INLINE empty #-}
empty :: (MapRepr keys vals k v) => Map keys vals k v
empty = EmptyMap

{-# INLINE singleton #-}
singleton :: (MapRepr keys vals k v) => k -> v -> Map keys vals k v
singleton !k v = SingletonMap k v

data Location = Inline | InChild | Nowhere

{-# INLINE bitposLocation #-}
bitposLocation node@(CompactNode bitmap _ _ _) bitpos
  | bitmap .&. bitpos /= 0 = Inline
  | (childrenBitmap node) .&. bitpos /= 0 = InChild
  | otherwise = Nowhere

-- \(O(n)\) Construct a map with the supplied key-value mappings.
-- 
-- If the list contains duplicate keys, later mappings take precedence.
--
-- NOTE: Since there is no unsafeInsert yet,
-- the current implementation is slower than necessary.
fromList :: (Hashable k, MapRepr keys vals k v) => [(k, v)] -> Map keys vals k v
{-# INLINE fromList #-}
fromList = Foldable.foldl' (\m (k, v) -> insert k v m) empty

-- | \(O(n)\) Return a list of this map's elements (key-value pairs).
--
-- The resulting list is produced lazily and may participate in list fusion.
-- The order of its elements is unspecified.
toList :: (MapRepr keys vals k v) => Map keys vals k v -> [(k, v)]
{-# INLINE toList #-}
toList hashmap = Exts.build (\fusedCons fusedNil -> foldrWithKey (\k v xs -> (k, v) `fusedCons` xs) fusedNil hashmap)

-- | \(O(\log32 n)\) Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Hashable k, MapRepr keys vals k v) => k -> v -> Map keys vals k v -> Map keys vals k v
{-# INLINE insert #-}
insert !k v !m = case matchMap m of
  (# (# #) | | #) -> singleton k v
  (# | (# k', v' #) | #) ->
    if k == k'
      then singleton k' v'
      else
        let !(# size, node #) = insert' (hash k) k v 0 $ (MapNode (maskToBitpos (hashToMask 0 (hash k'))) (Contiguous.singleton k) (Contiguous.singleton v) Contiguous.empty)
         in ManyMap (1 + Exts.W# size) node
  (# | | (# size, node0 #) #) ->
    let (# didIGrow, node' #) = insert' (hash k) k v 0 node0
     in ManyMap (size + Exts.W# didIGrow) node'

{-# INLINEABLE insert' #-}
insert' !h !k v !shift !node@(CollisionNode _ _) = (# 1##, insertCollision k v node #)
insert' !h !k v !shift !node@(CompactNode !bitmap !keys !vals !children) =
  let !bitpos = maskToBitpos $ hashToMask shift h
   in case bitposLocation node bitpos of
        Inline ->
          -- exists inline; potentially turn inline to subnode with two keys
          insertMergeWithInline bitpos k v h shift node
        InChild ->
          -- Exists in child, insert in there and make sure this node contains the updated child
          let child = indexChild node bitpos
              (# didIGrow, child' #) = insert' h k v (nextShift shift) child
           in if child' `ptrEq` child
                then (# 0##, node #)
                else (# didIGrow, CompactNode bitmap keys vals (Contiguous.replaceAt children (childrenIndex node bitpos) child') #)
        Nowhere ->
          -- Doesn't exist yet, we can insert inline
          (# 1##, insertNewInline bitpos k v node #)

-- {-# SPECIALIZE insert :: Hashable k => k -> v -> MapBL k v -> MapBL k v #-}
-- {-# SPECIALIZE insert :: Hashable k => k -> v -> MapBB k v -> MapBB k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim v) => k -> v -> MapBU k v -> MapBU k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k) => k -> v -> MapUL k v -> MapUL k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k) => k -> v -> MapUB k v -> MapUB k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k, Prim v) => k -> v -> MapUU k v -> MapUU k v #-}
-- {-# SPECIALIZE insert :: Int -> Int -> MapUU Int Int -> MapUU Int Int #-}

-- Collisions are appended at the end
-- Note that we cannot insert them in sorted order
-- (which would theoretically allow a binary search on lookup)
-- because we don't have an `Ord` instance.
{-# INLINE insertCollision #-}
insertCollision :: (MapRepr keys vals k v) => k -> v -> MapNode keys vals k v -> MapNode keys vals k v
insertCollision k v (CollisionNode keys vals) =
  let idx = Contiguous.size keys
      keys' = Contiguous.insertAt keys idx k
      vals' = Contiguous.insertAt vals idx v
   in CollisionNode keys' vals'

{-# INLINE insertNewInline #-}
insertNewInline :: (MapRepr keys vals k v) => Bitmap -> k -> v -> MapNode keys vals k v -> MapNode keys vals k v
insertNewInline bitpos k v node@(MapNode bitmap keys vals children) =
  let bitmap' = bitmap .|. bitpos
      idx = dataIndex node bitpos
      keys' = Contiguous.insertAt keys idx k
      vals' = Contiguous.insertAt vals idx v
   in MapNode bitmap' keys' vals' children

{-# INLINE insertMergeWithInline #-}
insertMergeWithInline :: (Hashable k, MapRepr keys vals k v) => Bitmap -> k -> v -> Hash -> Word -> MapNode keys vals k v -> (# Exts.Word#, MapNode keys vals k v #)
insertMergeWithInline bitpos k v h shift node@(CompactNode bitmap keys vals children) =
  let bitmap' = bitmap .^. bitpos .|. (bitpos `unsafeShiftL` HASH_CODE_LENGTH)
      idx = dataIndex node bitpos
      existingKey = indexKey node bitpos
      (# existingVal #) = indexVal# node bitpos
   in if
        | existingKey == k && v `ptrEq` existingVal -> (# 0##, node #)
        | existingKey == k -> (# 1##, CompactNode bitmap' keys (Contiguous.replaceAt vals idx v) children #)
        | otherwise ->
            let newIdx = childrenIndex node bitpos
                keys' = Contiguous.deleteAt keys idx
                vals' = Contiguous.deleteAt vals idx
                child = pairNode existingKey existingVal (hash existingKey) k v h (nextShift shift)
                children' = Contiguous.insertAt children newIdx child
             in (# 1##, CompactNode bitmap' keys' vals' children' #)

{-# INLINE pairNode #-}
pairNode :: (MapRepr keys vals k v) => k -> v -> Hash -> k -> v -> Hash -> Word -> MapNode keys vals k v
pairNode k1 v1 h1 k2 v2 h2 shift
  | shift >= HASH_CODE_LENGTH = CollisionNode (Contiguous.doubleton k1 k2) (Contiguous.doubleton v1 v2)
  | otherwise =
      let mask1 = hashToMask shift h1
          mask2 = hashToMask shift h2
       in if mask1 /= mask2
            then
              -- Both fit on this level
              mergeCompactInline k1 v1 h1 k2 v2 h2 shift
            else
              -- Both fit on the _next_ level
              let child = mergeCompactInline k1 v1 h1 k2 v2 h2 (nextShift shift)
                  bitmap = maskToBitpos mask1 `unsafeShiftL` HASH_CODE_LENGTH
               in CompactNode bitmap Contiguous.empty Contiguous.empty (Contiguous.singleton child)

{-# INLINE mergeCompactInline #-}
mergeCompactInline k1 v1 h1 k2 v2 h2 shift =
  let !mask0@(Mask (Exts.W# i1)) = hashToMask shift h1
      !mask1@(Mask (Exts.W# i2)) = hashToMask shift h2
      !bitmap = maskToBitpos mask0 .|. maskToBitpos mask1
      !c = Exts.I# (i1 `Exts.ltWord#` i2)
      keys = Array.doubletonBranchless c k1 k2
      vals = Array.doubletonBranchless c v1 v2
   in CompactNode bitmap keys vals Contiguous.empty

{-# INLINE lookup #-}
lookup :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> Map keys vals k v -> Maybe v
lookup !k m = case matchMap m of
  (# (# #) | | #) -> Nothing
  (# | (# k', v #) | #) -> if k == k' then Just v else Nothing
  (# | | (# _size, node0 #) #) ->
    -- NOTE: Manually inlining the first iteration of lookup'
    -- (which will _never_ be in a collision node)
    -- results in a noticeable speedup for maps that have at most 32 elements.
    let !h = hash k 
        !bitpos = maskToBitpos $ hashToMask 0 h
    in case bitposLocation node0 bitpos of
        Nowhere -> Nothing
        Inline ->
            let k' = indexKey node0 bitpos
                (# v #) = indexVal# node0 bitpos
            in if k == k' then Just v else Nothing
        InChild -> lookup' (nextShift 0) (indexChild node0 bitpos)
            where
                {-# INLINEABLE lookup' #-}
                lookup' !_s !(CollisionNode keys vals) =
                    keys
                        & Contiguous.findIndex (\k' -> k' == k)
                        & fmap (Contiguous.index vals)
                lookup' !s !node@(CompactNode _bitmap _keys _vals _children) =
                    let !bitpos = maskToBitpos $ hashToMask s h
                    in case bitposLocation node bitpos of
                            Nowhere -> Nothing
                            Inline ->
                                let k' = indexKey node bitpos
                                    (# v #) = indexVal# node bitpos
                                in if k == k' then Just v else Nothing
                            InChild -> lookup' (nextShift s) (indexChild node bitpos)

-- mylookup :: Int -> MapUU Int Int -> Maybe Int
-- mylookup = lookup

{-# INLINE indexKey #-}
indexKey :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> k
indexKey (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexKey node@(CompactNode _bitmap keys _vals _children) bitpos =
    Contiguous.index keys (dataIndex node bitpos)

{-# INLINE indexVal# #-}
indexVal# :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> (# v #)
indexVal# (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexVal# node@(CompactNode _bitmap _keys vals _children) bitpos =
    Contiguous.index# vals (dataIndex node bitpos)

{-# INLINE indexChild #-}
indexChild :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> MapNode keys vals k v
indexChild (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexChild node@(CompactNode _bitmap _keys _vals children) bitpos =
    Contiguous.index children (childrenIndex node bitpos)

member :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> Map keys vals k v -> Bool
{-# INLINEABLE member #-}
member k v = Maybe.isJust $ lookup k v

instance (MapRepr keys vals k v, Eq v, Eq k) => Eq (Map keys vals k v) where
  {-# INLINEABLE (==) #-}
  EmptyMap == EmptyMap = True
  (SingletonMap k v) == (SingletonMap k' v') = (k == k') && (v == v')
  (ManyMap sz node) == (ManyMap sz' node') = sz == sz' && node == node'
  _ == _ = False

instance (MapRepr keys vals k v, Eq v, Eq k) => Eq (MapNode keys vals k v) where
  {-# INLINEABLE (==) #-}
  n1 == n2 | n1 `ptrEq` n2 = True
  (CollisionNode keys vals) == (CollisionNode keys' vals') = 
    Collision.isPermutationBy (==) (Contiguous.toList keys) (Contiguous.toList keys')
    && Collision.isPermutationBy (==) (Contiguous.toList vals) (Contiguous.toList vals')
  (CompactNode b1 k1 v1 c1) == (CompactNode b2 k2 v2 c2) =
    b1 == b2
      && (k1 `Contiguous.same` k2 || k1 `Contiguous.equals` k2)
      && (v1 `Contiguous.same` v2 || v1 `Contiguous.equals` v2)
      && (c1 `Contiguous.same` c2 || c1 `Contiguous.equals` c2) -- Here we recurse
  _ == _ = False

myeq :: MapUU Int Int -> MapUU Int Int -> Bool
myeq a b = a == b




instance (Hashable k, Eq k, MapRepr keys vals k v) => IsList (Map keys vals k v) where
  type Item (Map keys vals k v) = (k, v)
  {-# INLINE toList #-}
  toList = MyLib.toList
  {-# INLINE fromList #-}
  fromList = MyLib.fromList

instance (NFData k, NFData v, MapRepr keys vals k v) => NFData (Map keys vals k v) where
  {-# INLINE rnf #-}
  rnf EmptyMap = ()
  rnf (SingletonMap k v) = rnf k `seq` rnf v
  rnf (ManyMap _size node) = rnf node

instance {-# OVERLAPPABLE #-} (NFData k, NFData v, MapRepr keys vals k v) => NFData (MapNode keys vals k v) where
  {-# INLINE rnf #-}
  rnf (CollisionNode keys vals) = Contiguous.rnf keys `seq` Contiguous.rnf vals
  rnf (MapNode _bitmap keys vals children) =
    Contiguous.rnf keys `seq` Contiguous.rnf vals `seq` Contiguous.rnf children

instance {-# OVERLAPS #-} (NFData k, NFData v) => NFData (MapNode Unboxed (Strict Unboxed) k v) where
  {-# INLINE rnf #-}
  rnf !_ = ()

instance Functor (MapBL k) where
  {-# INLINE fmap #-}
  fmap = MyLib.map

instance Functor (MapBB k) where
  {-# INLINE fmap #-}
  fmap = MyLib.map

instance (Prim k) => Functor (MapUL k) where
  {-# INLINE fmap #-}
  fmap = MyLib.map

instance (Prim k) => Functor (MapUB k) where
  {-# INLINE fmap #-}
  fmap = MyLib.map

-- NOTE: Supporting Functor for MapXU is impossible
-- without being very tricksy

-- | Map a function over the values in a hashmap
--
-- O(n)
map :: (MapRepr keys vals k a, MapRepr keys vals k b) => (a -> b) -> Map keys vals k a -> Map keys vals k b
{-# INLINE map #-}
map = map'

-- | Map a function over the values in a hashmap,
-- allowing to switch the value storage type.
--
-- that is: you can switch between MapBL <-> MapBB <-> MapBU,
-- or switch between MapUL <-> MapUB <-> MapUU.
--
-- When using this function, you will need 
-- to specify the type of the output map.
--
-- O(n)
--
-- Example:
--
-- >>> mymap = fromList [(1, 10), (2, 20)] :: MapUU Int Int
-- >>> map' show mymap :: MapUB Int String 
-- fromList [(1, "10"), (2, "20")]
map' :: (MapRepr keys as k a, MapRepr keys bs k b) => (a -> b) -> Map keys as k a -> Map keys bs k b
{-# INLINE map' #-}
map' !_f EmptyMap = EmptyMap
map' !f (SingletonMap k v) = SingletonMap k (f v)
map' !f (ManyMap sz node) = ManyMap sz (mapNode node)
  where
    mapNode (CollisionNode keys vals) = (CollisionNode keys (Contiguous.map f vals))
    mapNode (CompactNode bitmap keys vals children) =
      let
        vals' = Contiguous.map f vals
        children' = Contiguous.map mapNode children
      in
        CompactNode bitmap keys vals' children'

instance Foldable (MapBL k) where
  {-# INLINE foldr #-}
  foldr = MyLib.foldr
  {-# INLINE foldr' #-}
  foldr' = MyLib.foldr'
  {-# INLINE foldl #-}
  foldl = MyLib.foldl
  {-# INLINE foldl' #-}
  foldl' = MyLib.foldl'
  {-# INLINE null #-}
  null = MyLib.null
  {-# INLINE length #-}
  length = MyLib.size

-- TODO: manual impls of the other funs
-- as those are more efficient

instance Foldable (MapBB k) where
  {-# INLINE foldr #-}
  foldr = MyLib.foldr
  {-# INLINE foldr' #-}
  foldr' = MyLib.foldr'
  {-# INLINE foldl #-}
  foldl = MyLib.foldl
  {-# INLINE foldl' #-}
  foldl' = MyLib.foldl'
  {-# INLINE null #-}
  null = MyLib.null
  {-# INLINE length #-}
  length = MyLib.size

-- TODO: manual impls of the other funs
-- as those are more efficient


instance (Prim k) => Foldable (MapUL k) where
  {-# INLINE foldr #-}
  foldr = MyLib.foldr
  {-# INLINE foldr' #-}
  foldr' = MyLib.foldr'
  {-# INLINE foldl #-}
  foldl = MyLib.foldl
  {-# INLINE foldl' #-}
  foldl' = MyLib.foldl'
  {-# INLINE null #-}
  null = MyLib.null
  {-# INLINE length #-}
  length = MyLib.size

-- TODO: manual impls of the other funs
-- as those are more efficient

instance (Prim k) => Foldable (MapUB k) where
  {-# INLINE foldr #-}
  foldr = MyLib.foldr
  {-# INLINE foldr' #-}
  foldr' = MyLib.foldr'
  {-# INLINE foldl #-}
  foldl = MyLib.foldl
  {-# INLINE foldl' #-}
  foldl' = MyLib.foldl'
  {-# INLINE null #-}
  null = MyLib.null
  {-# INLINE length #-}
  length = MyLib.size

-- TODO: manual impls of the other funs
-- as those are more efficient

-- TODO: Would be nice to offer Foldable for
-- maps with unboxed values,
-- but without some tricksy workaround this is impossible.

-- instance Foldable (MapBU k) where
--   foldr = MyLib.foldr
--   foldr' = MyLib.foldr'
--   -- TODO: manual impls of the other funs
--   -- as those are more efficient

-- instance (Prim k) => Foldable (MapUU k) where
--   foldr = MyLib.foldr
--   foldr' = MyLib.foldr'
--   -- TODO: manual impls of the other funs
--   -- as those are more efficient

{-# INLINE foldr #-}
foldr :: (MapRepr keys vals k v) => (v -> r -> r) -> r -> Map keys vals k v -> r
foldr f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _k, v #) | #) -> f v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap _keys !vals !children) z =
      z
        & flip (Contiguous.foldr f) vals
        & flip (Contiguous.foldr go) children

{-# INLINE foldl #-}
foldl :: (MapRepr keys vals k v) => (r -> v -> r) -> r -> Map keys vals k v -> r
foldl f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _k, v #) | #) -> f z0 v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go z (MapNode _bitmap _keys !vals !children) =
      z
        & flip (Contiguous.foldl go) children
        & flip (Contiguous.foldl f) vals


{-# INLINE foldr' #-}
foldr' :: (MapRepr keys vals k v) => (v -> r -> r) -> r -> Map keys vals k v -> r
foldr' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _k, v #) | #) -> f v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap _keys !vals !children) !z =
      z
        & flip (Contiguous.foldr' f) vals
        & flip (Contiguous.foldr' go) children

{-# INLINE foldl' #-}
foldl' :: (MapRepr keys vals k v) => (r -> v -> r) -> r -> Map keys vals k v -> r
foldl' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _k, v #) | #) -> f z0 v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go !z (MapNode _bitmap _keys !vals !children) =
      z
        & flip (Contiguous.foldl' go) children
        & flip (Contiguous.foldl' f) vals

{-# INLINE foldrWithKey #-}
foldrWithKey :: (MapRepr keys vals k v) => (k -> v -> r -> r) -> r -> Map keys vals k v -> r
foldrWithKey f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# k, v #) | #) -> f k v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap keys !vals !children) z =
      (Contiguous.foldrZipWith f) z keys vals
        & flip (Contiguous.foldr go) children

{-# INLINE foldlWithKey' #-}
foldlWithKey' :: (MapRepr keys vals k v) => (r -> k -> v -> r) -> r -> Map keys vals k v -> r
foldlWithKey' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# k, v #) | #) -> f z0 k v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go !z (MapNode _bitmap keys !vals !children) =
      (Contiguous.foldlZipWith' f) z keys vals
        & flip (Contiguous.foldl' go) children

-- | \(O(n)\) Returns a list of the map's keys.
--
-- The resulting list is produced lazily and participates in list fusion.
-- The order of keys is unspecified.
keys :: MapRepr keys vals k v => Map keys vals k v -> [k]
{-# INLINE keys #-}
keys = Prelude.map fst . MyLib.toList

-- | \(O(n)\) Returns a list of the map's elements (or values).
--
-- The resulting list is produced lazily and participates in list fusion.
-- The order of elements is unspecified.
elems :: MapRepr keys vals k v => Map keys vals k v -> [v]
{-# INLINE elems #-}
elems = Prelude.map snd . MyLib.toList

instance (Show k, Show v, MapRepr keys vals k v) => Show (Map keys vals k v) where
    show m = "fromList " <> show (MyLib.toList m)

-- Enable for debugging: 
-- instance (Show (ArrayOf (Strict keys) k), Show (ArrayOf vals v), Show k, Show v, MapRepr keys vals k v) => Show (Map keys vals k v) where
--   show EmptyMap = "EmptyMap"
--   show (SingletonMap k v) = "(SingletonMap " <> show k <> " " <> show v <> ")"
--   show (ManyMap _size node) = "(ManyMap " <> show node <> ")"

-- instance (Show (ArrayOf (Strict keys) k), Show (ArrayOf vals v), Show k, Show v, MapRepr keys vals k v) => Show (MapNode keys vals k v) where
--   show (CollisionNode keys vals) = "(CollisionNode " <> show keys <> " " <> show vals <> ")"
--   show (CompactNode bitmap keys vals children) = "(CompactNode " <> show bitmap <> " " <> show keys <> " " <> show vals <> " " <> show children <> ")"

-- mysumOne :: MapBL Int Int -> Int
-- mysumOne = foldr' (+) 0

-- mysumTwo :: MapBL Int Int -> Int
-- mysumTwo = foldr'2 (+) 0

-- Helper functions & types:

-- Inside a MapNode,
-- the lower 32 bits indicate the inline leaves
-- and the higher 32 bits indicate child nodes
newtype Bitmap = Bitmap Word64
  deriving (Eq, Ord, Num, Bits, Enum, Real, Integral)

instance Show Bitmap where
  -- \| Shows a bitmap in binary, with the lowest bit on the left
  -- and always padded to 32 bits
  show :: Bitmap -> String
  show bitmap = "( " <> show32 bitmap <> ", " <> show32 (bitmap `unsafeShiftR` HASH_CODE_LENGTH) <> ")"
    where
      show32 b = take 32 $ reverse (showBin b "") <> repeat '0'

newtype Mask = Mask Word
  deriving (Eq, Ord, Show)

newtype Hash = Hash Word
  deriving (Eq, Ord, Show)

hash :: (Hashable a) => a -> Hash
hash x = Hash $ fromIntegral $ Hashable.hash x

{-# INLINE hashToMask #-}
hashToMask :: Word -> Hash -> Mask
hashToMask (fromIntegral -> depth) (Hash keyhash) = Mask $ (fromIntegral $ keyhash `unsafeShiftR` depth) .&. BIT_PARTITION_MASK

{-# INLINE maskToBitpos #-}
maskToBitpos :: Mask -> Bitmap
maskToBitpos (Mask mask) = 1 `unsafeShiftL` (fromIntegral mask)

{-# INLINE childrenBitmap #-}
childrenBitmap :: (MapRepr keys vals k v) => MapNode keys vals k v -> Bitmap
childrenBitmap (MapNode bitmap _ _ _) = bitmap `unsafeShiftR` 32

{-# INLINE leavesBitmap #-}
leavesBitmap :: (MapRepr keys vals k v) => MapNode keys vals k v -> Bitmap
leavesBitmap (MapNode bitmap _ _ _) = bitmap .&. (1 `unsafeShiftL` 32 - 1)

{-# INLINE leavesCount #-}
leavesCount :: (Element (ArrayOf (Strict keys)) k) => (MapRepr keys vals k v) => MapNode keys vals k v -> Int
leavesCount (CollisionNode keys _) = Contiguous.size keys `div` 2 -- The count of leaves in a CollisionNode
leavesCount node = popCount $ leavesBitmap node

{-# INLINE childrenCount #-}
childrenCount :: (MapRepr keys vals k v) => MapNode keys vals k v -> Int
childrenCount node = popCount $ childrenBitmap node

-- | Given a Bitpos, returns the index into the elems array of a node (looking at how many other elems are already in the node's databitmap)
dataIndex :: (MapRepr keys vals k v) => MapNode keys vals k v -> Bitmap -> Int
{-# INLINE dataIndex #-}
dataIndex (MapNode bitmap _ _ _) bitpos =
  popCount $ bitmap .&. (bitpos - 1)

-- | Given a Bitpos, returns the index into the elems array of a node (looking at how many other elems are already in the node's nodebitmap)
childrenIndex :: (MapRepr keys vals k v) => MapNode keys vals k v -> Bitmap -> Int
{-# INLINE childrenIndex #-}
childrenIndex node bitpos =
  popCount $ (childrenBitmap node) .&. (bitpos - 1)

nextShift :: (Num a) => a -> a
nextShift s = s + BIT_PARTITION_SIZE

-- intsToList :: MapBL Int Int -> [(Int, Int)]
-- intsToList = MyLib.toList

-- sumStrict :: MapBB Int Int -> Int
-- sumStrict = foldr' (+) 0

-- sumLazy :: MapBL Int Int -> Int
-- sumLazy = foldr' (+) 0

------------------------------------------------------------------------
-- Pointer equality

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects, or things being unpacked/repacked.)
-- but never false positives
ptrEq :: a -> a -> Bool
ptrEq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINE ptrEq #-}

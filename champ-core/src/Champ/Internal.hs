{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -dsuppress-uniques -ddump-stg-from-core -ddump-cmm -ddump-to-file #-}
{-# OPTIONS_GHC -funbox-strict-fields -O2 #-}

-- We'd really like to use the names 'keys', 'elems', 'vals'
-- for a bunch of different things.
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module Champ.Internal where

import Champ.Internal.Array (Array, StrictSmallArray, IsUnit, PrimUnlifted, Safety(..))
import Champ.Internal.Array qualified as Array
import Champ.Internal.Collision qualified as Collision
import Champ.Internal.Storage (ArrayOf, Storage (..), StrictStorage (..), Soloist(..))
import Champ.Internal.Util (ptrEq)
import Control.Monad qualified
import Data.Bits hiding (shift)
import Data.Foldable qualified as Foldable
import Data.Function ((&))
import Data.Hashable (Hashable)
import Data.Hashable qualified as Hashable
import Data.List qualified as List
import Data.Primitive (Prim)
import Data.Primitive.Contiguous (Element)
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Word (Word64, Word32)
import GHC.Exts qualified as Exts
import GHC.IsList (IsList (..))
import Numeric (showBin)
import Prelude hiding (null, lookup, filter)
import Data.Coerce (Coercible)
import Data.Coerce qualified
import Unsafe.Coerce (unsafeCoerce)
import Data.Type.Coercion (Coercion(Coercion))
import GHC.Exts (Any)
import Control.DeepSeq

-- * Setup
--
-- $setup
--
-- It is recommended to import the types from the main `Champ` module
-- and import `Champ.HashMap` and `Champ.HashSet` qualified,
-- since they contain many functions that would conflict
-- with hashmap-specific versions of same-named functions in the Prelude.
-- 
-- >>> import Champ
-- >>> import Champ.HashMap qualified
-- >>> import Champ.HashSet qualified
--
-- To reduce boilerplate, you can use `OverloadedLists`
-- to build hashmaps directly from a list containing key-value pairs (and hashsets from a list of elements).
-- This will automatically call `Champ.HashMap.toList`.
-- >>> :set -XOverloadedLists
--
-- Some examples also use the following extensions:
-- >>> :set -XOverloadedStrings
--
-- The following types from other libraries
-- are used in some examples:
-- >>> import Data.Text.Short (ShortText) -- from the @text-short@ library

#define BIT_PARTITION_SIZE 5
#define HASH_CODE_LENGTH (1 `unsafeShiftL` BIT_PARTITION_SIZE)
#define BIT_PARTITION_MASK (HASH_CODE_LENGTH - 1)

-- * Boxed keys: 

-- | A HashMap with strict boxed keys and lazy boxed values
-- 
-- Has the same semantics as @unordered-containers@ `Data.HashMap.Lazy`
type HashMapBL = HashMap Boxed Lazy

-- | A HashMap with strict boxed keys and strict boxed values
-- 
-- Has the same semantics as @unordered-containers@ `Data.HashMap.Strict`
type HashMapBB = HashMap Boxed (Strict Boxed)

-- | A HashMap with strict boxed keys and unboxed values
-- 
-- This uses significantly less memory than the boxed versions
-- but requires values to implement `Prim`.
type HashMapBU = HashMap Boxed (Strict Unboxed)

-- | A HashMap with strict boxed keys and unboxed values
-- 
-- This skips one level of boxing
-- but requires values to implement `PrimUnlifted`.
type HashMapBUl = HashMap Boxed (Strict Unlifted)

-- * Unboxed keys:

-- | A HashMap with unboxed keys and lazy boxed values
--
-- This uses significantly less memory for the keys 
-- than the boxed versions
-- but requires keys to implement `Prim`.
type HashMapUL = HashMap Unboxed Lazy

-- | A HashMap with unboxed keys and strict boxed values
--
-- This uses significantly less memory for the keys 
-- than the boxed versions
-- but requires keys to implement `Prim`.
type HashMapUB = HashMap Unboxed (Strict Boxed)

-- | A HashMap with unboxed keys and unboxed values
--
-- This uses significantly less memory for the keys and values
-- than the boxed versions
-- but requires the keys and values to implement `Prim`.
type HashMapUU = HashMap Unboxed (Strict Unboxed)

-- | A HashMap with unboxed keys and unboxed values
--
-- This uses significantly less memory for the keys 
-- than the boxed versions
-- but requires keys to implement `Prim`.
-- 
-- Also, one level of boxing is skipped for the values,
-- but requires values to implement `PrimUnlifted`.
type HashMapUUl = HashMap Unboxed (Strict Unlifted)

-- * Unlifted keys:

-- | A HashMap with unlifed keys and lazy boxed values
-- 
-- This skips one level of boxing for the keys,
-- but requires keys to implement `PrimUnlifted`.
type HashMapUlL = HashMap Unlifted Lazy

-- | A HashMap with unlifed keys and strict boxed values
-- 
-- This skips one level of boxing for the keys,
-- but requires keys to implement `PrimUnlifted`.
type HashMapUlB = HashMap Unlifted (Strict Boxed)

-- | A HashMap with unlifed keys and unboxed values
-- 
-- This skips one level of boxing for the keys,
-- but requires keys to implement `PrimUnlifted`.
--
-- Also, this uses significantly less memory for the values
-- than the boxed versions
-- but requires values to implement `Prim`.
type HashMapUlU = HashMap Unlifted (Strict Unboxed)

-- | A HashMap with unlifted keys and unlifed values
--
-- This skips one level of boxing for both the keys and values,
-- but requires the keys and values to implement `PrimUnlifted`.
type HashMapUlUl = HashMap Unlifted (Strict Unlifted)

pattern EmptyMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  HashMap keyStorage valStorage k v
{-# INLINE EmptyMap #-}
pattern EmptyMap <- (matchMap -> (# (# #) | | #))
  where
    EmptyMap = emptyMap

pattern SingletonMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  Hash -> k -> v -> HashMap keyStorage valStorage k v
{-# INLINE SingletonMap #-}
pattern SingletonMap h k v <- (matchMap -> (# | (# h, k, v #) | #))
  where
    SingletonMap = singletonMap

pattern ManyMap ::
  forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
  (MapRepr keyStorage valStorage k v) =>
  Word -> MapNode keyStorage valStorage k v -> HashMap keyStorage valStorage k v
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
-- Rather than implementing this as a plain datatype,
-- it is implemented as a typeclass with an associated data family called `HashMap`.
--
-- This convinces GHC that all intermediate constructors are actually not polymorphic at all,
-- and therefore can all be unboxed.
--
-- Conceptually, a `HashMap keys vals k v` you can think of it as:
--
-- @
-- data HashMap (keys :: StrictStorage) (vals :: Storage) k v
--  = EmptyMap 
--  | SingletonMap !k v 
--  | ManyMap {size :: !Word, contents :: (MapNode k v)}
--
-- data MapNode keys vals k v
--    = CollisionNode !(ArrayOf (Strict keys)) !(ArrayOf vals)
--    | CompactNode !Bitmap !Bitmap !(ArrayOf (Strict keys)) !(ArrayOf vals) !(StrictSmallArray (MapNode keys vals k v))
-- @
--
-- with the following tricks:
--
-- - We only store a single 64-bit bitmap (taking up one word) rather than two separate 32-bit bitmaps,
--  and use its lower/higher 32 bits using masking and shifting instead, saving one word per map node.
-- - There is no special `CollisionNode` variant.
--  Instead, we disambiguate using the special bitmap value '0'
--  which can never occur in a valid CompactNode as they are never empty.
-- - As mentioned above, we make sure that GHC unpacks the intermediate array boxes,
--  so we store the `GHC.Exts.SmallArray#` resp `GHC.Exts.ByteArray#` pointers directly.
--  This results in one word saved for the outer map and three more words saved per map node.
-- - We make sure that internal map nodes are stored as `Data.Kind.UnliftedType`
--  inside their array, as we're always strict in the tree-spine of the CHAMP map.
--  This means GHC will skip any thunk-forcing code whenever reading/recursing
-- - We actually don't have `EmptyMap` and `SingletonMap` as separate variants of the outer map type.
--   They are distinguishable by reading the size field. 
--
--      - In the case of `EmptyMap`, only the size is filled in (`0`). We mark it as NOINLINE so all empty maps of a particular type share a single allocation
--      - In the case of `SingletonMap`: 
--
--          - the hash of the single key is stored in place of the `Bitmap`.
--          - the single key is stored in place of the keys array
--          - the single value are stored in place of the values array. Iff the map is lazy in its values, it is wrapped in an extra `Solo` (c.f. `Soloist`).
--
--     Doing this simplifies some code, and allows us to support UNPACKing maps (they cost 5 inline words).
--
-- So in actuality, the memory representation is:
--
-- @
-- data MapNode (keys :: StrictStorage) (vals :: Storage) k v = MapNode 
--   { bitmap :: !Bitmap
--   , keys :: !(ArrayOf (Strict keys) k)
--   , vals :: !(ArrayOf vals v)
--   , children :: !(StrictSmallArray (MapNode keys vals k v))
--   }
--
-- data HashMap (keys :: StrictStorage) (vals :: Storage) k v = HashMap
--   { size :: !Word,
--   , bitmap :: !Bitmap,
--   , keyOrKeys :: !Any,
--   , valOrVals :: !Any,
--   , children :: !(StrictSmallArray (MapNode keys vals k v))
--   }
-- @
class (Array (ArrayOf (Strict keyStorage)), Array (ArrayOf (valStorage)), Element (ArrayOf (Strict keyStorage)) k, Element (ArrayOf valStorage) v) => MapRepr (keyStorage :: StrictStorage) (valStorage :: Storage) k v where
  -- | A CHAMP-based HashMap.
  -- 
  -- It is recommended to:
  --
  -- - Use one of [the concrete types]("Champ.HashMap#concreteTypesComparison") in your code. This will allow GHC to create an optimized implementation.
  -- - If not possible or youŕe working on highly generic code, use this generic t`HashMap` type, and add @INLINABLE@ or @SPECIALIZE@ pragmas
  --   to still give GHC the best opportunity to generate efficient code.
  data HashMap keyStorage valStorage k v


  -- | Internal data family representing any map node other than the root of the map.
  --
  -- Only used internally, never as part of the external interface.
  data MapNode keyStorage valStorage k v

  -- Constructs a `MapNode` from an unboxed tuple of its fields
  packNode :: (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, StrictSmallArray (MapNode keyStorage valStorage k v) #) -> MapNode keyStorage valStorage k v
  -- Destructs a `MapNode` into an unboxed tuple of its fields
  unpackNode :: MapNode keyStorage valStorage k v -> (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, StrictSmallArray (MapNode keyStorage valStorage k v) #)
  -- Constructs a `HashMap` with more than one element from its size and root MapNode
  manyMap :: Word -> MapNode keyStorage valStorage k v -> HashMap keyStorage valStorage k v
  -- Constructs an empty `HashMap`
  emptyMap :: HashMap keyStorage valStorage k v
  -- Constructs a `HashMap` with exactly one key-value pair
  singletonMap :: Hash -> k -> v -> HashMap keyStorage valStorage k v
  -- Destructs a HashMap into either an empty hashmap, a singleton hashmap or a many-element hashmap
  matchMap :: HashMap keyStorage valStorage k v -> (# (# #) | (# Hash, k, v #) | (# Word, MapNode keyStorage valStorage k v #) #)

#define MAP_NODE_NAME(name) MapNode/**/_/**/name

#define MAP_NODE_FIELDS(keystorage, valstorage) \
     {-# UNPACK #-} !Bitmap \
     {-# UNPACK #-} !((ArrayOf (Strict (keystorage))) k) \
     {-# UNPACK #-} !((ArrayOf (valstorage)) v) \
     {-# UNPACK #-} !(StrictSmallArray (MapNode (keystorage) (valstorage) k v))

#define map_repr_instance(name, keystorage, valstorage, constraints)                                                                                  \
instance (constraints, Soloist (valstorage)) => MapRepr (keystorage) (valstorage) k v where                                                           \
{ {-# INLINE unpackNode #-}                                                                                                                           \
; unpackNode (MAP_NODE_NAME(name) b keys vals children) = (# b, keys, vals, children #)                                                               \
; {-# INLINE packNode #-}                                                                                                                             \
; packNode (# b, keys, vals, children #) = (MAP_NODE_NAME(name) b keys vals children)                                                                 \
; {-# NOINLINE emptyMap #-}                                                                                                                           \
; emptyMap = Map_/**/name 0 0 (unsafeCoerce (mempty :: ArrayOf (Strict keystorage) k)) (unsafeCoerce (mempty :: ArrayOf (valstorage) v)) mempty       \
; {-# INLINE singletonMap #-}                                                                                                                         \
; singletonMap !h !k v = Map_/**/name 1 (unsafeCoerce $ h) (unsafeCoerce k) (unsafeCoerce (solo @(valstorage) v)) mempty                              \
; {-# INLINE manyMap #-}                                                                                                                              \
; manyMap mapsize (MAP_NODE_NAME(name) b keys vals children) = Map_/**/name mapsize b (unsafeCoerce keys) (unsafeCoerce vals) children                \
; {-# INLINE matchMap #-}                                                                                                                             \
; matchMap = \case {                                                                                                                                  \
; Map_/**/name 0 _ _ _ _ -> (# (# #) | | #)                                                                                                           \
; Map_/**/name 1 (unsafeCoerce -> h) (unsafeCoerce -> k) (unsafeCoerce -> soloV) _ -> (#  | (# h, k, unSolo @(valstorage) soloV #) | #)               \
; Map_/**/name mapsize b keys vals children -> (# | | (# mapsize, MAP_NODE_NAME(name) b (unsafeCoerce keys) (unsafeCoerce vals) children #) #) }      \
; data MapNode (keystorage) (valstorage) k v = MAP_NODE_NAME(name) MAP_NODE_FIELDS(keystorage, valstorage)                                            \
; data HashMap (keystorage) (valstorage) k v                                                                                                          \
  = Map_/**/name !Word !Bitmap !Any !Any !(StrictSmallArray (MapNode (keystorage) (valstorage) k v))                                                  \
}

-- HashMap variants:
-- ** Boxed keys:
map_repr_instance(Boxed_Lazy, Boxed, Lazy, ())
map_repr_instance(Boxed_Boxed, Boxed, Strict Boxed, ())
map_repr_instance(Boxed_Unlifted, Boxed, Strict Unlifted, (PrimUnlifted v))
map_repr_instance(Boxed_Unboxed, Boxed, Strict Unboxed, (Prim v))

-- ** Unboxed keys:
map_repr_instance(Unboxed_Lazy, Unboxed, Lazy, (Prim k))
map_repr_instance(Unboxed_Boxed, Unboxed, Strict Boxed, (Prim k))
map_repr_instance(Unboxed_Unlifted, Unboxed, Strict Unlifted, (Prim k, PrimUnlifted v))
map_repr_instance(Unboxed_Unboxed, Unboxed, Strict Unboxed, (Prim k, Prim v))

-- * Unlifted keys:
map_repr_instance(Unlifted_Lazy, Unlifted, Lazy, (PrimUnlifted k))
map_repr_instance(Unlifted_Boxed, Unlifted, Strict Boxed, (PrimUnlifted k))
map_repr_instance(Unlifted_Unlifted, Unlifted, Strict Unlifted, (PrimUnlifted k, PrimUnlifted v))
map_repr_instance(Unlifted_Unboxed, Unlifted, Strict Unboxed, (PrimUnlifted k, Prim v))


-- HashSet internals:
map_repr_instance(Boxed_Unexistent, Boxed, Unexistent, (IsUnit v))
map_repr_instance(Unboxed_Unexistent, Unboxed, Unexistent, (Prim k, IsUnit v))

-- | \O(1)\ Return `True` if the map is empty, `False` otherwise
null :: (MapRepr keys vals k v) => HashMap keys vals k v -> Bool
{-# INLINE null #-}
null EmptyMap = True
null _ = False

-- | \O(1)\ Return the number of key-value pairs in this map.
--
-- Since the map actually keeps track of the size while elements are inserted,
-- this function runs in constant-time.
size :: (MapRepr keys vals k v) => HashMap keys vals k v -> Int
{-# INLINE size #-}
size EmptyMap = 0
size (SingletonMap _h _k _v) = 1
size (ManyMap s _) = fromIntegral s

-- | \O(1)\ Construct the empty map
empty :: (MapRepr keys vals k v) => HashMap keys vals k v
{-# INLINE empty #-}
empty = EmptyMap

-- | \O(1)\ Construct a one-element map
singleton :: (Hashable k, MapRepr keys vals k v) => k -> v -> HashMap keys vals k v
{-# INLINE singleton #-}
singleton !k v = singletonKnownHash (hash k) k v

singletonKnownHash :: (MapRepr keys vals k v) => Hash -> k -> v -> HashMap keys vals k v
singletonKnownHash !h !k v = SingletonMap h k v

data Location = Inline | InChild | Nowhere

bitposLocation :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> Location
{-# INLINE bitposLocation #-}
bitposLocation node@(MapNode bitmap _ _ _) bitpos
  | bitmap .&. bitpos /= 0 = Inline
  | (childrenBitmap node) .&. bitpos /= 0 = InChild
  | otherwise = Nowhere

-- \(O(n \log32 n)\) Construct a map with the supplied key-value mappings.
-- 
-- If the list contains duplicate keys, later mappings take precedence.
--
-- NOTE: Since there is no unsafeInsert yet,
-- the current implementation is slower than necessary.
fromList :: (Hashable k, MapRepr keys vals k v) => [(k, v)] -> HashMap keys vals k v
{-# INLINE fromList #-}
fromList = Foldable.foldl' (\m (k, v) -> unsafeInsert k v m) empty

-- | \(O(n \log32 n)\) Construct a map from a list of elements.  Uses
-- the provided function @f@ to merge duplicate entries with
-- @(f newVal oldVal)@.
fromListWith :: (Eq k, Hashable k, MapRepr keys vals k v) => (v -> v -> v) -> [(k, v)] -> HashMap keys vals k v
{-# INLINE fromListWith #-}
fromListWith f = List.foldl' (\m (k, v) -> unsafeInsertWith f k v m) empty

-- | \(O(n \log32 n)\) Construct a map from a list of elements.  Uses
-- the provided function @f@ to merge duplicate entries with
-- @(f key newVal oldVal)@.
fromListWithKey :: (Eq k, Hashable k, MapRepr keys vals k v) => (k -> v -> v -> v) -> [(k, v)] -> HashMap keys vals k v
{-# INLINE fromListWithKey #-}
fromListWithKey f = List.foldl' (\m (k, v) -> unsafeInsertWithKey f k v m) empty

-- | \(O(n)\) Return a list of this map's elements (key-value pairs).
--
-- The resulting list is produced lazily and may participate in list fusion.
-- The order of its elements is unspecified.
toList :: (MapRepr keys vals k v) => HashMap keys vals k v -> [(k, v)]
{-# INLINE toList #-}
toList hashmap = Exts.build (\fusedCons fusedNil -> foldrWithKey (\k v xs -> (k, v) `fusedCons` xs) fusedNil hashmap)

-- | \(O(\log32 n)\) Adjust the value tied to a given key in this map only
-- if it is present. Otherwise, leave the map alone.
--
-- The current implementation is very simple
-- but is not super performant as it will traverse the map twice.
adjust :: (Hashable k, MapRepr keys vals k v) => (v -> v) -> k -> HashMap keys vals k v -> HashMap keys vals k v
adjust f k m = 
  lookupKVKnownHash# absent present h k m
  where
    h = hash k
    absent (# #) = m
    present k' v = 
      let v' = f v 
      in insert' Safe h k' v' m 

-- | \(O(\log32 n)\)  The expression @('alter' f k map)@ alters the value @x@ at @k@, or
-- absence thereof.
--
-- 'alter' can be used to insert, delete, or update a value in a map.
--
-- If the function returns Nothing, the key is removed from the map.
-- If the function returns Just v', the key is inserted or replaced with the new value.
--
-- The current implementation is very simple,
-- but is not super performant.
alter :: (Hashable k, MapRepr keys vals k v) => (Maybe v -> Maybe v) -> k -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINABLE alter #-}
alter f k m = 
  let h = hash k
  in
  case lookupKVKnownHash h k m of
  Nothing -> 
    case f Nothing of
      Nothing -> m
      Just v -> insert' Safe h k v m
  Just (k', v) -> 
    case f (Just v) of
      Nothing -> delete' Safe h k' m
      Just v' -> if ptrEq v v' then m else insert' Safe h k' v' m

-- | \(O(\log32 n)\)  The expression @('update' f k map)@ updates the value @x@ at @k@
-- (if it is in the map). If @(f x)@ is 'Nothing', the element is deleted.
-- If it is @('Just' y)@, the key @k@ is bound to the new value @y@.
update :: (Eq k, Hashable k, MapRepr keys vals k v) => (v -> Maybe v) -> k -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINABLE update #-}
update f = alter (>>= f)

-- | \(O(\log32 n)\)  The expression @('alterF' f k map)@ alters the value @x@ at
-- @k@, or absence thereof.
--
--  'alterF' can be used to insert, delete, or update a value in a map.
--
-- 'alterF' is a flipped version of the 'at' combinator from
-- <https://hackage.haskell.org/package/lens/docs/Control-Lens-At.html#v:at Control.Lens.At>.
--
--
-- The current implementation does not (yet) have a set of rewrite rules in place
-- that optimize particular common kinds of `f`. 
alterF :: (MapRepr keys vals a1 a2, Hashable a1, Functor f) => (Maybe a2 -> f (Maybe a2)) -> a1 -> HashMap keys vals a1 a2 -> f (HashMap keys vals a1 a2)
{-# INLINE alterF #-}
alterF f = \ !k !m ->
  let
    !h = hash k
  in
    case lookupKVKnownHash h k m of
      Nothing -> do
        mv <- (f Nothing)
        pure $ case mv of
          Nothing -> m
          Just v -> (insert' Safe h k v m)
      Just (k', v) -> do 
        mv <- (f (Just v))
        pure $ case mv of
          Nothing -> delete' Safe h k' m
          Just v' -> insert' Safe h k' v' m

-- | \(O(n \log32 n)\).
-- @'mapKeys' f s@ is the map obtained by applying @f@ to each key of @s@.
--
-- The size of the result may be smaller if @f@ maps two or more distinct
-- keys to the same new key. In this case there is no guarantee which of the
-- associated values is chosen for the conflicting key.
--
-- >>> Champ.HashMap.mapKeys (+ 1) (Champ.HashMap.fromList [(5,"a"), (3,"b")] :: HashMapUUl Int ShortText)
-- Champ.HashMap.fromList [(4,"b"),(6,"a")]
-- >>> Champ.HashMap.mapKeys (\ _ -> 1.0) (Champ.HashMap.fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")] :: HashMapUUl Int ShortText)
-- Champ.HashMap.fromList [(1.0,"c")]
-- >>> Champ.HashMap.mapKeys (\ _ -> 3 :: Int) (Champ.HashMap.fromList [(1,"b"), (2,"a"), (3,"d"), (4,"c")] :: HashMapUUl Int ShortText)
-- Champ.HashMap.fromList [(3,"c")]
mapKeys :: (Eq k2, Hashable k2, MapRepr keys vals k1 v, MapRepr keys vals k2 v) => (k1 -> k2) -> HashMap keys vals k1 v -> HashMap keys vals k2 v
{-# INLINE mapKeys #-}
mapKeys = mapKeys'

mapKeys' :: (Eq k2, Hashable k2, MapRepr keys vals k1 v, MapRepr keys2 vals k2 v) => (k1 -> k2) -> HashMap keys vals k1 v -> HashMap keys2 vals k2 v
{-# INLINE mapKeys' #-}
mapKeys' f = Champ.Internal.fromList . Champ.Internal.foldrWithKey (\k x xs -> (f k, x) : xs) []


-- | \(O(\log32 n)\) Associate the specified value with the specified
-- key in this map.  If this map previously contained a mapping for
-- the key, the old value is replaced.
insert :: (Hashable k, MapRepr keys vals k v) => k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE insert #-}
insert k = insert' Safe (hash k) k

unsafeInsert :: (Hashable k, MapRepr keys vals k v) => k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unsafeInsert #-}
unsafeInsert k = insert' Unsafe (hash k) k

insert' :: (Hashable k, MapRepr keys vals k v) => Safety -> Hash -> k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE insert' #-}
insert' safety h !k v !m = case matchMap m of
  (# (# #) | | #) -> singleton k v
  (# | (# h', k', v' #) | #) ->
    if h == h' && k == k'
      then singletonKnownHash h k' v
      else
          Contiguous.empty
            & MapNode (maskToBitpos (hashToMask 0 (hash k'))) (Array.singleton safety k') (Array.singleton safety v')
            & \node -> insertInNode safety h k v 0 node 
              (\size node' -> ManyMap (1 + Exts.W# size) node')
  (# | | (# size, node0 #) #) ->
    insertInNode safety h k v 0 node0 (\didIGrow node' -> ManyMap (size + Exts.W# didIGrow) node')

insertInNode :: (MapRepr keys vals k v, Hashable k) =>
                Safety
                -> Hash
                -> k
                -> v
                -> Word
                -> MapNode keys vals k v
                -> (Exts.Word# -> MapNode keys vals k v -> r)
                -> r
{-# INLINEABLE insertInNode #-}
insertInNode safety !h !k v !shift node cont = case node of
  !node@(CollisionNode _ _) -> cont 1## (insertCollision safety k v node)
  !node@(CompactNode !bitmap !keys !vals !children) ->
    let !bitpos = maskToBitpos $ hashToMask shift h
    in case bitposLocation node bitpos of
        Inline ->
          -- exists inline; potentially turn inline to subnode with two keys
          insertMergeWithInline safety bitpos k v h shift node cont
        InChild ->
          -- Exists in child, insert in there and make sure this node contains the updated child
          let child = indexChild node bitpos
          in insertInNode safety h k v (nextShift shift) child (\didIGrow child' -> 
                  cont didIGrow (CompactNode bitmap keys vals (Array.replaceAt safety children (childrenIndex node bitpos) child')))
        Nowhere ->
          -- Doesn't exist yet, we can insert inline
          cont 1## (insertNewInline safety bitpos k v node)

-- {-# SPECIALIZE insert :: Hashable k => k -> v -> HashMapBL k v -> HashMapBL k v #-}
-- {-# SPECIALIZE insert :: Hashable k => k -> v -> HashMapBB k v -> HashMapBB k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim v) => k -> v -> HashMapBU k v -> HashMapBU k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k) => k -> v -> HashMapUL k v -> HashMapUL k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k) => k -> v -> HashMapUB k v -> HashMapUB k v #-}
-- {-# SPECIALIZE insert :: (Hashable k, Prim k, Prim v) => k -> v -> HashMapUU k v -> HashMapUU k v #-}
-- {-# SPECIALIZE insert :: Int -> Int -> HashMapUU Int Int -> HashMapUU Int Int #-}

-- Collisions are appended at the end
-- Note that we cannot insert them in sorted order
-- (which would theoretically allow a binary search on lookup)
-- because we don't have an `Ord` instance.
{-# INLINE insertCollision #-}
insertCollision :: (MapRepr keys vals k v) => Safety -> k -> v -> MapNode keys vals k v -> MapNode keys vals k v
insertCollision safety k v (MapNode _ keys vals _) =
  let idx = Contiguous.size keys
      keys' = Array.insertAt safety keys idx k
      vals' = Array.insertAt safety vals idx v
   in CollisionNode keys' vals'

{-# INLINE insertNewInline #-}
insertNewInline :: (MapRepr keys vals k v) => Safety -> Bitmap -> k -> v -> MapNode keys vals k v -> MapNode keys vals k v
insertNewInline safety bitpos k v node@(MapNode bitmap keys vals children) =
  let bitmap' = bitmap .|. bitpos
      idx = dataIndex node bitpos
      keys' = Array.insertAt safety keys idx k
      vals' = Array.insertAt safety vals idx v
   in MapNode bitmap' keys' vals' children

{-# INLINE insertMergeWithInline #-}
insertMergeWithInline :: (Hashable k, MapRepr keys vals k v) => Safety -> Bitmap -> k -> v -> Hash -> Word -> MapNode keys vals k v -> (Exts.Word# -> MapNode keys vals k v -> r) -> r
insertMergeWithInline safety bitpos k v h shift node@(MapNode bitmap keys vals children) cont =
  let idx = dataIndex node bitpos
      existingKey = indexKey node bitpos
      (# existingVal #) = indexVal# node bitpos
   in if existingKey == k then cont 1## (CompactNode bitmap keys (Array.replaceAt safety vals idx v) children )
      else
          let newIdx = childrenIndex node bitpos
              -- SAFETY: The forcing of `child`/`pairNode` here is extremely important
              -- if we're in `Unsafe` mode about to delete `existingVal` from the values array;
              -- then when we are too lazy in the _fetching_ of `existingVal`,
              -- we'd fetch off-by-one!
              !child = existingVal `seq` pairNode safety existingKey existingVal (hash existingKey) k v h (nextShift shift)
              keys' = Array.deleteAt safety keys idx
              vals' = Array.deleteAt safety vals idx
              !children' = Array.insertAt safety children newIdx child
              bitmap' = bitmap .^. bitpos .|. (bitpos `unsafeShiftL` HASH_CODE_LENGTH)
            in cont 1## (CompactNode bitmap' keys' vals' children')

{-# INLINE pairNode #-}
pairNode :: (MapRepr keys vals k v) => Safety -> k -> v -> Hash -> k -> v -> Hash -> Word -> MapNode keys vals k v
pairNode safety k1 v1 h1 k2 v2 h2 shift
  | shift >= HASH_CODE_LENGTH = CollisionNode (Contiguous.doubleton k1 k2) (Contiguous.doubleton v1 v2)
  | otherwise =
      let mask1 = hashToMask shift h1
          mask2 = hashToMask shift h2
       in if mask1 /= mask2
            then
              -- Both fit on this level
              mergeCompactInline safety k1 v1 h1 k2 v2 h2 shift
            else
              -- Both fit on the _next_ level
              let child = mergeCompactInline safety k1 v1 h1 k2 v2 h2 (nextShift shift)
                  bitmap = maskToBitpos mask1 `unsafeShiftL` HASH_CODE_LENGTH
               in CompactNode bitmap Contiguous.empty Contiguous.empty (Array.singleton safety child)

mergeCompactInline :: MapRepr keyStorage valStorage k v =>
                            Safety
                            -> k
                            -> v
                            -> Hash
                            -> k
                            -> v
                            -> Hash
                            -> Word
                            -> MapNode keyStorage valStorage k v
{-# INLINE mergeCompactInline #-}
mergeCompactInline safety k1 v1 h1 k2 v2 h2 shift =
  let !mask0@(Mask (Exts.W# i1)) = hashToMask shift h1
      !mask1@(Mask (Exts.W# i2)) = hashToMask shift h2
      !bitmap = maskToBitpos mask0 .|. maskToBitpos mask1
      !c = Exts.I# (i1 `Exts.ltWord#` i2)
      keys = Array.doubletonBranchless safety c k1 k2
      vals = Array.doubletonBranchless safety c v1 v2
   in CompactNode bitmap keys vals Contiguous.empty

-- | \(O(\log32 n)\) Associate the value with the key in this map.  If
-- this map previously contained a mapping for the key, the old value
-- is replaced by the result of applying the given function to the new
-- and old value.  Example:
--
-- > insertWith f k v map
-- >   where f new old = new + old
--
-- The current implementation is very simple
-- but not very performant since it walks over the map twice
insertWith :: (Hashable k, MapRepr keys vals k v) => (v -> v -> v) -> k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE insertWith #-}
insertWith f k v m = 
  let h = hash k
  in
  lookupKVKnownHash# absent present h k m
  where
    h = hash k
    absent (# #) = insert' Safe h k v m
    present k' v' = insert' Safe h k' (f v v') m

unsafeInsertWith :: (Hashable k, MapRepr keys vals k v) => (v -> v -> v) -> k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unsafeInsertWith #-}
unsafeInsertWith f = unsafeInsertWithKey# (\_ a b -> (# f a b #))

unsafeInsertWithKey :: (Hashable k, MapRepr keys vals k v) => (k -> v -> v -> v) -> k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unsafeInsertWithKey #-}
unsafeInsertWithKey f = unsafeInsertWithKey# (\k' a b -> (# f k' a b #))

unsafeInsertWithKey# :: (Hashable k, MapRepr keys vals k v) => (k -> v -> v -> (# v #)) -> k -> v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unsafeInsertWithKey# #-}
unsafeInsertWithKey# f k v m = 
  lookupKVKnownHash# absent present h k m
    where
      h = hash k
      absent (# #) = insert' Unsafe h k v m
      present k2 v2 = 
        case f k2 v v2 of
          (# v3 #) -> insert' Unsafe h k2 v3 m

delete :: (Hashable k, MapRepr keys vals k v) => k -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE delete #-}
delete k = delete' Safe (hash k) k

unsafeDelete :: (Hashable k, MapRepr keys vals k v) => k -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unsafeDelete #-}
unsafeDelete k = delete' Unsafe (hash k) k

delete' :: (Hashable k, MapRepr keys vals k v) => Safety -> Hash -> k -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE delete' #-}
delete' safety h !k !m = case matchMap m of
  (# (# #) | | #) -> EmptyMap
  (# | (# h', k', v #) | #) 
    | k == k' -> EmptyMap
    | otherwise -> SingletonMap h' k' v
  (# | | (# size, node0 #) #) ->
    case deleteFromNode safety h k 0 node0 of
      (# (# k', v #) | #) -> singleton k' v
      (# | (# didIShrink, node #) #) -> ManyMap (size - Exts.W# didIShrink) node

deleteFromNode :: (MapRepr keyStorage valStorage a1 a2, Eq a1) =>
                        Safety
                        -> Hash
                        -> a1
                        -> Word
                        -> MapNode keyStorage valStorage a1 a2
                        -> (# (# a1, a2 #) |
                              (# Exts.Word#, MapNode keyStorage valStorage a1 a2 #) #)
deleteFromNode safety !h !k !shift = \case
  node@(CollisionNode keys vals) -> case findCollisionIndex# k keys of
    (# (# #) | #) -> 
      (# | (# 0##, node #) #)
    (# | idx #) | Contiguous.size keys == 2 ->
      case idx of
        0 ->
          let (# existingVal #) = Contiguous.index# vals 1
              existingKey = Contiguous.index keys 1
          in (# (# existingKey, existingVal #) | #)
        1 ->
          let (# existingVal #) = Contiguous.index# vals 0
              existingKey = Contiguous.index keys 0
          in (# (# existingKey, existingVal #) | #)
        _ -> error "Unreachable"
    (# | idx #) ->
      let keys' = Array.deleteAt safety keys idx
          vals' = Array.deleteAt safety vals idx
          node' = CollisionNode keys' vals'
      in
        (# | (# 1##, node' #) #)
  node@(CompactNode !bitmap !keys !vals !children) ->
    let !bitpos = maskToBitpos (hashToMask shift h)
    in case bitposLocation node bitpos of
      Nowhere ->
        -- Nothing to delete, return self
        (# | (# 0##, node #) #)
      Inline ->
        -- Delete locally
        let existingKey = indexKey node bitpos
        in if 
             | existingKey /= k -> 
              -- Deleting something with the same hash
              -- but actually a different key; nothing to do
              (# |  (# 0## , node #) #)
             | existingKey == k && (Contiguous.size keys == 2) ->
              -- Collapse this array of two elements into a singleton node;
              -- and bubble it up to be merged inline one layer up
              case (dataIndex node bitpos) of
                0 ->
                  let (# existingVal #) = Contiguous.index# vals 1
                      existingKey = Contiguous.index keys 1
                  in (# (# existingKey, existingVal #) | #)
                1 ->
                  let (# existingVal #) = Contiguous.index# vals 0
                      existingKey = Contiguous.index keys 0
                  in (# (# existingKey, existingVal #) | #)
                _ -> error "Unreachable"
             | otherwise ->
              let idx = dataIndex node bitpos
                  keys' = Array.deleteAt safety keys idx
                  vals' = Array.deleteAt safety vals idx
                  bitmap' = bitmap .^. bitpos
              in
                (# | (# 1##, CompactNode bitmap' keys' vals' children #) #)
      InChild ->
        -- Recurse
        let child = indexChild node bitpos
            childIndex = childrenIndex node bitpos
            bitmap' = bitmap .^. (bitpos `unsafeShiftL` HASH_CODE_LENGTH)
        in
          case deleteFromNode safety h k (nextShift shift) child of
            (# (# k', v' #) | #) ->
              -- Child became too small, replace with inline
              let 
                arr = Array.deleteAt safety children childIndex
                node = CompactNode bitmap' keys vals arr
                node' = insertNewInline safety bitpos k' v' node
              in
                (# | (# 1##, node' #) #)
            (# | (# didIGrow, child' #) #) ->
              let node' = CompactNode bitmap' keys vals (Array.replaceAt safety children childIndex child')
              in (# | (# didIGrow, node' #) #)

-- | Look up the value for a given key.
--
-- Returns `Nothing` if the key does not exist in the map.
lookup :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> HashMap keys vals k v -> Maybe v
{-# INLINE lookup #-}
lookup k m = case lookupKV# k m of
  (# (# #) | #) -> Nothing
  (# | (# _k, v #) #) -> Just v

-- | Version of lookup that also returns the found key.
--
-- This is only useful if the 'memory identity' of the key is important.
-- It usually isn't, but sometimes it is: For example, some equality checks can short-cirquit using pointer-equality
-- and by fetching a key that is known to be the same pointer, you can speed up equality checks in the future,
-- as well as deduplicate the memory you're holding onto.
lookupKV :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> HashMap keys vals k v -> Maybe (k, v)
{-# INLINE lookupKV #-}
lookupKV !k = lookupKVKnownHash (hash k) k

lookupKV# :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> HashMap keys vals k v -> (# (# #) | (# k, v #) #)
{-# INLINE lookupKV# #-}
lookupKV# !k = lookupKVKnownHash# absent present (hash k) k
  where
    absent :: (# #) -> (# (# #) | (# k, v #) #)
    absent (# #) = (# (# #) | #)
    present :: k -> v -> (# (# #) | (# k, v #) #)
    present k v = (# | (# k, v #) #)

lookupKVKnownHash :: (MapRepr keys vals a b, Hashable a) => Hash -> a -> HashMap keys vals a b -> Maybe (a, b)
{-# INLINE lookupKVKnownHash #-}
lookupKVKnownHash = lookupKVKnownHash# absent present
  where
    absent (# #) = Nothing
    present k v = Just (k, v)

lookupKVKnownHash# 
  :: forall rep (r :: Exts.TYPE rep) keys vals k v.
  (MapRepr keys vals k v, Eq k, Hashable k)
  => ((# #) -> r) -- Absent continuation
  -> (k -> v -> r) -- Present continuation
  -> Hash -> k -> HashMap keys vals k v -> r
{-# INLINE lookupKVKnownHash# #-}
lookupKVKnownHash# absent present h !k m = case matchMap m of
  (# (# #) | | #) -> absent (# #)
  (# | (# h', k', v #) | #) -> if h == h' && k == k' then present k' v else absent (# #)
  (# | | (# _size, node0 #) #) ->
    -- NOTE: Manually inlining the first iteration of lookup'
    -- (which will _never_ be in a collision node)
    -- results in a noticeable speedup for maps that have at most 32 elements.
    let !bitpos = maskToBitpos $ hashToMask 0 h
    in case bitposLocation node0 bitpos of
        Nowhere -> absent (# #)
        Inline ->
            let k' = indexKey node0 bitpos
                (# v #) = indexVal# node0 bitpos
            in if k == k' then present k' v else absent (# #)
        InChild -> lookup' (nextShift 0) (indexChild node0 bitpos)
            where
                {-# INLINEABLE lookup' #-}
                lookup' !_s !(CollisionNode keys vals) =
                  case findCollisionIndex# k keys of
                    (# (# #) | #) -> absent (# #)
                    (# | idx #) -> present (Contiguous.index keys idx) (Contiguous.index vals idx)
                lookup' !s !node@(CompactNode _bitmap _keys _vals _children) =
                    let !bitpos = maskToBitpos $ hashToMask s h
                    in case bitposLocation node bitpos of
                            Nowhere -> absent (# #)
                            Inline ->
                                let k' = indexKey node bitpos
                                    (# v #) = indexVal# node bitpos
                                in if k == k' then present k' v else absent (# #)
                            InChild -> lookup' (nextShift s) (indexChild node bitpos)

-- mylookup :: Int -> HashMapUU Int Int -> Maybe Int
-- mylookup = lookup

findCollisionIndex# :: (Eq a, Array arr, Element arr a) => a -> arr a -> (# (# #) | Int #)
{-# INLINE findCollisionIndex# #-}
findCollisionIndex# k keys = Array.findIndex# (\existingKey -> existingKey `ptrEq` k || existingKey == k) keys

indexKey :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> k
{-# INLINE indexKey #-}
indexKey (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexKey node@(CompactNode _bitmap keys _vals _children) bitpos =
    Contiguous.index keys (dataIndex node bitpos)

indexVal# :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> (# v #)
{-# INLINE indexVal# #-}
indexVal# (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexVal# node@(CompactNode _bitmap _keys vals _children) bitpos =
    Contiguous.index# vals (dataIndex node bitpos)

indexChild :: MapRepr keys vals k v => MapNode keys vals k v -> Bitmap -> MapNode keys vals k v
{-# INLINE indexChild #-}
indexChild (CollisionNode _keys _vals) _ = error "Should only be called on CompactNodes"
indexChild node@(CompactNode _bitmap _keys _vals children) bitpos =
    Contiguous.index children (childrenIndex node bitpos)

-- | \O(log n)\ Return `True` if the specified key is present in the map, `False` otherwise.
member :: (MapRepr keys vals k v, Eq k, Hashable k) => k -> HashMap keys vals k v -> Bool
{-# INLINE member #-}
member k m = case lookupKV# k m of 
  (# (# #) | #) -> True
  _ -> False

instance (MapRepr keys vals k v, Eq v, Eq k) => Eq (HashMap keys vals k v) where
  {-# INLINE (==) #-}
  a == b = 
    if a `ptrEq` b then True 
    else 
      case (a, b) of
        (EmptyMap, EmptyMap) -> True
        ((SingletonMap h k v), (SingletonMap h' k' v')) -> (h == h') && (k == k') && (v == v')
        ((ManyMap sz node), (ManyMap sz' node')) -> 
          sz == sz' && node == node'
        (_, _) -> False

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

myeq :: HashMapUU Int Int -> HashMapUU Int Int -> Bool
myeq a b = a == b

instance (Hashable k, Eq k, MapRepr keys vals k v) => IsList (HashMap keys vals k v) where
  type Item (HashMap keys vals k v) = (k, v)
  {-# INLINE toList #-}
  toList = Champ.Internal.toList
  {-# INLINE fromList #-}
  fromList = Champ.Internal.fromList

instance (NFData v, NFData k, (MapRepr keys vals k v), NFData (MapNode keys vals k v)) => NFData (HashMap keys vals k v) where
  {-# INLINE rnf #-}
  rnf EmptyMap = ()
  rnf (SingletonMap _h k v) = rnf k `seq` rnf v
  rnf (ManyMap _size node) = rnf node

instance {-# OVERLAPPABLE #-} (NFData k, NFData v, MapRepr keys vals k v) => NFData (MapNode keys vals k v) where
  {-# INLINE rnf #-}
  rnf (CollisionNode keys vals) = Contiguous.rnf keys `seq` Contiguous.rnf vals
  rnf (MapNode bitmap keys vals children) =
    rnf bitmap `seq` Contiguous.rnf keys `seq` Contiguous.rnf vals `seq` Contiguous.rnf children

instance {-# OVERLAPS #-} (NFData k, NFData v) => NFData (MapNode Unboxed (Strict Unboxed) k v) where
  {-# INLINE rnf #-}
  rnf !_ = ()

instance {-# OVERLAPS #-} (NFData k, NFData v) => NFData (MapNode Unboxed Unexistent k v) where
  {-# INLINE rnf #-}
  rnf !_ = ()

instance Functor (HashMapBL k) where
  {-# INLINE fmap #-}
  fmap = Champ.Internal.map

instance Functor (HashMapBB k) where
  {-# INLINE fmap #-}
  fmap = Champ.Internal.map

instance (Prim k) => Functor (HashMapUL k) where
  {-# INLINE fmap #-}
  fmap = Champ.Internal.map

instance (Prim k) => Functor (HashMapUB k) where
  {-# INLINE fmap #-}
  fmap = Champ.Internal.map

-- NOTE: Supporting Functor for MapXU is impossible
-- without being very tricksy

-- | Map a function over the values in a hashmap
--
-- O(n)
map :: (MapRepr keys vals k a, MapRepr keys vals k b) => (a -> b) -> HashMap keys vals k a -> HashMap keys vals k b
{-# INLINE map #-}
map = map'

-- | Map a function over the values in a hashmap,
-- allowing to switch the value storage type.
--
-- that is: you can switch between HashMapBL <-> HashMapBB <-> HashMapBU,
-- or switch between HashMapUL <-> HashMapUB <-> HashMapUU.
--
-- When using this function, you will need 
-- to specify the type of the output map.
--
-- O(n)
--
-- Example:
--
-- >>> mymap = Champ.HashMap.fromList [(1, 10), (2, 20)] :: HashMapUU Int Int
-- >>> Champ.HashMap.map' show mymap :: HashMapUB Int String 
-- Champ.HashMap.fromList [(1,"10"),(2,"20")]
map' :: (MapRepr keys as k a, MapRepr keys bs k b) => (a -> b) -> HashMap keys as k a -> HashMap keys bs k b
{-# INLINE map' #-}
map' !f = \case 
  EmptyMap -> EmptyMap
  (SingletonMap h k v) -> SingletonMap h k (f v)
  (ManyMap sz node) -> ManyMap sz (mapNode node)
    where
      mapNode (CollisionNode keys vals) = (CollisionNode keys (Contiguous.map f vals))
      mapNode (CompactNode bitmap keys vals children) =
        let
          vals' = Contiguous.map f vals
          children' = Contiguous.map mapNode children
        in
          CompactNode bitmap keys vals' children'

instance Traversable (HashMapBL k) where
  traverse = Champ.Internal.traverse

instance Traversable (HashMapBB k) where
  traverse = Champ.Internal.traverse

traverse :: (Applicative f, MapRepr keys vals k a, MapRepr keys vals k b) => (a -> f b) -> HashMap keys vals k a -> f (HashMap keys vals k b)
{-# INLINE traverse #-}
traverse f = traverseWithKey (const f)

traverse' :: (Applicative f, MapRepr keys as k a, MapRepr keys bs k b) => (a -> f b) -> HashMap keys as k a -> f (HashMap keys bs k b)
{-# INLINE traverse' #-}
traverse' f = traverseWithKey' (const f)

traverseWithKey :: (Applicative f, MapRepr keys vals k a, MapRepr keys vals k b) => (k -> a -> f b) -> HashMap keys vals k a -> f (HashMap keys vals k b)
{-# INLINE traverseWithKey #-}
traverseWithKey = traverseWithKey'

traverseWithKey' :: (Applicative f, MapRepr keys as k a, MapRepr keys bs k b) => (k -> a -> f b) -> HashMap keys as k a -> f (HashMap keys bs k b)
{-# INLINE traverseWithKey' #-}
traverseWithKey' !f = \case
  EmptyMap -> pure EmptyMap
  (SingletonMap h k v) -> do
    v' <- f k v
    pure (SingletonMap h k v')
  ManyMap sz node -> do
    node' <- traverseNodeWithKey f node
    pure (ManyMap sz node')
  where
    traverseNodeWithKey :: (Applicative f, MapRepr keys as k a, MapRepr keys bs k b) => (k -> a -> f b) -> MapNode keys as k a -> f (MapNode keys bs k b)
    {-# INLINE traverseNodeWithKey #-}
    traverseNodeWithKey f = \case
      CollisionNode keys vals -> do
        vals' <- traverseInlineKVs f keys vals
        pure (CollisionNode keys vals')
      CompactNode bitmap keys vals children -> do
        vals' <- traverseInlineKVs f keys vals
        children' <- Contiguous.traverse (traverseNodeWithKey f) children
        pure (CompactNode bitmap keys vals' children')
    traverseInlineKVs :: (Applicative f, MapRepr keys as k a, MapRepr keys bs k b) => (k -> a -> f b) -> ArrayOf (Strict keys) k -> ArrayOf as a -> f (ArrayOf bs b)
    {-# INLINE traverseInlineKVs #-}
    traverseInlineKVs f keys = Contiguous.itraverse (\i v -> let (# k #) = Contiguous.index# keys i in f k v)

-- | Map a function over the values in a hashmap while passing the key to the mapping function.
--
-- O(n)
mapWithKey :: (MapRepr keys vals k a, MapRepr keys vals k b) => (k -> a -> b) -> HashMap keys vals k a -> HashMap keys vals k b
{-# INLINE mapWithKey #-}
mapWithKey = mapWithKey'

-- | Map a function over the values in a hashmap while passing the key to the mapping function,
-- and allowing to switch the value storage type.
--
-- that is: you can switch between HashMapBL <-> HashMapBB <-> HashMapBU,
-- or switch between HashMapUL <-> HashMapUB <-> HashMapUU.
--
-- When using this function, you will need 
-- to specify the type of the output map.
--
-- O(n)
mapWithKey' :: (MapRepr keys as k a, MapRepr keys bs k b) => (k -> a -> b) -> HashMap keys as k a -> HashMap keys bs k b
{-# INLINE mapWithKey' #-}
mapWithKey' !f = \case 
  EmptyMap -> EmptyMap
  (SingletonMap h k v) -> SingletonMap h k (f k v)
  (ManyMap sz node) -> ManyMap sz (mapNodeWithKey node)
    where
      mapNodeWithKey (CollisionNode keys vals) = (CollisionNode keys (Contiguous.zipWith f keys vals))
      mapNodeWithKey (CompactNode bitmap keys vals children) =
        let
          vals' = Contiguous.zipWith f keys vals
          children' = Contiguous.map mapNodeWithKey children
        in
          CompactNode bitmap keys vals' children'

-- | Transform this map by applying a function to every key-value pair
-- and retaining only some of them.
--
-- O(n).
mapMaybeWithKey :: (Eq k, Hashable k, MapRepr keys vals k v, MapRepr keys vals k v') => (k -> v -> Maybe v') -> HashMap keys vals k v -> HashMap keys vals k v'
{-# INLINE mapMaybeWithKey #-}
mapMaybeWithKey = mapMaybeWithKey'

-- | \(O(n)\) Transform this map by applying a function to every key-value pair
-- and retaining only some of them.
--
-- Allows changing the value storage type
-- that is: you can switch between HashMapBL <-> HashMapBB <-> HashMapBU,
-- or switch between HashMapUL <-> HashMapUB <-> HashMapUU.
mapMaybeWithKey' :: (Eq k, Hashable k, MapRepr keys vals k v, MapRepr keys vals' k v') => (k -> v -> Maybe v') -> HashMap keys vals k v -> HashMap keys vals' k v'
{-# INLINE mapMaybeWithKey' #-}
mapMaybeWithKey' f = \case
  EmptyMap -> EmptyMap
  SingletonMap h k v -> case f k v of
    Nothing -> EmptyMap
    Just v' -> SingletonMap h k v'
  -- eta-expand because of https://gitlab.haskell.org/ghc/ghc/-/issues/23150
  ManyMap _ node -> Exts.inline $ extractInNode (\k v hint -> mapMaybeKeysVals f k v hint) node

mapMaybeKeysVals
  :: (MapRepr keys vals k v, MapRepr keys vals' k v')
  => (k -> v -> Maybe v')
  -> ArrayOf (Strict keys) k
  -> ArrayOf vals v
  -> Int
  -> (Word32, ArrayOf (Strict keys) k, ArrayOf vals' v')
{-# INLINE mapMaybeKeysVals #-}
mapMaybeKeysVals !f !keys !vals !hint =
  let
    !(mask, vals') = mapMaybeMask f keys vals hint
    !keys' = Array.filterUsingMask keys mask hint
  in (mask, keys', vals')

mapMaybeMask
  :: ( Contiguous.Contiguous arr1, Element arr1 a
     , Contiguous.Contiguous arr2, Element arr2 b
     , Contiguous.ContiguousU arr3, Element arr3 c
     , Bits bits
     )
  => (a -> b -> Maybe c) -> arr1 a -> arr2 b -> Int -> (bits, arr3 c)
{-# INLINE mapMaybeMask #-}
mapMaybeMask !f !keys !vals !hint = Contiguous.createT $ do
  -- Preallocate a larger array
  dst <- Contiguous.new (Contiguous.size vals + hint)
  let
    fillDestination ix (dstIx, mask) a b = case f a b of
      Nothing -> pure (dstIx, mask)
      Just c -> do
        Contiguous.write dst dstIx c
        pure (dstIx + 1, mask `setBit` ix)

  (finalIx, !mask) <- Contiguous.ifoldlZipWithM' fillDestination (0, zeroBits) keys vals
  !dst' <- Contiguous.resize dst finalIx
  pure (mask, dst')

-- | \(O(n)\) Transform this map by applying a function to every value
-- and retaining only some of them.
mapMaybe :: (Eq k, Hashable k, MapRepr keys vals k v, MapRepr keys vals k v') => (v -> Maybe v') -> HashMap keys vals k v -> HashMap keys vals k v'
{-# INLINE mapMaybe #-}
mapMaybe f = mapMaybeWithKey' (const f)

mapMaybe' :: (Eq k, Hashable k, MapRepr keys vals k v, MapRepr keys vals' k v') => (v -> Maybe v') -> HashMap keys vals k v -> HashMap keys vals' k v'
{-# INLINE mapMaybe' #-}
mapMaybe' f = mapMaybeWithKey' (const f)

-- | \(O(n)\) Filter this map by retaining only elements satisfying a
-- predicate.
filterWithKey :: forall k v keys vals. (Eq k, Hashable k, MapRepr keys vals k v) => (k -> v -> Bool) -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE filterWithKey #-}
filterWithKey !f = \case
  EmptyMap -> EmptyMap
  SingletonMap h k v -> if f k v
    then SingletonMap h k v
    else EmptyMap
  -- eta-expand because of https://gitlab.haskell.org/ghc/ghc/-/issues/23150
  ManyMap _ node -> Exts.inline $ extractInNode (\k v hint -> filterKeysVals f k v hint) node

-- | Generic implementation of filterWithKey and mapMaybeWithKey
extractInNode
  :: (MapRepr keys vals k v, MapRepr keys vals' k v', Hashable k)
  => (ArrayOf (Strict keys) k -> ArrayOf vals v -> Int -> (Word32, ArrayOf (Strict keys) k, ArrayOf vals' v'))
  -> MapNode keys vals k v
  -> HashMap keys vals' k v'
{-# INLINE extractInNode #-}
extractInNode !extractFrom !node = case node of
  CollisionNode keys vals ->
    let (keptKeyIxs :: Word32, keys', vals') = extractFrom keys vals 0
    in  ManyMap (fromIntegral (popCount keptKeyIxs)) (CollisionNode keys' vals')
  CompactNode b k v c ->
    let !result = Exts.inline $ extractCompactNode 0 b k v c
    in  result
    where
      extractCompactNode !shift !bitmap !keys !vals !children =
        -- Straightforward tactic:
        -- 1. Run the filter across the inline entries.
        -- 2. Go over children and run the filter across them.
        --  a. Remove any child nodes that become empty.
        --  b. Bring up any child nodes that can be inlined.
        -- 3. Depending on the final cardinality, emit smaller representations.
        let
          -- Go over inline values and see which survive.
          (booleanMask :: Word32, keys', vals') =
            extractFrom keys vals (Contiguous.size children)
          -- Notice that the relative 1 bit positions in the bitmap denote the
          -- locations of the entries within the arrays. Seeing as we already
          -- have a boolean bitmask on the entries, we can cross reference it
          -- with the bitmap and flip entries we removed, without having to
          -- rehash values.
          inlineBitmap = bitmap .&. ((1 `unsafeShiftL` 32) - 1)
          bitmapOnlyInline =
            maskBitmap booleanMask inlineBitmap

          -- Traverse over children, keeping track of data we need to determine what
          -- variant of the map node we emit.
          calcRemovedChild (!childSum, !childMask, !children', !keys', !vals', !dataBitmap) ix (CompactNode b k v c) =
            let !filteredChild = extractCompactNode (nextShift shift) b k v c
            in  case filteredChild of
                  EmptyMap ->
                    ( childSum
                    , childMask
                    , children'
                    , keys'
                    , vals'
                    , dataBitmap
                    )

                  SingletonMap h k v ->
                    let
                      bitpos = maskToBitpos (hashToMask shift h)
                      !dataBitmap' = dataBitmap .|. bitpos
                      idx = popCount $ dataBitmap' .&. (bitpos - 1)
                      !keys'' = Array.insertAt Unsafe keys' idx k
                      !vals'' = Array.insertAt Unsafe vals' idx v
                    in
                      ( childSum
                      , childMask
                      , children'
                      , keys''
                      , vals''
                      , dataBitmap'
                      )

                  ManyMap childCount !child' ->
                    let
                      !nrInChildren' = childSum + childCount
                      !childMask' = childMask `setBit` ix
                      !idx = popCount $ childMask
                      -- Notice that the ordering we handle the children in, is
                      -- the same as what the hashes of the child node present
                      -- in the bitmap corresponds to. So we can actually always
                      -- write at the end of the new "filtered" array.
                      !children'' = Array.replaceAt Unsafe children' idx child'
                    in
                      ( nrInChildren'
                      , childMask'
                      , children''
                      , keys'
                      , vals'
                      , dataBitmap
                      )
          calcRemovedChild _ _ _ = error "impossible"

          (nrInChildren, childMask, children', keys'', vals'', bitmapAllInlined) =
            Contiguous.ifoldl'
              calcRemovedChild
              ( 0
              , zeroBits
              , Contiguous.create (Contiguous.new (Contiguous.size children))
              , keys'
              , vals'
              , bitmapOnlyInline
              )
              children

          -- Same trick applied to the inline key bitmap.
          childBitmap = maskBitmap (childMask :: Word32) (bitmap !>>. 32)
          bitmapRemovedChildren = childBitmap !<<. HASH_CODE_LENGTH
          children'' = Contiguous.create $ do
            dst <- Array.unsafeThaw children'
            Contiguous.resize dst (popCount childMask)

          -- Reduce when appropriate for the cardinality in the node
          matchOnKeys b k v = \case
            0 -> EmptyMap
            1 -> SingletonMap (hash (Contiguous.index k 0)) (Contiguous.index k 0) (Contiguous.index v 0)
            n -> ManyMap n (CompactNode b k v Contiguous.empty)

          -- Do some optimistic shortcircuiting, to quicken leaves.
          -- When there're no subnodes, only have to filter inline entries.
          nrInlineKeys' = fromIntegral $ Contiguous.size keys'
          nrInlineKeys'' = fromIntegral $ Contiguous.size keys''
          shortcut c _ | Contiguous.null c =
            matchOnKeys bitmapOnlyInline keys' vals' nrInlineKeys'
          -- If all subnodes were filtered out, we only need take into account
          -- subnodes that were inlined
          shortcut _ c | Contiguous.null c = matchOnKeys bitmapAllInlined keys'' vals'' nrInlineKeys''
          shortcut _ !c =
            ManyMap
              ( let !total = nrInChildren + nrInlineKeys'' in total )
              ( let !bitmapComplete = bitmapAllInlined .|. bitmapRemovedChildren
                in CompactNode
                  bitmapComplete
                  keys''
                  vals''
                  c
              )
        in shortcut children children''

-- | It's quite useful to take an existing bitmap, and using a bitmask on
-- the elements, remove bits from the bitmap. To avoid having to rehash values
-- to determine the positions of elements in the bitmap.
maskBitmap :: (Bits a, Bits b, Bits c, Num a, Num b, Num c) => a -> b -> c
{-# INLINE maskBitmap #-}
maskBitmap !booleanMask !bitmap = let !result = go booleanMask bitmap 0 0 in result
  where
  go !_ !0 !acc !_ = acc
  go !0 !_ !acc !_ = acc
  go !booleanMask !bitmap !acc !ix | bitmap .&. 1 == 0
    = go booleanMask (bitmap !>>. 1) acc (ix + 1)
  go !booleanMask !bitmap !acc !ix | booleanMask .&. 1 == 0
    = go (booleanMask !>>. 1) (bitmap !>>. 1) acc (ix + 1)
  go !booleanMask !bitmap !acc !ix
    = go (booleanMask !>>. 1) (bitmap !>>. 1) (acc `setBit` ix) (ix + 1)

boolMask2
  :: (Contiguous.Contiguous arr1, Element arr1 a, Contiguous.Contiguous arr2, Element arr2 b, Bits bits)
  => (a -> b -> Bool)
  -> arr1 a
  -> arr2 b
  -> bits
{-# INLINE boolMask2 #-}
boolMask2 !f !arr1 !arr2 =
  let createMask !ix !a !b !acc = if f a b then acc `setBit` ix else acc
      !mask = Array.ifoldrZipWith' createMask zeroBits arr1 arr2
  in  mask

filterKeysVals
  :: (MapRepr keys vals k v, Bits bits, Integral bits)
  => (k -> v -> Bool)
  -> ArrayOf (Strict keys) k
  -> ArrayOf vals v
  -> Int
  -> (bits, ArrayOf (Strict keys) k, ArrayOf vals v)
{-# INLINE filterKeysVals #-}
filterKeysVals !f !keys !vals !hint =
  let
    !mask = boolMask2 f keys vals
    !keys' = Array.filterUsingMask keys mask hint
    !vals' = Array.filterUsingMask vals mask hint
  in (mask, keys', vals')

-- | \(O(n)\) Filter this map by retaining only elements satisfying a
-- predicate.
filter :: (Eq k, Hashable k, MapRepr keys vals k v) => (v -> Bool) -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE filter #-}
filter f = filterWithKey (const f)

instance Foldable (HashMapBL k) where
  {-# INLINE foldr #-}
  foldr = Champ.Internal.foldr
  {-# INLINE foldr' #-}
  foldr' = Champ.Internal.foldr'
  {-# INLINE foldl #-}
  foldl = Champ.Internal.foldl
  {-# INLINE foldl' #-}
  foldl' = Champ.Internal.foldl'
  {-# INLINE null #-}
  null = Champ.Internal.null
  {-# INLINE length #-}
  length = Champ.Internal.size

-- TODO: manual impls of the other funs
-- as those are more efficient

instance Foldable (HashMapBB k) where
  {-# INLINE foldr #-}
  foldr = Champ.Internal.foldr
  {-# INLINE foldr' #-}
  foldr' = Champ.Internal.foldr'
  {-# INLINE foldl #-}
  foldl = Champ.Internal.foldl
  {-# INLINE foldl' #-}
  foldl' = Champ.Internal.foldl'
  {-# INLINE null #-}
  null = Champ.Internal.null
  {-# INLINE length #-}
  length = Champ.Internal.size

-- TODO: manual impls of the other funs
-- as those are more efficient


instance (Prim k) => Foldable (HashMapUL k) where
  {-# INLINE foldr #-}
  foldr = Champ.Internal.foldr
  {-# INLINE foldr' #-}
  foldr' = Champ.Internal.foldr'
  {-# INLINE foldl #-}
  foldl = Champ.Internal.foldl
  {-# INLINE foldl' #-}
  foldl' = Champ.Internal.foldl'
  {-# INLINE null #-}
  null = Champ.Internal.null
  {-# INLINE length #-}
  length = Champ.Internal.size

-- TODO: manual impls of the other funs
-- as those are more efficient

instance (Prim k) => Foldable (HashMapUB k) where
  {-# INLINE foldr #-}
  foldr = Champ.Internal.foldr
  {-# INLINE foldr' #-}
  foldr' = Champ.Internal.foldr'
  {-# INLINE foldl #-}
  foldl = Champ.Internal.foldl
  {-# INLINE foldl' #-}
  foldl' = Champ.Internal.foldl'
  {-# INLINE null #-}
  null = Champ.Internal.null
  {-# INLINE length #-}
  length = Champ.Internal.size

-- TODO: manual impls of the other funs
-- as those are more efficient

-- TODO: Would be nice to offer Foldable for
-- maps with unboxed values,
-- but without some tricksy workaround this is impossible.

-- instance Foldable (HashMapBU k) where
--   foldr = Champ.Internal.foldr
--   foldr' = Champ.Internal.foldr'
--   -- TODO: manual impls of the other funs
--   -- as those are more efficient

-- instance (Prim k) => Foldable (HashMapUU k) where
--   foldr = Champ.Internal.foldr
--   foldr' = Champ.Internal.foldr'
--   -- TODO: manual impls of the other funs
--   -- as those are more efficient

{-# INLINE foldr #-}
foldr :: (MapRepr keys vals k v) => (v -> r -> r) -> r -> HashMap keys vals k v -> r
foldr f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, _k, v #) | #) -> f v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap _keys !vals !children) z =
      z
        & flip (Contiguous.foldr f) vals
        & flip (Contiguous.foldr go) children

{-# INLINE foldl #-}
foldl :: (MapRepr keys vals k v) => (r -> v -> r) -> r -> HashMap keys vals k v -> r
foldl f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, _k, v #) | #) -> f z0 v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go z (MapNode _bitmap _keys !vals !children) =
      z
        & flip (Contiguous.foldl go) children
        & flip (Contiguous.foldl f) vals


{-# INLINE foldr' #-}
foldr' :: (MapRepr keys vals k v) => (v -> r -> r) -> r -> HashMap keys vals k v -> r
foldr' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, _k, v #) | #) -> f v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap _keys !vals !children) !z =
      z
        & flip (Contiguous.foldr' f) vals
        & flip (Contiguous.foldr' go) children

{-# INLINE foldl' #-}
foldl' :: (MapRepr keys vals k v) => (r -> v -> r) -> r -> HashMap keys vals k v -> r
foldl' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, _k, v #) | #) -> f z0 v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go !z (MapNode _bitmap _keys !vals !children) =
      z
        & flip (Contiguous.foldl' go) children
        & flip (Contiguous.foldl' f) vals

{-# INLINE foldrWithKey #-}
foldrWithKey :: (MapRepr keys vals k v) => (k -> v -> r -> r) -> r -> HashMap keys vals k v -> r
foldrWithKey f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, k, v #) | #) -> f k v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap keys !vals !children) z =
      z
        & (\acc -> (Contiguous.foldrZipWith f) acc keys vals)
        & flip (Contiguous.foldr go) children


{-# INLINE foldrWithKey' #-}
foldrWithKey' :: (MapRepr keys vals k v) => (k -> v -> r -> r) -> r -> HashMap keys vals k v -> r
foldrWithKey' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, k, v #) | #) -> f k v z0
  (# | | (# _size, node0 #) #) -> Exts.inline go node0 z0
  where
    go (MapNode _bitmap !keys !vals !children) !z =
      z
        & (\acc -> (Array.foldrZipWith' f) acc keys vals)
        & flip (Contiguous.foldr' go) children

{-# INLINE foldlWithKey #-}
foldlWithKey :: (MapRepr keys vals k v) => (r -> k -> v -> r) -> r -> HashMap keys vals k v -> r
foldlWithKey f z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, k, v #) | #) -> f z0 k v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go !z (MapNode _bitmap keys !vals !children) =
      z
        & flip (Contiguous.foldl go) children
        & (\acc -> (Array.foldlZipWith f) acc keys vals)

{-# INLINE foldlWithKey' #-}
foldlWithKey' :: (MapRepr keys vals k v) => (r -> k -> v -> r) -> r -> HashMap keys vals k v -> r
foldlWithKey' f !z0 m = case matchMap m of
  (# (# #) | | #) -> z0
  (# | (# _h, k, v #) | #) -> f z0 k v
  (# | | (# _size, node0 #) #) -> Exts.inline go z0 node0
  where
    go !z (MapNode _bitmap keys !vals !children) =
      z
        & (\acc -> (Contiguous.foldlZipWith' f) acc keys vals)
        & flip (Contiguous.foldl' go) children

-- | \(O(n)\) Reduce the map by applying a function to each element
-- and combining the results with a monoid operation.
--
-- Iteration order is unspecified
foldMapWithKey :: (MapRepr keys vals k v, Monoid m) => (k -> v -> m) -> HashMap keys vals k v -> m
foldMapWithKey f m = case matchMap m of
  (# (# #) | | #) -> mempty
  (# | (# _h, k, v #) | #) -> f k v
  (# | | (# _size, node0 #) #) -> Exts.inline go node0
  where
    go (MapNode _bitmap keys vals !children) = 
      (Contiguous.foldrZipWith (\k v acc -> acc <> (f k v) )) mempty keys vals
      <> Contiguous.foldMap go children
{-# INLINE foldMapWithKey #-}

-- | \(O(n)\) Returns a list of the map's keys.
--
-- The resulting list is produced lazily and participates in list fusion.
-- The order of keys is unspecified.
keys :: MapRepr keys vals k v => HashMap keys vals k v -> [k]
{-# INLINE keys #-}
keys = Prelude.map fst . Champ.Internal.toList

-- | \(O(n)\) Returns a list of the map's elements (or values).
--
-- The resulting list is produced lazily and participates in list fusion.
-- The order of elements is unspecified.
elems :: MapRepr keys vals k v => HashMap keys vals k v -> [v]
{-# INLINE elems #-}
elems = Prelude.map snd . Champ.Internal.toList

instance (Show k, Show v, MapRepr keys vals k v) => Show (HashMap keys vals k v) where
    show m = "Champ.HashMap.fromList " <> show (Champ.Internal.toList m)

-- | Turn a HashMap into a HashMap with the same structure but another storage mechanism.
--
-- For example, turn a `HashMapBL` into a `HashMapBB` to force all laziness in all its values.
-- Or turn a `HashMapBB` into a `HashMapUU` to unbox all keys and values.
--
-- \(O(n)\), walks over the complete original hashmap and constructs a deep copy of all of its parts,
-- with the new storage mechanism.
--
convert :: forall h1 h2 {ks} {ks'} {vs} {vs'} {k} {v}. (h1 ~ HashMap ks vs, h2 ~ HashMap ks' vs', MapRepr ks vs k v, MapRepr ks' vs' k v) => HashMap ks vs k v -> HashMap ks' vs' k v
{-# INLINE [2] convert #-}
convert m = case matchMap m of
  (# (# #) | | #) -> EmptyMap
  (# | (# h, k, v #) | #) -> SingletonMap h k v
  (# | | (# size, node #) #) -> ManyMap size (convert' node)
  where
    convert' :: MapNode ks vs k v -> MapNode ks' vs' k v
    convert' (MapNode bitmap keys vals children) = (MapNode bitmap (Contiguous.convert keys) (Contiguous.convert vals) (Contiguous.map convert' children))

-- {-# RULES 
-- "Champ.HashMap.convert to the identical type" forall (h :: HashMap ks vs k v). convert @(HashMap ks vs) @(HashMap ks vs) h = h
--   #-}


convertDropVals :: forall h1 h2 {ks} {ks'} {vs} {k} {v}. (h1 ~ HashMap ks vs, h2 ~ HashMap ks' Unexistent, MapRepr ks vs k v, MapRepr ks' Unexistent k ()) => HashMap ks vs k v -> HashMap ks' Unexistent k ()
{-# INLINE [2] convertDropVals #-}
convertDropVals m = case matchMap m of
  (# (# #) | | #) -> EmptyMap
  (# | (# h, k, _v #) | #) -> SingletonMap h k ()
  (# | | (# size, node #) #) -> ManyMap size (convert' node)
  where
    convert' :: MapNode ks vs k v -> MapNode ks' Unexistent k ()
    convert' (MapNode bitmap keys vals children) = (MapNode bitmap (Contiguous.convert keys) (Contiguous.replicate (Contiguous.size vals) ()) (Contiguous.map convert' children))

-- | Print the internal representation of the hashmap.
--
-- Used for debugging the internals of Champ.HashMap
-- and potentially figure out where its internal invariants might have been broken.
-- Not intended for wider usage.
debugShow :: (Show (ArrayOf (Strict keys) k), Show (ArrayOf vals v), Show k, Show v, MapRepr keys vals k v) => HashMap keys vals k v -> String
debugShow EmptyMap = "EmptyMap"
debugShow (SingletonMap _h k v) = "(SingletonMap " <> show k <> " " <> show v <> ")"
debugShow (ManyMap _size node) = "(ManyMap " <> debugShowNode node <> ")"

debugShowNode :: (Show (ArrayOf (Strict keys) k), Show (ArrayOf vals v), Show k, Show v, MapRepr keys vals k v) => MapNode keys vals k v -> String
debugShowNode (CollisionNode keys vals) = "(CollisionNode " <> show keys <> " " <> show vals <> ")"
debugShowNode (CompactNode bitmap keys vals children) = "(CompactNode " <> show bitmap <> " " <> show keys <> " " <> show vals <> " [" <> showChildren children <> "])"
  where
    showChildren xs = 
      xs
      & Foldable.toList
      & fmap debugShowNode
      & List.intercalate ","

-- | \(O(n + m)\) The union of two maps. If a key occurs in both maps, the
-- mapping from the first will be the mapping in the result.
--
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
union :: (Hashable k, MapRepr keys vals k v) => HashMap keys vals k v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE union #-}
union l r = Champ.Internal.fromList (Champ.Internal.toList r <> Champ.Internal.toList l) 

-- | \(O(n + m * log32(n + m))\) The union of two maps. If a key occurs in both maps, the
-- function is called with both values to create the resulting value.
-- 
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
unionWith :: (Hashable k, MapRepr keys vals k v) => (v -> v -> v) -> HashMap keys vals k v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unionWith #-}
unionWith f l r = Champ.Internal.fromListWith f (Champ.Internal.toList r <> Champ.Internal.toList l)

-- | \(O(n + m * log32(n + m))\) The union of two maps. If a key occurs in both maps, the
-- function is called with the key and both values to create the resulting value.
-- 
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
unionWithKey :: (Hashable k, MapRepr keys vals k v) => (k -> v -> v -> v) -> HashMap keys vals k v -> HashMap keys vals k v -> HashMap keys vals k v
{-# INLINE unionWithKey #-}
unionWithKey f l r = Champ.Internal.fromListWithKey f (Champ.Internal.toList r <> Champ.Internal.toList l)

-- | The union of a list (or other foldable) of maps.
--
-- O((n * m) * log32(n * m)) for @n@ maps each having at most @m@ keys.
-- 
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
unions :: (Foldable f, Hashable k, MapRepr keys vals k v) => f (HashMap keys vals k v) -> HashMap keys vals k v
{-# INLINE unions #-}
unions maps = 
  maps
  & Foldable.toList
  & fmap Champ.Internal.toList
  & Control.Monad.join
  & Champ.Internal.fromList


-- | \(O(n \log m)\) Difference of two maps. Return elements of the first map
-- not existing in the second.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
difference :: (Hashable k, MapRepr keys vals k v, MapRepr keys vals' k w) => HashMap keys vals k v -> HashMap keys vals' k w -> HashMap keys vals k v
{-# INLINE difference #-}
difference a b = foldlWithKey' go empty a
  where
    go m k v = case member k b of
                 False -> unsafeInsert k v m
                 _       -> m

-- | \(O(n \log m)\) Intersection of two maps. Return elements of the first map
-- also existing in the second.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
intersection :: (Hashable k, MapRepr keys vals k v, MapRepr keys vals' k w) => HashMap keys vals k v -> HashMap keys vals' k w -> HashMap keys vals k v
{-# INLINE intersection #-}
intersection a b = foldlWithKey' go empty a
  where
    go m k v = lookupKVKnownHash# absent present h k b
      where
        h = hash k
        absent (# #) = m
        present k' _ = insert' Unsafe h k' v m

-- | \(O(n \log m)\) Difference with a combining function. When two equal keys are
-- encountered, the combining function is applied to the values of these keys.
-- If it returns 'Nothing', the element is discarded (proper set difference). If
-- it returns (@'Just' y@), the element is updated with a new value @y@.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
differenceWith :: (Hashable k, MapRepr keys vals k v, MapRepr keys vals' k w) => (v -> w -> Maybe v) -> HashMap keys vals k v -> HashMap keys vals' k w -> HashMap keys vals k v
differenceWith f a b = foldlWithKey' go empty a
  where
    go m k v =  lookupKVKnownHash# absent present h k b
      where
        h = hash k
        absent (# #) = insert' Unsafe h k v m
        present k' w = maybe m (\y -> insert' Unsafe h k' y m) (f v w)
{-# INLINABLE differenceWith #-}

-- | \(O(n \log m)\) Intersection with a combining function
--
-- When a key exists in both maps, the combining function is applied to the values.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
intersectionWith :: (Hashable k, MapRepr keys vals1 k v1, MapRepr keys vals2 k v2, MapRepr keys vals3 k v3) => (v1 -> v2 -> v3) -> HashMap keys vals1 k v1 -> HashMap keys vals2 k v2 -> HashMap keys vals3 k v3
intersectionWith f = Exts.inline intersectionWithKey (const f)
{-# INLINABLE intersectionWith #-}

-- | \(O(n \log m)\) Intersection with a combining function that also receives the key.
--
-- When a key exists in both maps, the combining function is applied to the values.
-- Variant of `intersectionWith`
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
intersectionWithKey :: (Hashable k, MapRepr keys vals1 k v1, MapRepr keys vals2 k v2, MapRepr keys vals3 k v3) => (k -> v1 -> v2 -> v3) -> HashMap keys vals1 k v1 -> HashMap keys vals2 k v2 -> HashMap keys vals3 k v3
intersectionWithKey f a b = foldlWithKey' go empty a
  where
    go m k v = 
      let h = hash k in
      lookupKVKnownHash# absent present h k b
        where
          h = hash k
          absent (# #) = m
          present k' w = insert' Unsafe h k' (f k' v w) m
{-# INLINABLE intersectionWithKey #-}

-- | Inclusion of maps. A map is included in another map if the keys are subsets and the corresponding values are equal:
--
-- Complexity: \O(n log32(m))\, where \(n)\ and \(m)\ are the sizes of the two hashmaps.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
isSubmapOf :: (Hashable k, Eq v, MapRepr keys vals k v) => HashMap keys vals k v -> HashMap keys vals k v -> Bool
{-# INLINABLE isSubmapOf #-}
isSubmapOf a b = isSubmapOfBy (==) a b

-- | Inclusion of maps using a custom value-equality function.
--
-- Generalization of `isSubmapOf`.
--
-- Complexity: \O(n log32(m))\, where \(n)\ and \(m)\ are the sizes of the two hashmaps.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
isSubmapOfBy :: forall keys k vals1 vals2 v1 v2. (Hashable k, MapRepr keys vals1 k v1, MapRepr keys vals2 k v2) => (v1 -> v2 -> Bool) -> HashMap keys vals1 k v1 -> HashMap keys vals2 k v2 -> Bool
{-# INLINABLE isSubmapOfBy #-}
isSubmapOfBy comp a b = foldrWithKey go True b
  where
    go :: k -> v2 -> Bool -> Bool
    go k v bool = case lookupKV# k a of
      (# (# #) | #) -> False
      (# | (# _k, v' #) #) -> bool && comp v' v


-- | Relate the keys of one map to the values of
-- the other, by using the values of the former as keys for lookups
-- in the latter.
--
-- Complexity: \( O (n * \log(m)) \), where \(m\) is the size of the first argument
--
-- >>> charToString = Champ.HashMap.fromList [('a', "A"), ('b', "B")] :: HashMapBB Char String
-- >>> intToChar = Champ.HashMap.fromList [(1,'a'),(2,'b'),(3,'z')] :: HashMapUU Int Char
-- >>> compose charToString intToChar
-- Champ.HashMap.fromList [(1,"A"),(2,"B")]
compose :: (Eq a, Hashable a, Eq b, Hashable b, MapRepr as bs a b, MapRepr bs' cs b c, MapRepr as cs a c) => HashMap bs' cs b c -> HashMap as bs a b -> HashMap as cs a c
{-# INLINE compose #-}
compose bc !ab
  | null bc = empty
  | otherwise = mapMaybe' (\b -> lookup b bc) ab

-- Inside a MapNode,
-- the lower 32 bits indicate the inline leaves
-- and the higher 32 bits indicate child nodes
newtype Bitmap = Bitmap Word64
  deriving (Eq, Ord, Num, Bits, Enum, Real, Integral, FiniteBits, NFData)

instance Show Bitmap where
  -- \| Shows a bitmap in binary, with the lowest bit on the left
  -- and always padded to 32 bits
  show :: Bitmap -> String
  show bitmap = "( " <> show32 bitmap <> ", " <> show32 (bitmap `unsafeShiftR` HASH_CODE_LENGTH) <> ")"
    where
      show32 b = take 32 $ reverse (showBin b "") <> repeat '0'

newtype Mask = Mask Word
  deriving (Eq, Ord, Show)

newtype Hash = Hash Word64
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

-- intsToList :: HashMapBL Int Int -> [(Int, Int)]
-- intsToList = Champ.Internal.toList

-- sumStrict :: HashMapBB Int Int -> Int
-- sumStrict = foldr' (+) 0

-- sumLazy :: HashMapBL Int Int -> Int
-- sumLazy = foldr' (+) 0

-- | Simplified usage of `withCoercible` for the common case where you want to directly coerce the hashmap itself.
--
-- `Data.Coerce.coerce` but specialized to `Champ.HashMap`.
-- See `withCoercible` for more information.
coerce :: forall v v' keys vals k. (Coercible v v', MapRepr keys vals k v, MapRepr keys vals k v') => HashMap keys vals k v -> HashMap keys vals k v'
coerce x = withCoercible @(HashMap keys vals k v) @(HashMap keys vals k v') (Data.Coerce.coerce x)

-- | Brings a `Coercible` instance in scope to coerce between two `Champ.HashMap`s
-- 
-- Because of the way Champ.HashMap is currently implemented (using associated data families),
-- the role annotation of the value type inside Champ.HashMap is `nominal` rather than `representational`.
--
-- This is a problem when you want to use `Data.Coerce.coerce`.
-- This function allows an escape hatch to do so anyway.
--
-- You will need to specify the precise desired types `h1` and `h2`,
-- otherwise GHC will complain that the types might not match in representation.
withCoercible :: forall h1 h2 {keys} {vals} {k} {v} {v'} r. (Coercible v v', h1 ~ HashMap keys vals k v, h2 ~ HashMap keys vals k v', MapRepr keys vals k v, MapRepr keys vals k v') => (Coercible h1 h2 => r) -> r
withCoercible val = case (unsafeCoerce (Coercion @v @v') :: Coercion h1 h2) of
  Coercion -> val

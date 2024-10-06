{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-stg-from-core -ddump-to-file #-}
module MyLib where

import Data.Primitive
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Function ((&))

import Storage (Storage(..), StrictStorage(..), ArrayOf)
-- import Array (Array)
import Array qualified
import Data.Primitive.Contiguous (Contiguous)

type MapBL = Map Boxed Lazy
type MapBB = Map Boxed (Strict Boxed)
type MapBU = Map Boxed (Strict Unboxed)
type MapUL = Map Unboxed Lazy
type MapUB = Map Unboxed (Strict Boxed)
type MapUU = Map Unboxed (Strict Unboxed)

pattern EmptyMap 
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   Map keyStorage valStorage k v
{-# INLINE EmptyMap #-}
pattern EmptyMap <- (matchMap -> (# (##) | | #)) where
    EmptyMap = emptyMap

pattern SingletonMap 
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   k -> v -> Map keyStorage valStorage k v
{-# INLINE SingletonMap #-}
pattern SingletonMap k v <- (matchMap -> (# | (# k, v #) | #)) where
    SingletonMap = singletonMap

pattern ManyMap
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   MapNode keyStorage valStorage k v -> Map keyStorage valStorage k v
{-# INLINE ManyMap #-}
pattern ManyMap node <- (matchMap -> (#  | | node #)) where
    ManyMap = manyMap

{-# COMPLETE EmptyMap, SingletonMap, ManyMap #-}

{-# INLINE MapNode #-}
pattern MapNode :: MapRepr keyStorage valStorage k v => Bitmap -> (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> SmallArray (MapNode keyStorage valStorage k v) -> MapNode keyStorage valStorage k v
pattern MapNode bitmap keys vals children <- (unpackNode -> (# bitmap, keys, vals, children #)) where
    MapNode bitmap keys vals children = packNode (# bitmap, keys, vals, children #)

pattern CollisionNode :: MapRepr keyStorage valStorage k v => (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> MapNode keyStorage valStorage k v
{-# INLINE CollisionNode #-}
pattern CollisionNode keys vals <- (unpackNode -> (# _, keys, vals, _ #)) where
    CollisionNode keys vals = packNode (# 0, keys, vals, SmallArray.emptySmallArray #)

pattern CompactNode :: MapRepr keyStorage valStorage k v => Bitmap -> (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> SmallArray (MapNode keyStorage valStorage k v) -> MapNode keyStorage valStorage k v
{-# INLINE CompactNode #-}
pattern CompactNode bitmap keys vals children <- (unpackNode -> (# (isNonZeroBitmap -> (# | bitmap #) ), keys, vals, children #)) where
    CompactNode bitmap keys vals children = packNode (# bitmap, keys, vals, children #)

{-# INLINE isNonZeroBitmap #-}
isNonZeroBitmap :: Bitmap -> (# (# #) | Bitmap #)
isNonZeroBitmap 0 = (# (# #) | #)
isNonZeroBitmap b = (# | b #)

{-# COMPLETE CollisionNode, CompactNode #-}
{-# COMPLETE MapNode #-}

{- | A CHAMP-based Hashmap.

This is implemented as a typeclass containing a data family called `Map`
rather than a plain datatype,
to ensure GHC can unbox all intermediate polymorphic constructors
(that depend on the concrete types of `keyStorage` and `valStorage`).

Conceptually, you can think of it as:

```
data Map (keys :: StrictStorage) (vals :: Storage) k v = EmptyMap | SingletonMap !k v | ManyMap (MapNode k v)

data MapNode keys vals k v
    = CollisionNode !(ArrayOf (Strict keys)) !(ArrayOf vals)
    | CompactNode !Bitmap !Bitmap !(ArrayOf (Strict keys)) !(ArrayOf vals) !(SmallArray (MapNode keys vals k v))
```
with the following tricks:
- We only store a single 64-bit bitmap (taking up one word) rather than two separate 32-bit bitmaps,
  and use its lower/higher 32 bits using masking and shifting instead, saving one word per map node.
- There is no special `CollisionNode` variant. 
  Instead, we disambiguate using the special bitmap value '0'
  which can never occur in a valid CompactNode as they are never empty.
- As mentioned above, we make sure that GHC unpacks the intermediate array boxes,
  so we store the `SmallArray#` resp `ByteArray#` pointers directly.
  This results in one word saved for the outer map and three more words saved per map node.
-}
class (Contiguous (ArrayOf (Strict keyStorage)), Contiguous (ArrayOf (valStorage))) => MapRepr (keyStorage :: StrictStorage) (valStorage :: Storage) k v where
  data Map keyStorage valStorage k v
  data MapNode keyStorage valStorage k v
  packNode :: (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #) -> MapNode keyStorage valStorage k v
  unpackNode :: MapNode keyStorage valStorage k v -> (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #)
  manyMap :: MapNode keyStorage valStorage k v -> Map keyStorage valStorage k v
  emptyMap :: Map keyStorage valStorage k v
  singletonMap :: k -> v -> Map keyStorage valStorage k v
  matchMap :: Map keyStorage valStorage k v -> (# (##) | (# k, v #) | MapNode keyStorage valStorage k v #)

#define MAP_NODE_NAME(name) MapNode/**/_/**/name

#define MAP_NODE_FIELDS(keystorage, valstorage) \
     !Bitmap \
     !((ArrayOf (Strict (keystorage))) k) \
     !((ArrayOf (valstorage)) v) \
     !(SmallArray (MapNode (keystorage) (valstorage) k v))

#define map_repr_instance(name, keystorage, valstorage, constraints)                                       \
instance constraints => MapRepr (keystorage) (valstorage) k v where                                        \
{ {-# INLINE unpackNode #-}                                                                                \
; unpackNode (MAP_NODE_NAME(name) b keys vals children) = (# b, keys, vals, children #)                    \
; {-# INLINE packNode #-}                                                                                  \
; packNode (# b, keys, vals, children #) = (MAP_NODE_NAME(name) b keys vals children)                      \
; {-# INLINE emptyMap #-}                                                                                  \
; emptyMap = EmptyMap_/**/name                                                                             \
; {-# INLINE singletonMap #-}                                                                              \
; singletonMap !k v = SingletonMap_/**/name k v                                                            \
; {-# INLINE manyMap #-}                                                                                   \
; manyMap (MAP_NODE_NAME(name) b keys vals children) = ManyMap_/**/name b keys vals children               \
; {-# INLINE matchMap #-}                                                                                  \
; matchMap = \case {                                                                                       \
; EmptyMap_/**/name -> (# (# #) | | #)                                                                     \
; SingletonMap_/**/name k v -> (#  | (# k, v #) | #)                                                       \
; ManyMap_/**/name b keys vals children -> (# | | MAP_NODE_NAME(name) b keys vals children #) }            \
; data MapNode (keystorage) (valstorage) k v = MAP_NODE_NAME(name) MAP_NODE_FIELDS(keystorage, valstorage) \
; data Map (keystorage) (valstorage) k v                                                              \
  = EmptyMap_/**/name                                                                                      \
  | SingletonMap_/**/name !k v                                                                             \
  | ManyMap_/**/name MAP_NODE_FIELDS(keystorage, valstorage)                                               \
}

map_repr_instance(Boxed_Lazy, Boxed, Lazy, ())
map_repr_instance(Boxed_Boxed, Boxed, Strict Boxed, ())
map_repr_instance(Boxed_Unboxed, Boxed, Strict Unboxed, (Prim v))

map_repr_instance(Unboxed_Lazy, Unboxed, Lazy, (Prim k))
map_repr_instance(Unboxed_Boxed, Unboxed, Strict Boxed, (Prim k))
map_repr_instance(Unboxed_Unboxed, Unboxed, Strict Unboxed, (Prim k, Prim v))

newtype Bitmap = Bitmap Int
  deriving (Eq, Ord, Show, Num)

someFunc = undefined

null :: MapRepr keys vals k v => Map keys vals k v -> Bool
null EmptyMap = True
null _ = False

empty :: MapRepr keys vals k v => Map keys vals k v
empty = EmptyMap

singleton :: MapRepr keys vals k v => k -> v -> Map keys vals k v
singleton !k v = SingletonMap k v

-- {-# INLINE foldr' #-}
-- foldr' :: MapRepr keys vals k v => (v -> r -> r) -> r -> Map keys vals k v -> r
-- foldr' f z0 m = case m of
--    EmptyMap -> z0
--    SingletonMap _k v -> f v z0
--    (ManyMap node0) -> go node0 z0
--    where
--       go (MapNode _bitmap _keys vals children) z = 
--         z
--         & flip (Array.foldr' f) vals
--         & flip (Array.foldr' go) children

-- -- foldr' _f z EmptyMap = z
-- -- foldr' f !z (SingletonMap _k !v) = f v z
-- -- foldr' f z0 (ManyMap !node0) = go z0 node0 where
-- --     go z (MapNode _bitmap _keys vals children) = 
-- --         z
-- --         & flip (Array.foldr' f) vals
-- --         & flip (Array.foldr' (flip go)) children

-- {-# INLINE foldr'2 #-}
-- foldr'2 :: MapRepr keys vals k v => (v -> r -> r) -> r -> Map keys vals k v -> r
-- foldr'2 f z0 m = case matchMap m of
--   (# (##) | | #) -> z0
--   (# | (# _k, v #) | #) -> f v z0
--   (# | | node0 #) -> go node0 z0
--     where
--       go (MapNode _bitmap _keys vals children) z = 
--         z
--         & flip (Array.foldr' f) vals
--         & flip (Array.foldr' go) children

-- {-# INLINE foldl'2 #-}
-- foldl'2 :: MapRepr keys vals k v => (r -> v -> r) -> r -> Map keys vals k v -> r
-- foldl'2 f z0 m = case matchMap m of
--   (# (##) | | #) -> z0
--   (# | (# _k, v #) | #) -> f z0 v
--   (# | | node0 #) -> go z0 node0 
--     where
--       go z (MapNode _bitmap _keys vals children) = 
--         z
--         & flip (Array.foldl' f) vals
--         & flip (Array.foldl' go) children


-- mysum :: MapUU Int Int -> Int
-- mysum = foldl'2 (+) 0

-- mysumTwo :: MapUU Int Int -> Int
-- mysumTwo = foldr'2 (+) 0

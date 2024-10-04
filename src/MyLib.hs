{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-from-core -ddump-to-file #-}
module MyLib where

import Data.Kind
import Data.Primitive
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Array.Unboxed (UArray)
import Foreign.Storable qualified as Storable

-- type LazyChampMap = ChampMap Boxed Lazy
-- type StrictChampMap = ChampMap Boxed (Strict Boxed)
-- type UnboxedChampMap = ChampMap Boxed (Strict Unboxed)
-- type UnboxedUnboxedChampMap = ChampMap Unboxed (Strict Unboxed)

data Storage = Lazy | Strict StrictStorage
data StrictStorage = Boxed | Unboxed -- | Storable

pattern EmptyMap 
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   ChampMap keyStorage valStorage k v
{-# INLINE EmptyMap #-}
pattern EmptyMap <- (matchMap -> (# (##) | | #)) where
    EmptyMap = emptyMap

pattern SingletonMap 
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   k -> v -> ChampMap keyStorage valStorage k v
{-# INLINE SingletonMap #-}
pattern SingletonMap k v <- (matchMap -> (#  | (# k, v #) | #)) where
    SingletonMap = singletonMap

pattern ManyMap
  :: forall (keyStorage :: StrictStorage) (valStorage :: Storage) k v.
   MapRepr keyStorage valStorage k v =>
   MapNode keyStorage valStorage k v -> ChampMap keyStorage valStorage k v
{-# INLINE ManyMap #-}
pattern ManyMap node <- (matchMap -> (#  | | node #)) where
    ManyMap = manyMap

{-# COMPLETE EmptyMap, SingletonMap, ManyMap #-}

pattern CollisionNode :: MapRepr keyStorage valStorage k v => (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> MapNode keyStorage valStorage k v
{-# INLINE CollisionNode #-}
pattern CollisionNode keys vals <- (unpackNode -> (# _, keys, vals, _ #)) where
    CollisionNode keys vals = packNode (# 0, keys, vals, SmallArray.emptySmallArray #)

pattern CompactNode :: MapRepr keyStorage valStorage k v => Bitmap -> (ArrayOf (Strict keyStorage) k) -> (ArrayOf valStorage) v -> SmallArray (MapNode keyStorage valStorage k v) -> MapNode keyStorage valStorage k v
{-# INLINE CompactNode #-}
pattern CompactNode bitmap keys vals children <- (unpackNode -> (# bitmap, keys, vals, children #)) where
    CompactNode bitmap keys vals children = packNode (# bitmap, keys, vals, children #)

{-# COMPLETE CollisionNode, CompactNode #-}

class MapRepr (keyStorage :: StrictStorage) (valStorage :: Storage) k v where
  data ChampMap keyStorage valStorage k v
  data MapNode keyStorage valStorage k v
  packNode :: (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #) -> MapNode keyStorage valStorage k v
  unpackNode :: MapNode keyStorage valStorage k v -> (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #)
  manyMap :: MapNode keyStorage valStorage k v -> ChampMap keyStorage valStorage k v
  emptyMap :: ChampMap keyStorage valStorage k v
  singletonMap :: k -> v -> ChampMap keyStorage valStorage k v
  matchMap :: ChampMap keyStorage valStorage k v -> (# (##) | (# k, v #) | MapNode keyStorage valStorage k v #)

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
; data ChampMap (keystorage) (valstorage) k v                                                              \
  = EmptyMap_/**/name                                                                                      \
  | SingletonMap_/**/name !k v                                                                             \
  | ManyMap_/**/name MAP_NODE_FIELDS(keystorage, valstorage)                                               \
}

map_repr_instance(Boxed_Lazy, Boxed, Lazy, ())
map_repr_instance(Boxed_Boxed, Boxed, Strict Boxed, ())
map_repr_instance(Boxed_Unboxed, Boxed, Strict Unboxed, (Prim v))
-- map_repr_instance(Boxed_Storable, Boxed, Strict Storable, (Storable.Storable v))

map_repr_instance(Unboxed_Lazy, Unboxed, Lazy, (Prim k))
map_repr_instance(Unboxed_Boxed, Unboxed, Strict Boxed, (Prim k))
map_repr_instance(Unboxed_Unboxed, Unboxed, Strict Unboxed, (Prim k, Prim v))
-- map_repr_instance(Unboxed_Storable, Unboxed, Strict Storable, (Prim k, Storable.Storable v))

-- map_repr_instance(Storable_Lazy, Storable, Lazy, (Storable.Storable k))
-- map_repr_instance(Storable_Boxed, Storable, Strict Boxed, (Storable.Storable k))
-- map_repr_instance(Storable_Unboxed, Storable, Strict Unboxed, (Storable.Storable k, Prim v))
-- map_repr_instance(Storable_Storable, Storable, Strict Storable, (Storable.Storable k, Storable.Storable v))

type family ArrayOf (s :: Storage) = (r :: Type -> Type) | r -> s where
  ArrayOf Lazy = SmallArray
  ArrayOf (Strict Boxed) = StrictSmallArray
  ArrayOf (Strict Unboxed) = PrimArray
--   ArrayOf (Strict Storable) = StorableArray

newtype Bitmap = Bitmap Int
  deriving (Eq, Ord, Show, Num)

-- -- | Backing store is a ByteArray,
-- -- but all operations read/write using a's Storable instance
-- newtype StorableArray a = StorableArray ByteArray

-- | Backing store is a SmallArray,
-- but all reading/writing is strict in `a`
newtype StrictSmallArray a = StrictSmallArray (SmallArray a)

someFunc = undefined

null :: ChampMap Boxed Lazy k v -> Bool
null EmptyMap = True
null _ = False

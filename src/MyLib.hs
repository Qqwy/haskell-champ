{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}
module MyLib where
import Data.Kind
import Data.Primitive
import Data.Array.Unboxed (UArray)

type LazyChampMap = ChampMap StrictBoxed Lazy
type StrictChampMap = ChampMap StrictBoxed (Strict StrictBoxed)
type UnboxedChampMap = ChampMap StrictBoxed (Strict StrictUnboxed)
type UnboxedUnboxedChampMap = ChampMap StrictUnboxed (Strict StrictUnboxed)

data Storage = Lazy | Strict StrictStorage
data StrictStorage = StrictBoxed | StrictUnboxed | StrictStorable

class MapRepr (keyStorage :: StrictStorage) (valStorage :: Storage) k v where
  data ChampMap keyStorage valStorage k v
  data MapNode keyStorage valStorage k v
  pack :: (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #) -> MapNode keyStorage valStorage k v
  unpack :: MapNode keyStorage valStorage k v -> (# Bitmap, ArrayOf (Strict keyStorage) k, (ArrayOf valStorage) v, SmallArray (MapNode keyStorage valStorage k v) #)

#define MAP_NODE_NAME(name) MapNode/**/_/**/name

#define map_repr_instance(name, keystorage, valstorage) \
instance MapRepr (keystorage) (valstorage) k v where \
{ unpack (MAP_NODE_NAME(name) b keys vals children) = (# b, keys, vals, children #) \
; pack (# b, keys, vals, children #) = (MAP_NODE_NAME(name) b keys vals children) \
; data MapNode (keystorage) (valstorage) k v = MAP_NODE_NAME(name) \
     !Bitmap \
     !((ArrayOf (Strict (keystorage))) k) \
     !((ArrayOf (valstorage)) v) \
     !(SmallArray (MapNode (keystorage) (valstorage) k v)) \
; data ChampMap (keystorage) (valstorage) k v \
  = EmptyMap_/**/name \
  | SingletonMap_/**/name !k v \
  | ManyMap_/**/name {-# UNPACK #-} !(MapNode (keystorage) (valstorage) k v) \
} \

map_repr_instance(StrictBoxed_Lazy, StrictBoxed, Lazy)
map_repr_instance(StrictBoxed_StrictBoxed, StrictBoxed, Strict StrictBoxed)
map_repr_instance(StrictBoxed_StrictUnboxed, StrictBoxed, Strict StrictUnboxed)
map_repr_instance(StrictBoxed_StrictStorable, StrictBoxed, Strict StrictStorable)

map_repr_instance(StrictUnboxed_Lazy, StrictUnboxed, Lazy)
map_repr_instance(StrictUnboxed_StrictBoxed, StrictUnboxed, Strict StrictBoxed)
map_repr_instance(StrictUnboxed_StrictUnboxed, StrictUnboxed, Strict StrictUnboxed)
map_repr_instance(StrictUnboxed_StrictStorable, StrictUnboxed, Strict StrictStorable)

map_repr_instance(StrictStorable_Lazy, StrictStorable, Lazy)
map_repr_instance(StrictStorable_StrictBoxed, StrictStorable, Strict StrictBoxed)
map_repr_instance(StrictStorable_StrictUnboxed, StrictStorable, Strict StrictUnboxed)
map_repr_instance(StrictStorable_StrictStorable, StrictStorable, Strict StrictStorable)

type family ArrayOf (s :: Storage) = (r :: Type -> Type) | r -> s where
  ArrayOf Lazy = SmallArray
  ArrayOf (Strict StrictBoxed) = StrictSmallArray
  ArrayOf (Strict StrictUnboxed) = PrimArray
  ArrayOf (Strict StrictStorable) = StorableArray

newtype Bitmap = Bitmap Int

-- | Backing store is a ByteArray,
-- but all operations read/write using a's Storable instance
newtype StorableArray a = StorableArray ByteArray

-- | Backing store is a SmallArray,
-- but all reading/writing is strict in `a`
newtype StrictSmallArray a = StrictSmallArray (SmallArray a)

someFunc = undefined

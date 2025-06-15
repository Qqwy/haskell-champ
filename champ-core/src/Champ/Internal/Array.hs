{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE UnliftedDatatypes #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-cmm -ddump-stg-tags -ddump-stg-final -ddump-asm -ddump-to-file #-}

module Champ.Internal.Array (
  -- * Array class
  Array (..),
  Element,
  -- * Simple arrays:
  SmallArray, 
  SmallUnliftedArray, 
  SmallUnliftedArray_, 
  SmallUnliftedArray'(..),
  StrictSmallArray, 
  PrimArray, 
  -- * Dealing with strict values:
  Strictly (..), 
  -- * Array helper functions:
  doubletonBranchless, 
  foldrZipWith',
  foldlZipWith,
  ifoldrZipWith',
  findIndex#,
  -- * Arrays to store zero-size values
  UnitArray,
  IsUnit,
  PrimUnlifted,
  -- * Conditionally safe interface
  Safety(..),
  Champ.Internal.Array.insertAt,
  Champ.Internal.Array.replaceAt,
  Champ.Internal.Array.deleteAt,
  Champ.Internal.Array.singleton,
  Champ.Internal.Array.filterUsingMask,
) where

import Champ.Internal.Util (ptrEq)
import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Bits hiding (shift)
import Data.Elevator (Strict (Strict), UnliftedType)
import Data.Foldable qualified as Foldable
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Primitive (PrimArray, SmallArray)
import Data.Primitive.Contiguous (SmallUnliftedArray, Always)
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Primitive.Contiguous.Class (Contiguous (..), ContiguousU (..), MutableSlice (..), Slice (..))
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Primitive.Unlifted.Class (PrimUnlifted (..), Unlifted)
import Data.Primitive.Unlifted.Class qualified as Unlifted
import Data.Primitive.Unlifted.SmallArray (SmallMutableUnliftedArray_ (..), SmallUnliftedArray_ (..), mapSmallUnliftedArray, unsafeThawSmallUnliftedArray, shrinkSmallMutableUnliftedArray)
import Data.Primitive.Unlifted.SmallArray.Primops (SmallMutableUnliftedArray# (SmallMutableUnliftedArray#), SmallUnliftedArray# (SmallUnliftedArray#))
import GHC.Exts (SmallArray#, SmallMutableArray#)
import Prelude hiding (foldl, foldl', foldr, length, null, read)

-- | Helper newtype to implement `PrimUnlifted` for any datatype,
-- part of making `StrictSmallArray`/`StrictSmallMutableArray` work for any `a`.
newtype Strictly a = Strictly {unStrictly :: a}
  deriving newtype (Show, Eq, Ord, Hashable, NFData)

instance PrimUnlifted (Strictly a) where
  type Unlifted (Strictly a) = Strict a
  toUnlifted# (Strictly a) = Strict a
  fromUnlifted# (Strict a) = (Strictly a)

-- | Array type whose elements are guaranteed to be in WHNF,
-- therefore skipping the 'thunk check' whenever an element is read from it.
--
-- An easier to use version of `SmallUnliftedArray`,
-- allowing storage of _any_ `a` by virtue of `Data.Elevator.Strict`
newtype StrictSmallArray a = StrictSmallArray (SmallUnliftedArray_ (Strict a) (Strictly a))
  deriving (Show)


instance Functor StrictSmallArray where
  fmap f (StrictSmallArray arr) = StrictSmallArray (mapSmallUnliftedArray (Strictly . f . unStrictly) arr)

instance Foldable StrictSmallArray where
  foldr = Contiguous.foldr
  foldl = Contiguous.foldl
  foldr' = Contiguous.foldr'
  foldl' = Contiguous.foldl'
  foldMap = Contiguous.foldMap

instance Semigroup (StrictSmallArray a) where
  a <> b = Contiguous.append a b

instance Monoid (StrictSmallArray a) where
  mempty = Contiguous.empty

-- | Mutable array type whose elements are guaranteed to be in WHNF
--
-- An easier to use version of `SmallMutableUnliftedArray`,
-- allowing storage of _any_ `a` by virtue of `Data.Elevator.Strict`
newtype StrictSmallMutableArray s a = StrictSmallMutableArray (SmallMutableUnliftedArray_ (Strict a) s (Strictly a))

-- | Unlifted version of `StrictSmallArray` itself
-- (of kind `UnliftedType`)
newtype StrictSmallArray# (a :: Type)
  = StrictSmallArray# (SmallArray# (Strict a))

-- | Unlifted version of `StrictSmallMutableArray` itself
-- (of kind `UnliftedType`)
newtype StrictSmallMutableArray# s (a :: Type)
  = StrictSmallMutableArray# (SmallMutableArray# s (Strict a))

instance Contiguous.Contiguous StrictSmallArray where
  type Mutable StrictSmallArray = StrictSmallMutableArray
  type Element StrictSmallArray = Always
  type Sliced StrictSmallArray = Slice StrictSmallArray
  type MutableSliced (StrictSmallArray) = MutableSlice (StrictSmallArray)
  {-# INLINE new #-}
  new n = StrictSmallMutableArray <$> new n
  {-# INLINE replicateMut #-}
  replicateMut n x = StrictSmallMutableArray <$> replicateMut n (Strictly x)
  {-# INLINE shrink #-}
  shrink (StrictSmallMutableArray arr) n = StrictSmallMutableArray <$> shrink arr n
  {-# INLINE empty #-}
  empty = StrictSmallArray Contiguous.empty
  {-# INLINE singleton #-}
  singleton = StrictSmallArray . Contiguous.singleton . Strictly
  {-# INLINE doubleton #-}
  doubleton a b = StrictSmallArray $ doubleton (Strictly a) (Strictly b)
  {-# INLINE tripleton #-}
  tripleton a b c = StrictSmallArray $ tripleton (Strictly a) (Strictly b) (Strictly c)
  {-# INLINE quadrupleton #-}
  quadrupleton a b c d = StrictSmallArray $ quadrupleton (Strictly a) (Strictly b) (Strictly c) (Strictly d)
  {-# INLINE quintupleton #-}
  quintupleton a b c d e = StrictSmallArray $ quintupleton (Strictly a) (Strictly b) (Strictly c) (Strictly d) (Strictly e)
  {-# INLINE sextupleton #-}
  sextupleton a b c d e f = StrictSmallArray $ sextupleton (Strictly a) (Strictly b) (Strictly c) (Strictly d) (Strictly e) (Strictly f)
  {-# INLINE index #-}
  index (StrictSmallArray ary) idx = unStrictly $ index ary idx
  {-# INLINE index# #-}
  index# (StrictSmallArray ary) idx | (# v #) <- index# ary idx = (# unStrictly v #)
  {-# INLINE indexM #-}
  indexM (StrictSmallArray ary) idx = unStrictly <$> indexM ary idx
  {-# INLINE size #-}
  size (StrictSmallArray ary) = size ary
  {-# INLINE sizeMut #-}
  sizeMut (StrictSmallMutableArray ary) = sizeMut ary
  {-# INLINE equals #-}
  equals (StrictSmallArray lhs) (StrictSmallArray rhs) = equals lhs rhs
  {-# INLINE equalsMut #-}
  equalsMut (StrictSmallMutableArray lhs) (StrictSmallMutableArray rhs) = equalsMut lhs rhs
  {-# INLINE rnf #-}
  rnf (StrictSmallArray ary) = rnf ary
  {-# INLINE null #-}
  null (StrictSmallArray ary) = null ary
  {-# INLINE read #-}
  read (StrictSmallMutableArray ary) idx = unStrictly <$> read ary idx
  {-# INLINE write #-}
  write (StrictSmallMutableArray ary) idx x = write ary idx (Strictly x)
  {-# INLINE slice #-}
  slice base offset length = Slice {offset, length, base = unlift base}
  {-# INLINE sliceMut #-}
  sliceMut baseMut offsetMut lengthMut = MutableSlice {offsetMut, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE toSlice #-}
  toSlice base = Slice {offset = 0, length = size base, base = unlift base}
  {-# INLINE toSliceMut #-}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice {offsetMut = 0, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE clone_ #-}
  clone_ (StrictSmallArray ary) offset length = StrictSmallArray $ clone_ ary offset length
  {-# INLINE cloneMut_ #-}
  cloneMut_ (StrictSmallMutableArray ary) offset length = StrictSmallMutableArray <$> cloneMut_ ary offset length
  {-# INLINE copy_ #-}
  copy_ (StrictSmallMutableArray dst) dstOffset (StrictSmallArray src) srcOffset length = copy_ dst dstOffset src srcOffset length
  {-# INLINE copyMut_ #-}
  copyMut_ (StrictSmallMutableArray dst) dstOffset (StrictSmallMutableArray src) srcOffset length = copyMut_ dst dstOffset src srcOffset length
  {-# INLINE freeze_ #-}
  freeze_ (StrictSmallMutableArray ary) offset length = StrictSmallArray <$> freeze_ ary offset length
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (StrictSmallMutableArray ary) = StrictSmallArray <$> unsafeFreeze ary
  {-# INLINE unsafeShrinkAndFreeze #-}
  unsafeShrinkAndFreeze (StrictSmallMutableArray ary) length = StrictSmallArray <$> unsafeShrinkAndFreeze ary length
  {-# INLINE thaw_ #-}
  thaw_ (StrictSmallArray ary) offset length = StrictSmallMutableArray <$> thaw_ ary offset length
  run = runST -- NOTE: not relying on a manually-written run-st here as modern GHCs inline runST properly.

instance Contiguous.ContiguousU StrictSmallArray where
  type Unlifted StrictSmallArray = StrictSmallArray#
  type UnliftedMut StrictSmallArray = StrictSmallMutableArray#
  {-# INLINE resize #-}
  resize (StrictSmallMutableArray ary) length = StrictSmallMutableArray <$> resize ary length
  {-# INLINE unlift #-}
  unlift (StrictSmallArray (SmallUnliftedArray (SmallUnliftedArray# x))) = StrictSmallArray# x
  {-# INLINE unliftMut #-}
  unliftMut (StrictSmallMutableArray (SmallMutableUnliftedArray (SmallMutableUnliftedArray# x))) = StrictSmallMutableArray# x
  {-# INLINE lift #-}
  lift (StrictSmallArray# x) = StrictSmallArray (SmallUnliftedArray (SmallUnliftedArray# x))
  {-# INLINE liftMut #-}
  liftMut (StrictSmallMutableArray# x) = StrictSmallMutableArray (SmallMutableUnliftedArray (SmallMutableUnliftedArray# x))

instance (Eq a) => Eq (StrictSmallArray a) where
  (StrictSmallArray l) == (StrictSmallArray r) = l == r

-- We define our own newtype wrapper
-- since it is not possible to use primitive-unlifted's `SmallUnliftedArray`
-- in type families (which we need for `ArrayOf`)
-- as it has an extra free-floating `unlifted_a` type parameter.
newtype SmallUnliftedArray' a = SmallUnliftedArray' (SmallUnliftedArray_ (Unlifted.Unlifted a) a)
  deriving (Semigroup, Monoid)
newtype MutableSmallUnliftedArray' s a = MutableSmallUnliftedArray' (SmallMutableUnliftedArray_ (Unlifted.Unlifted a) s a)

newtype SmallUnliftedArray'# a = SmallUnliftedArray'# (SmallUnliftedArray# (Unlifted.Unlifted a))
newtype MutableSmallUnliftedArray'# s a = MutableSmallUnliftedArray'# (SmallMutableUnliftedArray# s (Unlifted.Unlifted a))

instance Contiguous.Contiguous SmallUnliftedArray' where
  type Element SmallUnliftedArray' = PrimUnlifted
  type Mutable SmallUnliftedArray' = MutableSmallUnliftedArray'
  type Sliced SmallUnliftedArray' = Slice SmallUnliftedArray'
  type MutableSliced SmallUnliftedArray' = MutableSlice SmallUnliftedArray'
  {-# INLINE new #-}
  new n = MutableSmallUnliftedArray' <$> new n
  {-# INLINE replicateMut #-}
  replicateMut n x = MutableSmallUnliftedArray' <$> replicateMut n x
  {-# INLINE shrink #-}
  shrink (MutableSmallUnliftedArray' arr) n = MutableSmallUnliftedArray' <$> shrink arr n
  {-# INLINE empty #-}
  empty = SmallUnliftedArray' Contiguous.empty
  {-# INLINE singleton #-}
  singleton = SmallUnliftedArray' . Contiguous.singleton
  {-# INLINE doubleton #-}
  doubleton a b = SmallUnliftedArray' $ doubleton a b
  {-# INLINE tripleton #-}
  tripleton a b c = SmallUnliftedArray' $ tripleton a b c
  {-# INLINE quadrupleton #-}
  quadrupleton a b c d = SmallUnliftedArray' $ quadrupleton a b c d
  {-# INLINE quintupleton #-}
  quintupleton a b c d e = SmallUnliftedArray' $ quintupleton a b c d e
  {-# INLINE sextupleton #-}
  sextupleton a b c d e f = SmallUnliftedArray' $ sextupleton a b c d e f
  {-# INLINE index #-}
  index (SmallUnliftedArray' ary) idx = index ary idx
  {-# INLINE index# #-}
  index# (SmallUnliftedArray' ary) idx | (# v #) <- index# ary idx = (# v #)
  {-# INLINE indexM #-}
  indexM (SmallUnliftedArray' ary) idx = indexM ary idx
  {-# INLINE size #-}
  size (SmallUnliftedArray' ary) = size ary
  {-# INLINE sizeMut #-}
  sizeMut (MutableSmallUnliftedArray' ary) = sizeMut ary
  {-# INLINE equals #-}
  equals (SmallUnliftedArray' lhs) (SmallUnliftedArray' rhs) = equals lhs rhs
  {-# INLINE equalsMut #-}
  equalsMut (MutableSmallUnliftedArray' lhs) (MutableSmallUnliftedArray' rhs) = equalsMut lhs rhs
  {-# INLINE rnf #-}
  rnf (SmallUnliftedArray' ary) = rnf ary
  {-# INLINE null #-}
  null (SmallUnliftedArray' ary) = null ary
  {-# INLINE read #-}
  read (MutableSmallUnliftedArray' ary) idx = read ary idx
  {-# INLINE write #-}
  write (MutableSmallUnliftedArray' ary) idx x = write ary idx x
  {-# INLINE slice #-}
  slice base offset length = Slice {offset, length, base = unlift base}
  {-# INLINE sliceMut #-}
  sliceMut baseMut offsetMut lengthMut = MutableSlice {offsetMut, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE toSlice #-}
  toSlice base = Slice {offset = 0, length = size base, base = unlift base}
  {-# INLINE toSliceMut #-}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice {offsetMut = 0, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE clone_ #-}
  clone_ (SmallUnliftedArray' ary) offset length = SmallUnliftedArray' $ clone_ ary offset length
  {-# INLINE cloneMut_ #-}
  cloneMut_ (MutableSmallUnliftedArray' ary) offset length = MutableSmallUnliftedArray' <$> cloneMut_ ary offset length
  {-# INLINE copy_ #-}
  copy_ (MutableSmallUnliftedArray' dst) dstOffset (SmallUnliftedArray' src) srcOffset length = copy_ dst dstOffset src srcOffset length
  {-# INLINE copyMut_ #-}
  copyMut_ (MutableSmallUnliftedArray' dst) dstOffset (MutableSmallUnliftedArray' src) srcOffset length = copyMut_ dst dstOffset src srcOffset length
  {-# INLINE freeze_ #-}
  freeze_ (MutableSmallUnliftedArray' ary) offset length = SmallUnliftedArray' <$> freeze_ ary offset length
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MutableSmallUnliftedArray' ary) = SmallUnliftedArray' <$> unsafeFreeze ary
  {-# INLINE unsafeShrinkAndFreeze #-}
  unsafeShrinkAndFreeze (MutableSmallUnliftedArray' ary) length = SmallUnliftedArray' <$> unsafeShrinkAndFreeze ary length
  {-# INLINE thaw_ #-}
  thaw_ (SmallUnliftedArray' ary) offset length = MutableSmallUnliftedArray' <$> thaw_ ary offset length
  run = runST -- NOTE: not relying on a manually-written run-st here as modern GHCs inline runST properly.


instance Contiguous.ContiguousU SmallUnliftedArray' where
  type Unlifted SmallUnliftedArray' = SmallUnliftedArray'#
  type UnliftedMut SmallUnliftedArray' = MutableSmallUnliftedArray'#
  {-# INLINE resize #-}
  resize (MutableSmallUnliftedArray' ary) length = MutableSmallUnliftedArray' <$> resize ary length
  {-# INLINE unlift #-}
  unlift (SmallUnliftedArray' (SmallUnliftedArray x)) = SmallUnliftedArray'# x
  {-# INLINE unliftMut #-}
  unliftMut (MutableSmallUnliftedArray' (SmallMutableUnliftedArray x)) = MutableSmallUnliftedArray'# x
  {-# INLINE lift #-}
  lift (SmallUnliftedArray'# x) = SmallUnliftedArray' (SmallUnliftedArray x)
  {-# INLINE liftMut #-}
  liftMut (MutableSmallUnliftedArray'# x) = MutableSmallUnliftedArray' (SmallMutableUnliftedArray x)


-- | Array containing any number of `()`'s.
--
-- The trick is that we can forget and later produce as many `()`'s as we like from thin air.
-- 
-- These only keep track of their length (as unboxed Int).
--
-- This could be generalized to store any singleton type instances (c.f. `singletons`' SingI class),
-- but we don't need that generality here.
--
-- For the purposes of Champ, where we are already tracking the lengths of the array members with a bitmap,
-- it may be possible to get rid of even this length field if we are very careful.
-- However, that would require re-implementing many higher-level functions such as `insertAt`, `deleteAt` and `convert`,
-- which was not worth it to do yet. (PRs welcome!)
data UnitArray (a :: Type) where 
  UnitArray :: {-# UNPACK #-} !Int -> UnitArray a
  deriving Show

data MutableUnitArray s (a :: Type) where 
  MutableUnitArray :: {-# UNPACK #-} !Int -> MutableUnitArray s a
  deriving Show

instance Semigroup (UnitArray a) where
  UnitArray l <> UnitArray r = UnitArray (l + r)

instance Monoid (UnitArray a) where
  mempty = UnitArray 0

data UnitArray# (a :: Type) :: UnliftedType where 
  UnitArray# :: {-# UNPACK #-} !Int -> UnitArray# a

data MutableUnitArray# s (a :: Type) :: UnliftedType where
  MutableUnitArray# :: {-# UNPACK #-} !Int -> MutableUnitArray# s a

-- | Helper class to make `UnitArray` work
class IsUnit a where
  produceUnit :: a

instance (a ~ ()) => IsUnit a where
  {-# INLINE produceUnit #-}
  produceUnit = ()

instance Contiguous.Contiguous UnitArray where
  type Mutable UnitArray = MutableUnitArray
  type Element UnitArray = IsUnit
  type Sliced UnitArray = Slice UnitArray
  type MutableSliced UnitArray = MutableSlice UnitArray
  {-# INLINE new #-}
  new l = pure $ MutableUnitArray l
  {-# INLINE replicateMut #-}
  replicateMut l _ = pure $ MutableUnitArray l
  {-# INLINE shrink #-}
  shrink _ l = pure $ MutableUnitArray l
  {-# INLINE empty #-}
  empty = UnitArray 0
  {-# INLINE singleton #-}
  singleton _ = UnitArray 1
  {-# INLINE doubleton #-}
  doubleton _ _ = UnitArray 2
  {-# INLINE tripleton #-}
  tripleton _ _ _ = UnitArray 3
  {-# INLINE quadrupleton #-}
  quadrupleton _ _ _ _ = UnitArray 4
  {-# INLINE quintupleton #-}
  quintupleton _ _ _ _ _ = UnitArray 5
  {-# INLINE sextupleton #-}
  sextupleton _ _ _ _ _ _ = UnitArray 6
  {-# INLINE index #-}
  index _ _ = produceUnit
  {-# INLINE index# #-}
  index# _ _ = (# produceUnit #)
  {-# INLINE indexM #-}
  indexM _ _ = pure produceUnit
  {-# INLINE size #-}
  size (UnitArray l) = l
  {-# INLINE sizeMut #-}
  sizeMut (MutableUnitArray l) = pure l
  {-# INLINE equals #-}
  equals (UnitArray l) (UnitArray m) = l == m
  {-# INLINE equalsMut #-}
  equalsMut (MutableUnitArray l) (MutableUnitArray m) = l == m
  {-# INLINE rnf #-}
  rnf !UnitArray{} = ()
  {-# INLINE null #-}
  null (UnitArray l) = l == 0
  {-# INLINE read #-}
  read MutableUnitArray{} _ = pure produceUnit
  {-# INLINE write #-}
  write MutableUnitArray{} _ _ = pure ()
  {-# INLINE slice #-}
  slice base offset length = Slice {offset, length, base = unlift base}
  {-# INLINE sliceMut #-}
  sliceMut baseMut offsetMut lengthMut = MutableSlice {offsetMut, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE toSlice #-}
  toSlice base = Slice {offset = 0, length = size base, base = unlift base}
  {-# INLINE toSliceMut #-}
  toSliceMut baseMut = do
    lengthMut <- sizeMut baseMut
    pure MutableSlice {offsetMut = 0, lengthMut, baseMut = unliftMut baseMut}
  {-# INLINE clone_ #-}
  clone_ UnitArray{} _ l = UnitArray l
  {-# INLINE cloneMut_ #-}
  cloneMut_ MutableUnitArray{} _ l = pure $ MutableUnitArray l
  {-# INLINE copy_ #-}
  copy_ _ _ _ _ _ = pure ()
  {-# INLINE copyMut_ #-}
  copyMut_ _ _ _ _ _ = pure ()
  {-# INLINE freeze_ #-}
  freeze_ (MutableUnitArray l) _ _ = pure $ UnitArray l
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MutableUnitArray l) = pure $ UnitArray l
  {-# INLINE unsafeShrinkAndFreeze #-}
  unsafeShrinkAndFreeze MutableUnitArray{} l = pure $ UnitArray l
  {-# INLINE thaw_ #-}
  thaw_ UnitArray{} _ l = pure $ MutableUnitArray l
  run = runST -- NOTE: not relying on a manually-written run-st here as modern GHCs inline runST properly.

instance Contiguous.ContiguousU UnitArray where
  type Unlifted UnitArray = UnitArray#
  type UnliftedMut UnitArray = MutableUnitArray#
  {-# INLINE resize #-}
  resize MutableUnitArray{} l = pure $ MutableUnitArray l
  {-# INLINE unlift #-}
  unlift (UnitArray l) = UnitArray# l
  {-# INLINE unliftMut #-}
  unliftMut (MutableUnitArray l) = MutableUnitArray# l
  {-# INLINE lift #-}
  lift (UnitArray# l) = UnitArray l
  {-# INLINE liftMut #-}
  liftMut (MutableUnitArray# l) = MutableUnitArray l

class Contiguous.ContiguousU arr => Array arr where
  unsafeThaw :: PrimMonad m => arr a -> m (Mutable arr (PrimState m) a)
  unsafeShrinkMut :: (PrimMonad m, Element arr a) => (Mutable arr (PrimState m) a) -> Int -> m (Mutable arr (PrimState m) a)

instance Array SmallArray where
  unsafeThaw = SmallArray.unsafeThawSmallArray
  unsafeShrinkMut a s = do 
    SmallArray.shrinkSmallMutableArray a s
    pure a

instance Array PrimArray where
  unsafeThaw = PrimArray.unsafeThawPrimArray
  unsafeShrinkMut a s = do 
    PrimArray.shrinkMutablePrimArray a s
    pure a

instance Array StrictSmallArray where
  unsafeThaw (StrictSmallArray sa) = StrictSmallMutableArray <$> unsafeThawSmallUnliftedArray sa
  unsafeShrinkMut (StrictSmallMutableArray sa) s = do
    shrinkSmallMutableUnliftedArray sa s
    pure (StrictSmallMutableArray sa)

instance Array SmallUnliftedArray' where
  unsafeThaw (SmallUnliftedArray' sa) = MutableSmallUnliftedArray' <$> unsafeThawSmallUnliftedArray sa
  unsafeShrinkMut (MutableSmallUnliftedArray' sa) s = do
    shrinkSmallMutableUnliftedArray sa s
    pure (MutableSmallUnliftedArray' sa)

instance Array UnitArray where
  unsafeThaw (UnitArray l) = pure $ MutableUnitArray l
  unsafeShrinkMut MutableUnitArray{} l  = pure $ MutableUnitArray l

-- sumStrictArray :: StrictSmallArray Int -> Int
-- sumStrictArray = Foldable.foldr' (+) 0

-- sumLazyArray :: SmallArray Int -> Int
-- sumLazyArray = Foldable.foldr' (+) 0

-- | Branchless pair-array creation:
-- If the int is '1', creates the array [a, b]
-- If the int is '0', creates the array [b, a]
--
-- Trick copied from Data.Hashmap
doubletonBranchless :: (Contiguous arr, Element arr a) => Safety -> Int -> a -> a -> arr a
{-# INLINE doubletonBranchless #-}
doubletonBranchless _ idx0Or1 a b = run $ do
  arr <- new 2
  write arr (1 - idx0Or1) a
  write arr idx0Or1 b
  unsafeFreeze arr
-- doubletonBranchless Safe idx0Or1 a b = run $ do
--   arr <- new 2
--   write arr (1 - idx0Or1) a
--   write arr idx0Or1 b
--   unsafeFreeze arr

-- doubletonBranchless Unsafe idx0Or1 a b = run $ do
--   arr <- new 32
--   write arr (1 - idx0Or1) a
--   write arr idx0Or1 b
--   unsafeShrinkAndFreeze arr 2

-- | Allow running certain operations
-- in either a 'Safe' (copy-on-write)
-- and 'Unsafe' (mutate in place)
-- mode
data Safety = Safe | Unsafe

insertAt :: (Array arr, Element arr a) => Safety -> arr a -> Int -> a -> arr a
{-# INLINE insertAt #-}
insertAt Safe src i x = Contiguous.insertAt src i x
insertAt Unsafe src i x = 
  -- The following is probably faster for PrimArray
  -- but is not (yet) for SmallArray
  -- i.e. it won't currently re-use space in an earlier shrunk SmallArray
  -- c.f. https://gitlab.haskell.org/ghc/ghc/-/issues/21266
  Contiguous.create $ do
  -- Debug.Trace.traceM $ "Using unsafe insertAt for " <> show (addrOf src)
  -- dst <- (\arr -> unsafeResizeMut arr (Contiguous.size src + 1) x) =<< unsafeThaw src
  dst <- unsafeThaw src
  let srcSize = Contiguous.size src
  dst' <- Contiguous.resize dst (srcSize + 1)
  newSize <- Contiguous.sizeMut dst'
  copyMut dst' (i + 1) (sliceMut dst' i (newSize - (i + 1)))
  Contiguous.write dst' i x
  pure dst'

replaceAt :: (Array arr, Element arr a) => Safety -> arr a -> Int -> a -> arr a
{-# INLINE replaceAt #-}
replaceAt safety src i x =
  let (# oldX #) = (Contiguous.index# src i)
  in if x `ptrEq` oldX then src
  else case safety of
    Safe -> Contiguous.replaceAt src i x
    Unsafe -> Contiguous.create $ do
      dst <- unsafeThaw src
      Contiguous.write dst i x
      pure dst

deleteAt :: (Array arr, Element arr a) => Safety -> arr a -> Int -> arr a 
{-# INLINE deleteAt #-}
deleteAt Safe src i = Contiguous.deleteAt src i
deleteAt Unsafe src i = Contiguous.create $ do
  let !len = Contiguous.size src
  let i' = i + 1
  dst <- unsafeThaw src
  copyMut dst i (sliceMut dst i' (len - i'))
  unsafeShrinkMut dst (len - 1)

singleton :: (Array arr, Element arr a) => Safety -> a -> arr a
singleton _ a = Contiguous.singleton a
-- singleton Safe a = Contiguous.singleton a
-- singleton Unsafe a = Contiguous.create $ do
--   dst <- Contiguous.replicateMut 32 a
--   unsafeShrinkMut dst 1

foldrZipWith' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (a -> b -> c -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
{-# INLINE foldrZipWith' #-}
foldrZipWith' f = ifoldrZipWith' (\_ x y c -> f x y c)

ifoldrZipWith' ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> a -> b -> c -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
ifoldrZipWith' f !z !arr1 !arr2 = go (sz - 1) z
  where
  !sz = min (Contiguous.size arr1) (Contiguous.size arr2)
  go !ix !acc =
    if ix == -1
      then acc
      else case Contiguous.index# arr1 ix of
        (# x #) -> case Contiguous.index# arr2 ix of
          (# y #) -> go (ix - 1) (f ix x y acc)
{-# INLINE ifoldrZipWith' #-}

foldlZipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (c -> a -> b -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
{-# INLINE foldlZipWith #-}
foldlZipWith f = ifoldlZipWith (\_ c x y -> f c x y)

ifoldlZipWith ::
  ( Contiguous arr1
  , Contiguous arr2
  , Element arr1 a
  , Element arr2 b
  ) =>
  (Int -> c -> a -> b -> c) ->
  c ->
  arr1 a ->
  arr2 b ->
  c
{-# INLINE ifoldlZipWith #-}
ifoldlZipWith f z arr1 arr2 = go (sz - 1)
  where 
    !sz = min (size arr1) (size arr2)
    go !i = 
      if i == -1
      then z
      else case index# arr1 i of
        (# x #) -> case index# arr2 i of
          (# y #) -> f i (go (i - 1)) x y

{- | 'findIndex' takes a predicate and an array, and returns the index of
  the leftmost element of the array matching the prediate, or an unboxed 'Nothing'
  if there is no such element.

  This implementation is taken from Contiguous
  but made to return an unboxed Maybe instead
-}
findIndex# ::
  (Contiguous arr, Element arr a) =>
  (a -> Bool) ->
  arr a ->
  (# (# #) | Int #)
findIndex# p xs = loop 0
 where
  loop i
    | i < size xs = if p (index xs i) then (# | i #) else loop (i + 1)
    | otherwise = (# (# #) | #)
{-# INLINE findIndex# #-}

-- | Slice from a source array into a destination array, specified using
-- indices. Returns whether it did anything. Does not do any bounds checking
-- relative to the source or destination arrays.
unsafeSliceInto
  :: (Element arr b, Element arr b,  Contiguous arr, Contiguous arr, PrimMonad m)
  => arr b
  -> Mutable arr (PrimState m) b
  -> Int
  -> Int
  -> Int
  -> m Bool
{-# INLINE unsafeSliceInto #-}
unsafeSliceInto src dst !srcFromIx !srcToIx !dstIx = do
  if srcToIx >= srcFromIx
    then do
      -- traceShowM ("slicing", srcFromIx, srcToIx, dstIx)
      let srcSlice = Contiguous.slice src srcFromIx (srcToIx - srcFromIx + 1)
      Contiguous.copy dst dstIx srcSlice
      pure True
    else pure False

-- | Because @'Contiguous.ifilter'@ and @'Contiguous.filter'@ are too slow, if I
-- already have a bitmask. There is no check that the mask does not match the
-- source array. Does the writing in slices.
--
--  input = [1, 2, 3, 4]
--  mask = 1011
--  output = [1, 3, 4]
filterUsingMask :: (Contiguous.Contiguous arr, Array arr, Element arr a, Bits bits) => arr a -> bits -> Int -> arr a
{-# INLINE filterUsingMask #-}
filterUsingMask src mask _hint | popCount mask == Contiguous.size src = src
filterUsingMask _src mask _hint | mask == zeroBits = Contiguous.empty
filterUsingMask src mask hint = Contiguous.create $ do
  dst <- Contiguous.new (popCount mask + hint)
  let
    go !srcFromIx !srcToIx !dstIx !m | m == zeroBits = pure (srcFromIx, srcToIx, dstIx)
    go !srcFromIx !srcToIx !dstIx !m = do
        if m `testBit` 0
          then go srcFromIx (srcToIx + 1) dstIx (m !>>. 1)
          else do
            didSlice <- unsafeSliceInto src dst srcFromIx srcToIx dstIx
            if didSlice
              then go (srcToIx + 2) (srcToIx + 1) (dstIx + (srcToIx - srcFromIx) + 1) (m !>>. 1)
              else go (srcFromIx + 1) srcFromIx dstIx (m !>>. 1)

  (!srcFromIx, !srcToIx, !dstIx) <- go 0 (-1) 0 mask
  _ <- unsafeSliceInto src dst srcFromIx srcToIx dstIx
  dst' <- Contiguous.resize dst (popCount mask)
  pure dst'

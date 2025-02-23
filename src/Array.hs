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

module Array (
  -- * Simple arrays:
  SmallArray, 
  SmallUnliftedArray, 
  SmallUnliftedArray_, 
  StrictSmallArray, 
  PrimArray, 
  -- * Dealing with strict values:
  Strictly (..), 
  -- * Array helper functions:
  doubletonBranchless, 
  sumStrictArray, 
  sumLazyArray,
  -- * Arrays to store zero-size values
  ZeroCostFakeArray,
  IsUnit,
) where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Coerce (coerce)
import Data.Elevator (Strict (Strict), UnliftedType)
import Data.Foldable qualified as Foldable
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Primitive (Prim, PrimArray, SmallArray)
import Data.Primitive.Contiguous
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Primitive.Contiguous.Class (Contiguous (..), ContiguousU (..), MutableSlice (..), Slice (..))
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.SmallArray (SmallMutableUnliftedArray_ (..), SmallUnliftedArray_ (..), mapSmallUnliftedArray)
import Data.Primitive.Unlifted.SmallArray.Primops (SmallMutableUnliftedArray# (SmallMutableUnliftedArray#), SmallUnliftedArray# (SmallUnliftedArray#))
import GHC.Exts (Levity (..), RuntimeRep (BoxedRep), SmallArray#, SmallMutableArray#, TYPE)
import GHC.Exts qualified as Exts
import Prelude hiding (foldl, foldl', foldr, foldr', length, null, read)

-- | Helper newtype to implement `PrimUnlifted` for any datatype
-- to turn it into a `Data.Elevator.Strict`
newtype Strictly a = Strictly {unStrictly :: a}
  deriving newtype (Show, Eq, Ord, Hashable, NFData)

instance PrimUnlifted (Strictly a) where
  type Unlifted (Strictly a) = Strict a
  toUnlifted# (Strictly a) = Strict a
  fromUnlifted# (Strict a) = (Strictly a)

-- | Array type whose elements are guaranteed to be in WHNF.
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
  empty = StrictSmallArray empty
  {-# INLINE singleton #-}
  singleton = StrictSmallArray . singleton . Strictly
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

sumStrictArray :: StrictSmallArray Int -> Int
sumStrictArray = foldr' (+) 0

sumLazyArray :: SmallArray Int -> Int
sumLazyArray = foldr' (+) 0

-- | Branchless pair-array creation:
-- If the int is '1', creates the array [a, b]
-- If the int is '0', creates the array [b, a]
--
-- Trick copied from Data.Hashmap
doubletonBranchless :: (Contiguous arr, Element arr a) => Int -> a -> a -> arr a
{-# INLINE doubletonBranchless #-}
doubletonBranchless idx0Or1 a b = run $ do
  arr <- new 2
  write arr (1 - idx0Or1) a
  write arr idx0Or1 b
  unsafeFreeze arr

-- modifyAtIfChanged :: (Contiguous arr, Element arr a) => (a -> a) -> arr a -> Int -> arr a
-- modifyAtIfChanged f arr idx =
--   let
--     !elem = Contiguous.index arr idx
--     !elem' = f elem
--   in if elem' `ptrEq` elem
--      then arr
--      else Contiguous.replaceAt arr idx elem'

------------------------------------------------------------------------
-- Pointer equality

-- | Check if two the two arguments are the same value.  N.B. This
-- function might give false negatives (due to GC moving objects, or things being unpacked/repacked.)
-- but never false positives
ptrEq :: a -> a -> Bool
ptrEq x y = Exts.isTrue# (Exts.reallyUnsafePtrEquality# x y Exts.==# 1#)
{-# INLINE ptrEq #-}


-- Array containing any number of `()`'s.
-- 
-- The trick is that we only need to keep track of such an array's length,
-- since we can produce as many `()`'s a we like from thin air.
--
-- This could be generalized to store any singleton type instances (c.f. `singletons`' SingI class),
-- but we don't need that generality here.
data ZeroCostFakeArray (a :: Type) = ZeroCostFakeArray {-# UNPACK #-} !Int
data MutableZeroCostFakeArray s (a :: Type) = MutableZeroCostFakeArray {-# UNPACK #-} !Int

data ZeroCostFakeArray# (a :: Type) :: UnliftedType where 
  ZeroCostFakeArray# :: {-# UNPACK #-} !Int -> ZeroCostFakeArray# a

data MutableZeroCostFakeArray# s (a :: Type) :: UnliftedType where
  MutableZeroCostFakeArray# :: {-# UNPACK #-} !Int -> MutableZeroCostFakeArray# s a

class IsUnit a where
  produceUnit :: a

instance (a ~ ()) => IsUnit a where
  produceUnit = ()

instance Contiguous.Contiguous ZeroCostFakeArray where
  type Mutable ZeroCostFakeArray = MutableZeroCostFakeArray
  type Element ZeroCostFakeArray = IsUnit
  type Sliced ZeroCostFakeArray = Slice ZeroCostFakeArray
  type MutableSliced ZeroCostFakeArray = MutableSlice ZeroCostFakeArray
  {-# INLINE new #-}
  new n = pure $ MutableZeroCostFakeArray n
  {-# INLINE replicateMut #-}
  replicateMut n _ = pure $ MutableZeroCostFakeArray n
  {-# INLINE shrink #-}
  shrink _ n = pure $ MutableZeroCostFakeArray n
  {-# INLINE empty #-}
  empty = ZeroCostFakeArray 0
  {-# INLINE singleton #-}
  singleton _ = ZeroCostFakeArray 1
  {-# INLINE doubleton #-}
  doubleton _ _ = ZeroCostFakeArray 2
  {-# INLINE tripleton #-}
  tripleton _ _ _ = ZeroCostFakeArray 3
  {-# INLINE quadrupleton #-}
  quadrupleton _ _ _ _ = ZeroCostFakeArray 4
  {-# INLINE quintupleton #-}
  quintupleton _ _ _ _ _ = ZeroCostFakeArray 5
  {-# INLINE sextupleton #-}
  sextupleton _ _ _ _ _ _ = ZeroCostFakeArray 5
  {-# INLINE index #-}
  index _ _ = produceUnit
  {-# INLINE index# #-}
  index# _ _ = (# produceUnit #)
  {-# INLINE indexM #-}
  indexM _ _ = pure produceUnit
  {-# INLINE size #-}
  size (ZeroCostFakeArray n) = n
  {-# INLINE sizeMut #-}
  sizeMut (MutableZeroCostFakeArray n) = pure n
  {-# INLINE equals #-}
  equals (ZeroCostFakeArray n) (ZeroCostFakeArray m) = n == m
  {-# INLINE equalsMut #-}
  equalsMut (MutableZeroCostFakeArray n) (MutableZeroCostFakeArray m) = n == m
  {-# INLINE rnf #-}
  rnf !(ZeroCostFakeArray _) = ()
  {-# INLINE null #-}
  null (ZeroCostFakeArray n) = n == 0
  {-# INLINE read #-}
  read (MutableZeroCostFakeArray _) _ = pure produceUnit
  {-# INLINE write #-}
  write (MutableZeroCostFakeArray _) _ _ = pure ()
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
  clone_ (ZeroCostFakeArray _) offset length = ZeroCostFakeArray $ length
  {-# INLINE cloneMut_ #-}
  cloneMut_ (MutableZeroCostFakeArray _) offset length = pure $ MutableZeroCostFakeArray length
  {-# INLINE copy_ #-}
  copy_ _ _ _ _ _ = pure ()
  {-# INLINE copyMut_ #-}
  copyMut_ _ _ _ _ _ = pure ()
  {-# INLINE freeze_ #-}
  freeze_ (MutableZeroCostFakeArray _) _ length = pure $ ZeroCostFakeArray length
  {-# INLINE unsafeFreeze #-}
  unsafeFreeze (MutableZeroCostFakeArray l) = pure $ ZeroCostFakeArray l
  {-# INLINE unsafeShrinkAndFreeze #-}
  unsafeShrinkAndFreeze (MutableZeroCostFakeArray _) length = pure $ ZeroCostFakeArray length
  {-# INLINE thaw_ #-}
  thaw_ (ZeroCostFakeArray _) _ length = pure $ MutableZeroCostFakeArray length
  run = runST -- NOTE: not relying on a manually-written run-st here as modern GHCs inline runST properly.

instance Contiguous.ContiguousU ZeroCostFakeArray where
  type Unlifted ZeroCostFakeArray = ZeroCostFakeArray#
  type UnliftedMut ZeroCostFakeArray = MutableZeroCostFakeArray#
  {-# INLINE resize #-}
  resize (MutableZeroCostFakeArray _) length = pure $ MutableZeroCostFakeArray length
  {-# INLINE unlift #-}
  unlift (ZeroCostFakeArray l) = ZeroCostFakeArray# l
  {-# INLINE unliftMut #-}
  unliftMut (MutableZeroCostFakeArray l) = MutableZeroCostFakeArray# l
  {-# INLINE lift #-}
  lift (ZeroCostFakeArray# l) = ZeroCostFakeArray l
  {-# INLINE liftMut #-}
  liftMut (MutableZeroCostFakeArray# l) = MutableZeroCostFakeArray l


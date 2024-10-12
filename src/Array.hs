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
{-# OPTIONS_GHC -ddump-simpl -ddump-cmm -ddump-stg-tags -ddump-stg-final -ddump-asm -ddump-to-file #-}

module Array (SmallArray, SmallUnliftedArray, SmallUnliftedArray_, StrictSmallArray, PrimArray, Strictly (..), doubletonBranchless, sumStrictArray, sumLazyArray) where

import Control.DeepSeq (NFData)
import Control.Monad.Primitive
import Control.Monad.ST (runST)
import Data.Coerce (coerce)
import Data.Elevator (Strict (Strict), UnliftedType)
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Data.Primitive (Prim, PrimArray, SmallArray)
import Data.Primitive.Contiguous
import Data.Primitive.Contiguous qualified as Contiguous
import Data.Primitive.Contiguous.Class (Contiguous (..), ContiguousU (..), MutableSlice (..), Slice (..))
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Primitive.Unlifted.Class (PrimUnlifted (..))
import Data.Primitive.Unlifted.SmallArray (SmallMutableUnliftedArray_ (..), SmallUnliftedArray_ (..))
import Data.Primitive.Unlifted.SmallArray.Primops (SmallMutableUnliftedArray# (SmallMutableUnliftedArray#), SmallUnliftedArray# (SmallUnliftedArray#))
import GHC.Exts (Levity (..), RuntimeRep (BoxedRep), SmallArray#, SmallMutableArray#, TYPE)
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

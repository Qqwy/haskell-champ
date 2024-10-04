{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
module Array where

import Data.Coerce (coerce)
import Data.Primitive (SmallArray, ByteArray)
import Data.Primitive.SmallArray qualified as SmallArray


-- | Backing store is a SmallArray,
-- but all reading/writing is strict in `a`
newtype StrictSmallArray a = StrictSmallArray (SmallArray a)

class Array arr where
  type Item arr
  empty :: arr
  size :: arr -> Int
  get :: arr -> Int -> (# Item arr #)
  set :: arr -> Int -> Item arr -> arr
  insert :: arr -> Int -> Item arr -> arr
  delete :: arr -> Int -> arr
  -- | Insert at the end. Optimization on insert.
  snoc :: arr -> Item arr -> arr

instance Array (SmallArray a) where
  type Item (SmallArray a) = a
  empty = SmallArray.emptySmallArray
  size = SmallArray.sizeofSmallArray
  get = SmallArray.indexSmallArray##
  set src idx item = SmallArray.runSmallArray $ do
    dst <- SmallArray.thawSmallArray src 0 (size src)
    SmallArray.writeSmallArray dst idx item
    pure dst
  insert src idx item = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src + 1) item
    SmallArray.copySmallArray dst 0 src 0 idx
    SmallArray.copySmallArray dst (idx + 1) src idx (size src - idx)
    pure dst
  snoc src item = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src + 1) item
    SmallArray.copySmallArray dst 0 src 0 (size src)
    pure dst
  delete src idx = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src - 1) undefined
    SmallArray.copySmallArray dst 0 src 0 idx
    SmallArray.copySmallArray dst idx src (idx + 1) (size src - idx - 1)
    pure dst

instance Array (StrictSmallArray a) where
    type Item (StrictSmallArray a) = a
    empty = StrictSmallArray $ empty
    size (StrictSmallArray ary) = size ary
    -- | Strict in v
    -- (But as v was in all likelihood set by set or insert before,
    -- it would have been evaluated then already)
    get (StrictSmallArray ary) idx = let (# !v #) = get ary idx in (# v #)
    -- | Strict in v
    set (StrictSmallArray ary) idx !item = StrictSmallArray $ set ary idx item
    -- | Strict in v
    insert (StrictSmallArray ary) idx !item = StrictSmallArray $ insert ary idx item
    -- | Strict in v
    snoc (StrictSmallArray ary) !item = StrictSmallArray $ snoc ary item
    delete (StrictSmallArray ary) idx = StrictSmallArray $ delete ary idx


foldl' :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl' f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i !z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> go ary n (i+1) (f z x)
{-# INLINE foldl' #-}

foldlP' :: (Array aryA, Array aryB) => (b -> Item aryA -> Item aryB -> b) -> b -> aryA -> aryB -> b
foldlP' f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryA0) 0 z0
  where
    go aryA aryB n i !z
        | i >= n = z
        | otherwise
        = case (# get aryA i, get aryB i #) of
            (# (# x #), (# y #) #) -> go aryA aryB n (i+1) (f z x y)
{-# INLINE foldlP' #-}


foldr' :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr' f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go !_ary (-1) z = z
    go !ary i !z
      | (# x #) <- get ary i
      = go ary (i - 1) (f x z)
{-# INLINE foldr' #-}

foldrP' :: (Array aryA, Array aryB) => (Item aryA -> Item aryB -> b -> b) -> b -> aryA -> aryB -> b
foldrP' f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryA0 - 1) z0
  where
    go !_aryA !_aryB (-1) z = z
    go !aryA !aryB i !z
      | (# x #) <- get aryA i
      , (# y #) <- get aryB i
      = go aryA aryB (i - 1) (f x y z)
{-# INLINE foldrP' #-}

foldr :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> f x (go ary n (i+1) z)
{-# INLINE foldr #-}

foldrP :: (Array aryA, Array aryB) => (Item aryA -> Item aryB -> b -> b) -> b -> aryA -> aryB -> b
foldrP f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryA0) 0 z0
  where
    go aryA aryB n i z
        | i >= n = z
        | otherwise
        = case (# get aryA i, get aryB i #) of
            (# (# x #), (# y #) #) -> f x y (go aryA aryB n (i+1) z)
{-# INLINE foldrP #-}

foldl :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go _ary (-1) z = z
    go ary i z
      | (# x #) <- get ary i
      = f (go ary (i - 1) z) x
{-# INLINE foldl #-}


foldlP :: (Array aryA, Array aryB) => (b -> Item aryA -> Item aryB -> b) -> b -> aryA -> aryB -> b
foldlP f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryA0 - 1) z0
  where
    go _aryA _aryB (-1) z = z
    go aryA aryB i z
      | (# x #) <- get aryA i
      , (# y #) <- get aryB i
      = f (go aryA aryB (i - 1) z) x y
{-# INLINE foldlP #-}

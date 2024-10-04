{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}
module Array where

import Data.Primitive (SmallArray, PrimArray, Prim)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Primitive.PrimArray qualified as PrimArray


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

  {-# INLINE empty #-}
  empty = SmallArray.emptySmallArray

  {-# INLINE size #-}
  size = SmallArray.sizeofSmallArray

  {-# INLINE get #-}
  get = SmallArray.indexSmallArray##

  {-# INLINE set #-}
  set src idx item = SmallArray.runSmallArray $ do
    dst <- SmallArray.thawSmallArray src 0 (size src)
    SmallArray.writeSmallArray dst idx item
    pure dst

  {-# INLINE insert #-}
  insert src idx item = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src + 1) item
    SmallArray.copySmallArray dst 0 src 0 idx
    SmallArray.copySmallArray dst (idx + 1) src idx (size src - idx)
    pure dst

  {-# INLINE snoc #-}
  snoc src item = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src + 1) item
    SmallArray.copySmallArray dst 0 src 0 (size src)
    pure dst

  {-# INLINE delete #-}
  delete src idx = SmallArray.runSmallArray $ do
    dst <- SmallArray.newSmallArray (size src - 1) undefined
    SmallArray.copySmallArray dst 0 src 0 idx
    SmallArray.copySmallArray dst idx src (idx + 1) (size src - idx - 1)
    pure dst

instance Array (StrictSmallArray a) where
    type Item (StrictSmallArray a) = a
    {-# INLINE empty #-}
    empty = StrictSmallArray $ empty
    {-# INLINE size #-}
    size (StrictSmallArray ary) = size ary
    -- | Strict in v
    -- (But as v was in all likelihood set by set or insert before,
    -- it would have been evaluated then already)
    {-# INLINE get #-}
    get (StrictSmallArray ary) idx = let (# !v #) = get ary idx in (# v #)
    -- | Strict in v
    {-# INLINE set #-}
    set (StrictSmallArray ary) idx !item = StrictSmallArray $ set ary idx item
    -- | Strict in v
    {-# INLINE insert #-}
    insert (StrictSmallArray ary) idx !item = StrictSmallArray $ insert ary idx item
    -- | Strict in v
    {-# INLINE snoc #-}
    snoc (StrictSmallArray ary) !item = StrictSmallArray $ snoc ary item
    {-# INLINE delete #-}
    delete (StrictSmallArray ary) idx = StrictSmallArray $ delete ary idx

instance (Prim a) => Array (PrimArray a) where
  type Item (PrimArray a) = a

  {-# INLINE empty #-}
  empty = PrimArray.emptyPrimArray

  {-# INLINE size #-}
  size = PrimArray.sizeofPrimArray

  {-# INLINE get #-}
  get ary idx = let v = PrimArray.indexPrimArray ary idx in (# v #)

  {-# INLINE set #-}
  set src idx item = PrimArray.runPrimArray $ do
    dst <- PrimArray.thawPrimArray src 0 (size src)
    PrimArray.writePrimArray dst idx item
    pure dst

  {-# INLINE insert #-}
  insert src idx item = PrimArray.runPrimArray $ do
    dst <- PrimArray.newPrimArray (size src + 1)
    PrimArray.copyPrimArray dst 0 src 0 idx
    PrimArray.writePrimArray dst idx item
    PrimArray.copyPrimArray dst (idx + 1) src idx (size src - idx)
    pure dst

  {-# INLINE snoc #-}
  snoc src item = PrimArray.runPrimArray $ do
    let idx = size src
    dst <- PrimArray.newPrimArray (size src + 1)
    PrimArray.copyPrimArray dst 0 src 0 idx
    PrimArray.writePrimArray dst idx item
    pure dst

  {-# INLINE delete #-}
  delete src idx = PrimArray.runPrimArray $ do
    dst <- PrimArray.newPrimArray (size src - 1)
    PrimArray.copyPrimArray dst 0 src 0 idx
    PrimArray.copyPrimArray dst idx src (idx + 1) (size src - idx - 1)
    pure dst

instance (Array a, Array b) => Array (a, b) where
    type Item (a, b) = (Item a, Item b)
    {-# INLINE empty #-}
    empty = (empty, empty)
    {-# INLINE size #-}
    size (a, _) = size a
    {-# INLINE get #-}
    get (!a, !b) idx =
        let (# x #) = get a idx
            (# y #) = get b idx
        in (# (x, y) #)
    {-# INLINE set #-}
    set (!a, !b) idx (x, y) = (set a idx x, set b idx y)
    {-# INLINE insert #-}
    insert (!a, !b) idx (x, y) = (insert a idx x, insert b idx y)
    {-# INLINE snoc #-}
    snoc (!a, !b) (x, y) = (snoc a x, snoc b y)
    {-# INLINE delete #-}
    delete (!a, !b) idx = (delete a idx, delete b idx)

data StrictPair a b = StrictPair !a !b

instance (Array a, Array b) => Array (StrictPair a b) where
    type Item (StrictPair a b) = StrictPair (Item a) (Item b)
    {-# INLINE empty #-}
    empty = StrictPair empty empty
    {-# INLINE size #-}
    size (StrictPair a _) = size a
    {-# INLINE get #-}
    get (StrictPair a b) idx =
        let (# x #) = get a idx
            (# y #) = get b idx
        in (# (StrictPair x y) #)
    {-# INLINE set #-}
    set (StrictPair a b) idx (StrictPair x y) = StrictPair (set a idx x) (set b idx y)
    {-# INLINE insert #-}
    insert (StrictPair a b) idx (StrictPair x y) = StrictPair (insert a idx x) (insert b idx y)
    {-# INLINE snoc #-}
    snoc (StrictPair a b) (StrictPair x y) = StrictPair (snoc a x) (snoc b y)
    {-# INLINE delete #-}
    delete (StrictPair a b) idx = StrictPair (delete a idx) (delete b idx)


foldl' :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl' f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i !z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> go ary n (i+1) (f z x)
{-# INLINE foldl' #-}

-- foldlP' :: (Array aryA, Array aryB) => (b -> Item aryA -> Item aryB -> b) -> b -> aryA -> aryB -> b
-- foldlP' f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryB0) 0 z0
--   where
--     go aryA aryB n i !z
--         | i >= n = z
--         | otherwise
--         = case (# get aryA i, get aryB i #) of
--             (# (# x #), (# y #) #) -> go aryA aryB n (i+1) (f z x y)
-- {-# INLINE foldlP' #-}

sum :: SmallArray Int -> SmallArray Int -> Int
sum !a !b = (Array.foldr' (\(StrictPair k v) acc -> k + v + acc) 0) $ (StrictPair a b)

-- sum2 :: SmallArray Int -> SmallArray Int -> Int
-- sum2 = Array.foldrP' (\k v acc -> k + v + acc) 0

foldr' :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr' f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go !_ary (-1) z = z
    go !ary i !z
      | (# x #) <- get ary i
      = go ary (i - 1) (f x z)
{-# INLINE foldr' #-}

-- foldrP' :: (Array aryA, Array aryB) => (Item aryA -> Item aryB -> b -> b) -> b -> aryA -> aryB -> b
-- foldrP' f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryB0 - 1) z0
--   where
--     go !_aryA !_aryB (-1) z = z
--     go !aryA !aryB i !z
--       | (# x #) <- get aryA i
--       , (# y #) <- get aryB i
--       = go aryA aryB (i - 1) (f x y z)
-- {-# INLINE foldrP' #-}

foldr :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> f x (go ary n (i+1) z)
{-# INLINE foldr #-}

-- foldrP :: (Array aryA, Array aryB) => (Item aryA -> Item aryB -> b -> b) -> b -> aryA -> aryB -> b
-- foldrP f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryB0) 0 z0
--   where
--     go aryA aryB n i z
--         | i >= n = z
--         | otherwise
--         = case (# get aryA i, get aryB i #) of
--             (# (# x #), (# y #) #) -> f x y (go aryA aryB n (i+1) z)
-- {-# INLINE foldrP #-}

foldl :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go _ary (-1) z = z
    go ary i z
      | (# x #) <- get ary i
      = f (go ary (i - 1) z) x
{-# INLINE foldl #-}


-- foldlP :: (Array aryA, Array aryB) => (b -> Item aryA -> Item aryB -> b) -> b -> aryA -> aryB -> b
-- foldlP f = \ z0 aryA0 aryB0 -> go aryA0 aryB0 (size aryB0 - 1) z0
--   where
--     go _aryA _aryB (-1) z = z
--     go aryA aryB i z
--       | (# x #) <- get aryA i
--       , (# y #) <- get aryB i
--       = f (go aryA aryB (i - 1) z) x y
-- {-# INLINE foldlP #-}

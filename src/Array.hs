{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-to-file #-}
module Array(SmallArray, StrictSmallArray, PrimArray, Array(..)) where

import Data.Primitive (SmallArray, PrimArray, Prim)
import Data.Primitive.SmallArray qualified as SmallArray
import Data.Primitive.PrimArray qualified as PrimArray
import Data.Coerce (coerce)

-- | An array behaving similar to SmallArray,
-- but all reading/writing of elements first evaluates that element to WHNF.
--
-- It would be nice if GHC had separate machinery to turn
-- values of kind `Type` (AKA `TYPE (BoxedRep Lifted))` 
-- into values of kind `UnliftedType` (AKA (`TYPE (BoxedRep Unlifted`)))
-- but as that doesn't currently exist,
-- this is implemented as a wrapper around the normal SmallArray
-- with strictness annotations added to the various accessor functions.
--
-- TODO: Maybe we can harness the `data-elevator`?
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

-- instance (Array a, Array b) => Array (a, b) where
--     type Item (a, b) = (Item a, Item b)
--     {-# INLINE empty #-}
--     empty = (empty, empty)
--     {-# INLINE size #-}
--     size (a, _) = size a
--     {-# INLINE get #-}
--     get (!a, !b) idx =
--         let (# x #) = get a idx
--             (# y #) = get b idx
--         in (# (x, y) #)
--     {-# INLINE set #-}
--     set (!a, !b) idx (x, y) = (set a idx x, set b idx y)
--     {-# INLINE insert #-}
--     insert (!a, !b) idx (x, y) = (insert a idx x, insert b idx y)
--     {-# INLINE snoc #-}
--     snoc (!a, !b) (x, y) = (snoc a x, snoc b y)
--     {-# INLINE delete #-}
--     delete (!a, !b) idx = (delete a idx, delete b idx)

data StrictPair a b = StrictPair !a !b

-- | A pair of arrays can also be considered an array
--
-- Note that for effiency reasons, we only consider _strict_
-- pairs of arrays.
--
-- The items stored in these arrays are _normal_ pairs;
-- the strictness of the elements actually depends
-- on the the strictness of `aryA` resp. `aryB`.
instance (Array aryA, Array aryB) => Array (StrictPair aryA aryB) where
    type Item (StrictPair aryA aryB) = (Item aryA, Item aryB)
    {-# INLINE empty #-}
    empty = StrictPair empty empty
    {-# INLINE size #-}
    size (StrictPair a _) = size a
    {-# INLINE get #-}
    get (StrictPair a b) idx =
        let (# x #) = get a idx
            (# y #) = get b idx
        in (# (x, y) #)
    {-# INLINE set #-}
    set (StrictPair a b) idx (x, y) = StrictPair (set a idx x) (set b idx y)
    {-# INLINE insert #-}
    insert (StrictPair a b) idx (x, y) = StrictPair (insert a idx x) (insert b idx y)
    {-# INLINE snoc #-}
    snoc (StrictPair a b) (x, y) = StrictPair (snoc a x) (snoc b y)
    {-# INLINE delete #-}
    delete (StrictPair a b) idx = StrictPair (delete a idx) (delete b idx)

newtype StrictTriple a b c = StrictTriple' (StrictPair a (StrictPair b c))

instance (Array aryA, Array aryB, Array aryC) => Array (StrictTriple aryA aryB aryC) where
    type Item (StrictTriple aryA aryB aryC) = (Item aryA, Item aryB, Item aryC)
    {-# INLINE empty #-}
    empty = coerce $ empty @(StrictPair aryA (StrictPair aryB aryC))
    {-# INLINE size #-}
    size = coerce $ size  @(StrictPair aryA (StrictPair aryB aryC))
    {-# INLINE get #-}
    get ary idx = 
        let (# (x, (y, z)) #) = (coerce (get @(StrictPair aryA (StrictPair aryB aryC))) ary idx) 
        in (# (x, y, z) #)
    {-# INLINE set #-}
    set ary idx (x, y, z) = coerce (set @(StrictPair aryA (StrictPair aryB aryC))) ary idx (x, (y ,z))
    {-# INLINE insert #-}
    insert ary idx (x, y, z) = coerce (insert @(StrictPair aryA (StrictPair aryB aryC))) ary idx (x, (y, z))
    {-# INLINE snoc #-}
    snoc ary (x, y, z) = coerce (snoc @(StrictPair aryA (StrictPair aryB aryC))) ary (x, (y, z))
    {-# INLINE delete #-}
    delete = coerce $ delete @(StrictPair aryA (StrictPair aryB aryC))

{-# INLINE StrictTriple #-}
pattern StrictTriple :: a -> b -> c -> StrictTriple a b c
pattern StrictTriple a b c = StrictTriple' (StrictPair a (StrictPair b c))

foldl' :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl' f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i !z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> go ary n (i+1) (f z x)
{-# INLINE foldl' #-}

sum :: SmallArray Int -> SmallArray Int -> Int
sum !a !b = (Array.foldr' (\(k, v) acc -> k + v + acc) 0) $ (StrictPair a b)

sum3 :: SmallArray Int -> SmallArray Int -> SmallArray Int -> Int
sum3 !a !b !c = (Array.foldr' (\(x, y, z) acc -> x + y + z + acc) 0) $ (StrictTriple a b c)

foldr' :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr' f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go !_ary (-1) z = z
    go !ary i !z
      | (# x #) <- get ary i
      = go ary (i - 1) (f x z)
{-# INLINE foldr' #-}

foldr :: Array ary => (Item ary -> b -> b) -> b -> ary -> b
foldr f = \ z0 ary0 -> go ary0 (size ary0) 0 z0
  where
    go ary n i z
        | i >= n = z
        | otherwise
        = case get ary i of
            (# x #) -> f x (go ary n (i+1) z)
{-# INLINE foldr #-}

foldl :: Array ary => (b -> Item ary -> b) -> b -> ary -> b
foldl f = \ z0 ary0 -> go ary0 (size ary0 - 1) z0
  where
    go _ary (-1) z = z
    go ary i z
      | (# x #) <- get ary i
      = f (go ary (i - 1) z) x
{-# INLINE foldl #-}

module Champ.Internal.Collision where

import Champ.Internal.Array (Array, Element)
import Champ.Internal.Util (ptrEq)
import Data.List  (sortBy)
import Data.Maybe (fromMaybe)
import Data.Primitive.Contiguous qualified as Contiguous

-- | Under the assumption that two lists have the same length,
-- will try to check whether they have the same elements.
--
-- We have to rely on something awful like this
-- in collision nodes because we don't have an `Ord` constraint.
--
-- This is worst-case O(nˆ2) but is a slight improvement over
-- null (as // bs)
--
-- This code is a straight copy of `Data.HashMap.Internal.List.isPermutationBy`
isPermutationBy :: (a -> b -> Bool) -> [a] -> [b] -> Bool
isPermutationBy f = go
  where
    f' = flip f

    go [] [] = True
    go (x : xs) (y : ys)
        | f x y         = go xs ys
        | otherwise     = fromMaybe False $ do
            xs' <- deleteBy f' y xs
            ys' <- deleteBy f x ys
            return (go xs' ys')
    go [] (_ : _) = False
    go (_ : _) [] = False


-- | Returns Nothing when nothing is deleted
-- This code is a straight copy of `Data.HashMap.Internal.List.deleteBy`
deleteBy              :: (a -> b -> Bool) -> a -> [b] -> Maybe [b]
deleteBy _  _ []      = Nothing
deleteBy eq x (y:ys)  = if x `eq` y then Just ys else fmap (y :) (deleteBy eq x ys)


-- The idea:
--
-- Homogenous version
--
-- uc :: (a -> a -> Ordering) -> [a] -> [a] -> Ordering
-- uc c as bs = compare (sortBy c as) (sortBy c bs)
--
-- But as we have only (a -> b -> Ordering), we cannot directly compare
-- elements from the same list.
--
-- So when comparing elements from the list, we count how many elements are
-- "less and greater" in the other list, and use the count as a metric.
--
-- This code is a straigyt copy of `Data.HashMap.Internal.List.unorderedCompare 
unorderedCompare :: (a -> b -> Ordering) -> [a] -> [b] -> Ordering
unorderedCompare c as bs = go (sortBy cmpA as) (sortBy cmpB bs)
  where
    go [] [] = EQ
    go [] (_ : _) = LT
    go (_ : _) [] = GT
    go (x : xs) (y : ys) = c x y <> go xs ys

    cmpA a a' = compare (inB a) (inB a')
    cmpB b b' = compare (inA b) (inA b')

    inB a = (length $ filter (\b -> c a b == GT) bs, negate $ length $ filter (\b -> c a b == LT) bs)
    inA b = (length $ filter (\a -> c a b == LT) as, negate $ length $ filter (\a -> c a b == GT) as)


findCollisionIndex :: (Eq a, Array arr, Element arr a) => a -> arr a -> Maybe Int
{-# INLINE findCollisionIndex #-}
findCollisionIndex k keys = Contiguous.findIndex (\existingKey -> existingKey `ptrEq` k || existingKey == k) keys

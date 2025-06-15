{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Champ.HashSet (
    -- ** Concrete types
    -- $concreteTypesComparison
    HashSetB,
    HashSetU,
    HashSetUl,
    -- ** Generic type
    HashSet(..),
    SetRepr,
    -- * Construction
    empty,
    singleton,

    -- * Combine
    union,
    unions,

    -- * Basic interface
    Champ.HashSet.null,
    size,
    member,
    insert,
    delete,
    lookup,
    isSubsetOf,

    -- * Transformations
    map,
    map',
    convert,

    -- * Difference and intersection
    difference,
    intersection,

    -- * Folds
    Champ.HashSet.foldr,
    Champ.HashSet.foldl',
    Champ.HashSet.foldr',
    Champ.HashSet.foldl,
    Champ.HashSet.foldMap,

    -- * Conversions
    -- ** Lists
    Champ.HashSet.fromList,
    Champ.HashSet.toList,

    -- ** HashMaps
    toMap,
    fromMap,
    keysSet,
    keysSet',
    -- * Generic type details
    Champ.Internal.Storage.Storage(Unexistent),
    Champ.Internal.Storage.StrictStorage(..),
) where

import Prelude hiding (map, foldr, lookup)
import Data.Foldable
import Champ.Internal qualified
import Champ.Internal.Storage (Storage(Unexistent), StrictStorage(..))
import Data.Hashable (Hashable)
import Data.List qualified
import Data.Coerce (coerce)
import GHC.IsList (IsList (..))


-- $setup
-- >>> :set -XOverloadedStrings
-- >>> :set -XOverloadedLists
-- >>> import Champ
-- >>> import Champ.HashMap
-- >>> import Data.Text.Short (ShortText)

-- $concreteTypesComparison
--
-- Champ comes with a whole set of type aliases for HashSets with different element types.
-- /(See "Champ.HashMap" for a similar set of type aliases for maps.)/

--
-- +---------------------------+----------------------------+
-- | Strict boxed elements (B) | t`Champ.HashSet.HashSetB`  |
-- +---------------------------+----------------------------+
-- | Unlifted elements (Ul)    | t`Champ.HashSet.HashSetUl` |
-- +---------------------------+----------------------------+
-- | Unboxed elements (U)      | t`Champ.HashSet.HashSetU`  | 
-- +---------------------------+----------------------------+
-- 
-- Since we need to hash an element on insertion,
-- a HashSet is never lazy in its elements.
--
--   [@B@]: t`Champ.HashSet.Strict` t`Champ.HashSet.Boxed`. Store any element after forcing it.
--     Implemented for all types. (No constraints).
--     We store the forced value using `UnliftedType`, which means that GHC will be able to skip the 'thunk check' when reading a value later.
--   [@Ul@]: t`Champ.HashSet.Strict` t`Champ.HashSet.Unlifted`. Store the internal primitive-but-boxed type in the array.
--     Requires the `PrimUnlifted` typeclass to be implemented. For types where this is possible, removes one layer of boxing compared to 'Strict boxed'.
--     Otherwise behaves the same.
--   [@U@]: t`Champ.HashSet.Strict` t`Champ.HashSet.Unboxed`. Store the internal unboxed primitive in the array.
--     Requires the `Prim` typeclass to be implemented. Greatly reduces the memory usage compared to @B@ or @L@, but otherwise behaves the same.

newtype HashSet elems e = HashSet { asMap :: Champ.Internal.HashMap elems Unexistent e () }
type role HashSet nominal nominal

type SetRepr elems e = Champ.Internal.MapRepr elems Unexistent e ()

-- | A HashSet with strict boxed elements
--
-- This is a drop-in replacement for `Data.HashSet`
type HashSetB e = HashSet Boxed e

-- | A HashSet with unboxed elements
--
-- This uses significantly less memory for the elements
-- than the boxed version
-- but requires the elements to implement `Prim`
type HashSetU e = HashSet Unboxed e

-- | A HashSet with unlifted elements
--
-- This skips 'thunk checks' for the elements,
-- but requires elements to implement `PrimUnlifted`.
type HashSetUl e = HashSet Unlifted e

instance (Show e, SetRepr elems e) => Show (HashSet elems e) where
    show set = "Champ.HashSet.fromList " <> show (Champ.HashSet.toList set)

instance (Hashable e, Eq e, SetRepr elems e) => IsList (HashSet elems e) where
  type Item (HashSet elems e) = e
  {-# INLINE toList #-}
  toList = Champ.HashSet.toList
  {-# INLINE fromList #-}
  fromList = Champ.HashSet.fromList

instance (Eq e, SetRepr elems e) => Eq (HashSet elems e) where
    {-# INLINE (==) #-}
    (HashSet a) == (HashSet b) = a == b

insert :: (Hashable e, SetRepr elems e) => e -> HashSet elems e -> HashSet elems e
{-# INLINE insert #-}
insert e = coerce Champ.Internal.insert e ()

fromList :: (Hashable e, SetRepr elems e) => [e] -> HashSet elems e
{-# INLINE fromList #-}
fromList = coerce Champ.Internal.fromList . fmap (\x -> (x, ()))

toList :: (SetRepr elems e) => HashSet elems e -> [e]
{-# INLINE toList #-}
toList = Champ.Internal.keys . coerce

-- | \O(1)\ Construct the empty set
empty :: (SetRepr elems e) => HashSet elems e
{-# INLINE empty #-}
empty = coerce Champ.Internal.empty

-- | \O(1)\ Return `True` if the set is empty, `False` otherwise
null :: (SetRepr elems e) => HashSet elems e -> Bool
{-# INLINE null #-}
null = coerce Champ.Internal.null


-- | \O(1)\ Return the number of elements in this set.
--
-- Since the set actually keeps track of the size while elements are inserted,
-- this function runs in constant-time.
size :: (SetRepr elems e) => HashSet elems e -> Int
{-# INLINE size #-}
size = coerce Champ.Internal.size

-- | \O(1)\ Construct a one-element set
singleton :: (Hashable e, SetRepr elems e) => e -> HashSet elems e
{-# INLINE singleton #-}
singleton e = coerce Champ.Internal.singleton e ()

-- | \O(log n)\ Return `True` if the specified element is present in the set, `False` otherwise.
member :: (SetRepr elems e, Hashable e) => e -> HashSet elems e -> Bool
{-# INLINE member #-}
member = coerce Champ.Internal.member

-- | Look up whether a given element `e` exists in the set.
--
-- This is similar to `member`,
-- but it can be useful to swap out an element that is _equal_
-- with an element that is _identical_ (pointer-equality).
lookup :: (SetRepr elems e, Hashable e) => e -> HashSet elems e -> Maybe e
{-# INLINE lookup #-}
lookup e set = fst <$> Champ.Internal.lookupKV e (coerce set)

-- | \(O(n)\) Transform this set by applying a function to every value.
-- The resulting set may be smaller than the source.
--
-- >>> Champ.HashSet.map show (Champ.HashSet.fromList [1,2,3] :: HashSetB Int)
-- Champ.HashSet.fromList ["3","1","2"]
--
-- >>> Champ.HashSet.map (`mod` 3) (Champ.HashSet.fromList [1,2,4,5] :: HashSetU Int)
-- Champ.HashSet.fromList [1,2]
map :: (Hashable b, SetRepr elems a, SetRepr elems b) => (a -> b) -> HashSet elems a -> HashSet elems b
{-# INLINE map #-}
map = map'

-- | More flexible version of `map` that allows changing the storage mechanism of the result.
map' :: (Hashable b, SetRepr as a, SetRepr bs b) => (a -> b) -> HashSet as a -> HashSet bs b
{-# INLINE map' #-}
map' fun = Champ.HashSet.fromList . Data.List.map fun . Champ.HashSet.toList

-- | \(O(n)\) Lazy right-fold
--
-- Iteration order is unspecified, but is the opposite from `foldl`
foldr :: (SetRepr elems e) => (e -> r -> r) -> r -> HashSet elems e -> r
{-# INLINE foldr #-}
foldr fun acc set = Champ.Internal.foldrWithKey (\e () r -> fun e r) acc (coerce set)

-- | \(O(n)\) Strict right-fold
--
-- Iteration order is unspecified, but is the opposite from `foldl'`
foldr' :: (SetRepr elems e) => (e -> r -> r) -> r -> HashSet elems e -> r
{-# INLINE foldr' #-}
foldr' fun acc set = Champ.Internal.foldrWithKey' (\e () r -> fun e r) acc (coerce set)

-- | \(O(n)\) Lazy left-fold
--
-- Iteration order is unspecified, but is the opposite from `foldr`
foldl :: (SetRepr elems e) => (r -> e -> r) -> r -> HashSet elems e -> r
{-# INLINE foldl #-}
foldl fun acc set = Champ.Internal.foldlWithKey (\r e () -> fun r e) acc (coerce set)

-- | \(O(n)\) Strict left-fold
--
-- Iteration order is unspecified, but is the opposite from `foldr'`
foldl' :: (SetRepr elems e) => (r -> e -> r) -> r -> HashSet elems e -> r
{-# INLINE foldl' #-}
foldl' fun acc set = Champ.Internal.foldlWithKey' (\r e () -> fun r e) acc (coerce set)

-- | \(O(n)\) Reduce the set by applying a function to each element
-- and combining the results with a monoid operation.
--
-- Iteration order is unspecified
foldMap :: (Monoid m, SetRepr elems e) => (e -> m) -> HashSet elems e -> m
{-# INLINE foldMap #-}
foldMap f set = Champ.Internal.foldMapWithKey (\e () -> f e) (coerce set)

-- | \(O(n + m)\) The union of two sets. If a key occurs in both sets, the
-- element from the first will be the element in the result.
--
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
union :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> HashSet elems e
{-# INLINE union #-}
union = coerce Champ.Internal.union

-- | The union of a list (or other foldable) of sets.
--
-- O((n * m) * log32(n * m)) for @n@ maps each having at most @m@ keys.
-- 
-- The current implementation is simple but not the most performant;
-- performing repeated insertion
unions :: (Foldable f, Hashable e, SetRepr elems e) => f (HashSet elems e) -> HashSet elems e
{-# INLINE unions #-}
unions = coerce . Champ.Internal.unions . fmap coerce . Data.Foldable.toList

delete :: (Hashable e, SetRepr elems e) => e -> HashSet elems e -> HashSet elems e
{-# INLINE delete #-}
delete = coerce Champ.Internal.delete

-- | \(O(n \log m)\) Difference of two sets. Return elements of the first set
-- not existing in the second.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one set over the other instead of walking over the two sets in lock-step.
difference :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> HashSet elems e
{-# INLINE difference #-}
difference = coerce Champ.Internal.difference

-- | \(O(n \log m)\) Intersection of two sets. Return elements of the first set
-- also existing in the second.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one map over the other instead of walking over the two maps in lock-step.
intersection :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> HashSet elems e
{-# INLINE intersection #-}
intersection = coerce Champ.Internal.intersection

-- | Set inclusion.
--
-- The current implementation is very simple but not the most performant,
-- as we fold one set over the other instead of walking over the two sets in lock-step.
isSubsetOf :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> Bool
{-# INLINE isSubsetOf #-}
isSubsetOf = coerce Champ.Internal.isSubmapOf

-- TODO: Implement the other foldXWithKey's as well,
-- so we can wrap them here

instance Foldable (HashSet Boxed) where
    {-# INLINE foldr #-}
    foldr = Champ.HashSet.foldr
    {-# INLINE foldr' #-}
    foldr' = Champ.HashSet.foldr'
    {-# INLINE foldl #-}
    foldl = Champ.HashSet.foldl
    {-# INLINE foldl' #-}
    foldl' = Champ.HashSet.foldl'
    {-# INLINE foldMap #-}
    foldMap = Champ.HashSet.foldMap
    {-# INLINE null #-}
    null = Champ.HashSet.null
    {-# INLINE length #-}
    length = Champ.HashSet.size

convert :: forall s1 s2 {es} {es'} {e}. (s1 ~ HashSet es, s2 ~ HashSet es', SetRepr es e, SetRepr es' e) => HashSet es e -> HashSet es' e
convert = coerce Champ.Internal.convert


-- | \(O(1)\) Convert to set to the equivalent 'HashMap' with @()@ values.
--
-- >>> Champ.HashSet.toMap (Champ.HashSet.singleton 1 :: HashSetB Int)
-- Champ.HashMap.fromList [(1,())]
toMap :: HashSet elems e -> Champ.Internal.HashMap elems Unexistent e ()
{-# INLINE toMap #-}
toMap = asMap

-- | \(O(1)\) Convert from the equivalent 'HashMap' with @()@ values.
--
-- >>> Champ.HashSet.fromMap (Champ.HashMap.singleton 1 ()) :: HashSetU Int
-- Champ.HashSet.fromList [1]
fromMap :: Champ.Internal.HashMap elems Unexistent e () -> HashSet elems e
{-# INLINE fromMap #-}
fromMap = HashSet

-- | \(O(n)\) Produce a `HashSet` of all the keys in the given `HashMap`.
--
-- >>> Champ.HashSet.keysSet (Champ.HashMap.fromList [(1, "a"), (2, "b")] :: HashMapBB Int ShortText)
-- Champ.HashSet.fromList [1,2]
keysSet :: (Champ.Internal.MapRepr keys vals k v, SetRepr keys k) => Champ.Internal.HashMap keys vals k v -> HashSet keys k
{-# INLINE keysSet #-}
keysSet = keysSet'

-- | Like `keysSet` but allows switching the storage mechanism of the keys (that become the set's elements).
keysSet' :: (Champ.Internal.MapRepr keys vals k v, SetRepr keys' k) => Champ.Internal.HashMap keys vals k v -> HashSet keys' k
{-# INLINE keysSet' #-}
keysSet' = HashSet . Champ.Internal.convertDropVals

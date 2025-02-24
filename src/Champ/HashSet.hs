{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
module Champ.HashSet (
    HashSet(..),
    HashSetB,
    HashSetU,
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
    -- TODO isSubsetOf

    -- * Transformations
    map,
    map',
    convert,

    -- * Difference and intersection
    difference,
    -- TODO intersection

    -- * Folds
    Champ.HashSet.foldr,
    Champ.HashSet.foldl',
    Champ.HashSet.foldr',
    -- TODO foldl
    Champ.HashSet.foldMap,

    -- * Conversions
    -- ** Lists
    Champ.HashSet.fromList,
    Champ.HashSet.toList,

    -- ** HashMaps
    toMap,
    fromMap,
    keysSet,
    keysSet'
) where

import Prelude hiding (map, foldr, lookup)
import Data.Foldable
import Champ.Internal qualified
import Champ.Internal.Storage (Storage(Unexistent), StrictStorage(..))
import Data.Hashable (Hashable)
import Data.List qualified
import Data.Coerce (coerce)
import GHC.IsList (IsList (..))

newtype HashSet elems e = HashSet { asMap :: Champ.Internal.HashMap elems Unexistent e () }
type role HashSet nominal nominal

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

type SetRepr elems e = Champ.Internal.MapRepr elems Unexistent e ()
type HashSetB e = HashSet Boxed e
type HashSetU e = HashSet Unboxed e

insert :: (Hashable e, SetRepr elems e) => e -> HashSet elems e -> HashSet elems e
{-# INLINE insert #-}
insert e = coerce Champ.Internal.insert e ()

fromList :: (Hashable e, SetRepr elems e) => [e] -> HashSet elems e
{-# INLINE fromList #-}
fromList = coerce Champ.Internal.fromList . fmap (\x -> (x, ()))

toList :: (SetRepr elems e) => HashSet elems e -> [e]
{-# INLINE toList #-}
toList = Champ.Internal.keys . coerce

empty :: (SetRepr elems e) => HashSet elems e
{-# INLINE empty #-}
empty = coerce Champ.Internal.empty

null :: (SetRepr elems e) => HashSet elems e -> Bool
{-# INLINE null #-}
null = coerce Champ.Internal.null

size :: (SetRepr elems e) => HashSet elems e -> Int
{-# INLINE size #-}
size = coerce Champ.Internal.size

singleton :: (SetRepr elems e) => e -> HashSet elems e
{-# INLINE singleton #-}
singleton e = coerce Champ.Internal.singleton e ()

member :: (SetRepr elems e, Hashable e) => e -> HashSet elems e -> Bool
{-# INLINE member #-}
member = coerce Champ.Internal.member

lookup :: (SetRepr elems e, Hashable e) => e -> HashSet elems e -> Maybe e
{-# INLINE lookup #-}
lookup e set = fst <$> Champ.Internal.lookupKV e (coerce set)

-- | \(O(n)\) Transform this set by applying a function to every value.
-- The resulting set may be smaller than the source.
--
-- >>> HashSet.map show (HashSet.fromList [1,2,3])
-- HashSet.fromList ["1","2","3"]
--
-- >>> HashSet.map (`mod` 2) (HashSet.fromList [1,2,4,5])
-- HashSet.fromList [0, 1]
map :: (Hashable b, SetRepr elems a, SetRepr elems b) => (a -> b) -> HashSet elems a -> HashSet elems b
{-# INLINE map #-}
map = map'

-- | More flexible version of `map` that allows changing the storage mechanism of the result.
map' :: (Hashable b, SetRepr as a, SetRepr bs b) => (a -> b) -> HashSet as a -> HashSet bs b
{-# INLINE map' #-}
map' fun = Champ.HashSet.fromList . Data.List.map fun . Champ.HashSet.toList

foldr :: (SetRepr elems e) => (e -> r -> r) -> r -> HashSet elems e -> r
{-# INLINE foldr #-}
foldr fun acc set = Champ.Internal.foldrWithKey (\e () r -> fun e r) acc (coerce set)

foldr' :: (SetRepr elems e) => (e -> r -> r) -> r -> HashSet elems e -> r
{-# INLINE foldr' #-}
foldr' fun acc set = Champ.Internal.foldrWithKey' (\e () r -> fun e r) acc (coerce set)

foldl :: (SetRepr elems e) => (r -> e -> r) -> r -> HashSet elems e -> r
{-# INLINE foldl #-}
foldl fun acc set = Champ.Internal.foldlWithKey (\r e () -> fun r e) acc (coerce set)

foldl' :: (SetRepr elems e) => (r -> e -> r) -> r -> HashSet elems e -> r
{-# INLINE foldl' #-}
foldl' fun acc set = Champ.Internal.foldlWithKey' (\r e () -> fun r e) acc (coerce set)

foldMap :: (Monoid m, SetRepr elems e) => (e -> m) -> HashSet elems e -> m
{-# INLINE foldMap #-}
foldMap f set = Champ.Internal.foldMapWithKey (\e () -> f e) (coerce set)

union :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> HashSet elems e
union = coerce Champ.Internal.union

unions :: (Hashable e, SetRepr elems e) => [HashSet elems e] -> HashSet elems e
unions = coerce Champ.Internal.unions

delete :: (Hashable e, SetRepr elems e) => e -> HashSet elems e -> HashSet elems e
delete = coerce Champ.Internal.delete

difference :: (Hashable e, SetRepr elems e) => HashSet elems e -> HashSet elems e -> HashSet elems e
difference = coerce Champ.Internal.difference

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
-- >>> HashSet.toMap (HashSet.singleton 1)
-- Champ.HashMap.fromList [(1,())]
toMap :: HashSet elems e -> Champ.Internal.HashMap elems Unexistent e ()
toMap = asMap

-- | \(O(1)\) Convert from the equivalent 'HashMap' with @()@ values.
--
-- >>> HashSet.fromMap (HashMap.singleton 1 ())
fromMap :: Champ.Internal.HashMap elems Unexistent e () -> HashSet elems e
fromMap = HashSet

-- | \(O(n)\) Produce a `HashSet` of all the keys in the given `HashMap`.
--
-- >>> HashSet.keysSet (HashMap.fromList [(1, "a"), (2, "b")]
-- Champ.HashSet.fromList [1,2]
keysSet :: (Champ.Internal.MapRepr keys vals k v, SetRepr keys k) => Champ.Internal.HashMap keys vals k v -> HashSet keys k
keysSet = keysSet'

-- | Like `keysSet` but allows switching the storage mechanism of the keys (that become the set's elements).
keysSet' :: (Champ.Internal.MapRepr keys vals k v, SetRepr keys' k) => Champ.Internal.HashMap keys vals k v -> HashSet keys' k
keysSet' = HashSet . Champ.Internal.convertDropVals

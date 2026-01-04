module Champ.HashMap (
    -- * Data types
    -- ** Concrete types
    -- $concreteTypesComparison
    Champ.Internal.HashMapBL,
    Champ.Internal.HashMapBB,
    Champ.Internal.HashMapBU,
    Champ.Internal.HashMapBUl,
    Champ.Internal.HashMapUL,
    Champ.Internal.HashMapUB,
    Champ.Internal.HashMapUU,
    Champ.Internal.HashMapUUl,
    Champ.Internal.HashMapUlL,
    Champ.Internal.HashMapUlB,
    Champ.Internal.HashMapUlU,
    Champ.Internal.HashMapUlUl,
    -- ** Generic type
    Champ.Internal.HashMap,
    -- * Construction
    Champ.Internal.empty,
    Champ.Internal.singleton,
    -- * Basic interface
    Champ.Internal.null,
    Champ.Internal.size,
    Champ.Internal.member,
    Champ.Internal.lookup,
    Champ.Internal.lookupKV,
    (!?),
    (!),
    findWithDefault,
    Champ.Internal.insert,
    Champ.Internal.insertWith,
    Champ.Internal.delete,
    Champ.Internal.adjust,
    Champ.Internal.update,
    Champ.Internal.alter,
    Champ.Internal.alterF,
    Champ.Internal.isSubmapOf,
    Champ.Internal.isSubmapOfBy,
    -- * Combine
    -- ** Union
    Champ.Internal.union,
    Champ.Internal.unionWith,
    Champ.Internal.unionWithKey,
    Champ.Internal.unions,
    -- ** Compose
    Champ.Internal.compose,
    -- * Transformations
    Champ.Internal.map,
    Champ.Internal.map',
    Champ.Internal.mapWithKey,
    Champ.Internal.mapWithKey',
    Champ.Internal.mapKeys,
    Champ.Internal.mapKeys',
    Champ.Internal.traverse,
    Champ.Internal.traverse',
    Champ.Internal.traverseWithKey,
    Champ.Internal.traverseWithKey',

    -- * Difference and intersection
    Champ.Internal.difference,
    Champ.Internal.differenceWith,
    Champ.Internal.intersection,
    Champ.Internal.intersectionWith,
    Champ.Internal.intersectionWithKey,

    -- * Folds
    Champ.Internal.foldMapWithKey,
    Champ.Internal.foldr,
    Champ.Internal.foldr',
    Champ.Internal.foldl,
    Champ.Internal.foldl',
    Champ.Internal.foldrWithKey,
    Champ.Internal.foldrWithKey',
    Champ.Internal.foldlWithKey,
    Champ.Internal.foldlWithKey',

    -- * Filter
    Champ.Internal.filter,
    Champ.Internal.filterWithKey,
    Champ.Internal.mapMaybe,
    Champ.Internal.mapMaybe',
    Champ.Internal.mapMaybeWithKey,
    Champ.Internal.mapMaybeWithKey',

    -- * Conversions
    -- ** Between map types
    Champ.Internal.convert,

    -- *** Coercions
    Champ.Internal.coerce,
    Champ.Internal.coercion,

    -- ** Lists
    Champ.Internal.fromList,
    Champ.Internal.toList,
    Champ.Internal.keys,
    Champ.Internal.elems,
    Champ.Internal.fromListWith,
    Champ.Internal.fromListWithKey,

    -- ** HashSets
    Champ.HashSet.keysSet,
    Champ.HashSet.keysSet',
    -- * Generic type details
    Champ.Internal.Storage.Storage(..),
    Champ.Internal.Storage.StrictStorage(..),
    Champ.Internal.MapRepr
) where

import GHC.Stack (HasCallStack)
import Champ.Internal (HashMap, MapRepr)
import Champ.Internal qualified
import Champ.Internal.Storage qualified
import Champ.HashSet qualified
import Data.Hashable (Hashable)

-- $concreteTypesComparison
-- #concreteTypesComparison#
--
-- Champ comes with a whole set of type aliases for HashMaps with different key and value types.
--
-- It is recommended to:
--
-- - Use one of these concrete types in your code. This will allow GHC to create an optimized implementation.
-- - If not possible or youŕe working on highly generic code, use the generic t`HashMap` type, and add @INLINABLE@ or @SPECIALIZE@ pragmas
--   to still give GHC the best opportunity to generate efficient code.
--
-- /(See "Champ.HashSet" for a similar set of type aliases for sets.)/
--
-- +--------------------------+-----------------------------+-----------------------------+---------------------------------+-----------------------------------+
-- |                          | Lazy boxed values (L)       | Strict boxed values (B)     | Unlifted values (Ul)            | Unboxed values (U)                |
-- +==========================+=============================+=============================+=================================+===================================+
-- | Strict boxed keys (B)    | t`Champ.HashMap.HashMapBL`  | t`Champ.HashMap.HashMapBB`  | t`Champ.HashMap.HashMapBUl`     | t`Champ.HashMap.HashMapBU`        |
-- +--------------------------+-----------------------------+-----------------------------+---------------------------------+-----------------------------------+
-- | Unlifted keys (Ul)       | t`Champ.HashMap.HashMapUlL` | t`Champ.HashMap.HashMapUlB` | t`Champ.HashMap.HashMapUlUl`    | t`Champ.HashMap.HashMapUlU`       |
-- +--------------------------+-----------------------------+-----------------------------+---------------------------------+-----------------------------------+
-- | Unboxed keys (U)         | t`Champ.HashMap.HashMapUL`  | t`Champ.HashMap.HashMapUB`  | t`Champ.HashMap.HashMapUUl`     | t`Champ.HashMap.HashMapUU`        |
-- +--------------------------+-----------------------------+-----------------------------+---------------------------------+-----------------------------------+
--
--   [@L@]: t`Champ.HashMap.Lazy` (always boxed). Store any value without forcing it. 
--     Implemented for all types. (No constraints). Not available for keys of the hashmap, since insertion requires evaluating the key to hash it.
--   [@B@]: t`Champ.HashMap.Strict` t`Champ.HashMap.Boxed`. Store any value after forcing it.
--     Implemented for all types. (No constraints).
--     We store the forced value using `UnliftedType`, which means that GHC will be able to skip the 'thunk check' when reading a value later.
--   [@Ul@]: t`Champ.HashMap.Strict` t`Champ.HashMap.Unlifted`. Store the internal primitive-but-boxed type in the array.
--     Requires the `PrimUnlifted` typeclass to be implemented. For types where this is possible, removes one layer of boxing compared to 'Strict boxed'.
--     Otherwise behaves the same.
--   [@U@]: t`Champ.HashMap.Strict` t`Champ.HashMap.Unboxed`. Store the internal unboxed primitive in the array.
--     Requires the `Prim` typeclass to be implemented. Greatly reduces the memory usage compared to @B@ or @L@, but otherwise behaves the same.
--
-- Roughly speaking, for types like `Int` or `Word` or `Bool`, @Unboxed@ is the best implementation to choose.
-- For types like `Text` or `ByteString`, strongly consider whether you can use `ShortText`/`ShortByteString` as these implement @Unlifed@.
-- If not, use @Strict Boxed@. Use @Lazy Boxed@ iff you really require the laziness for your values (all common caveats with space leaks apply here).



(!?) :: (Hashable k, MapRepr keys vals k v) => HashMap keys vals k v -> k -> Maybe v
(!?) m k = Champ.Internal.lookup k m
{-# INLINE (!?) #-}

-- | \(O(\log n)\) Return the value to which the specified key is mapped.
-- Calls 'error' if this map contains no mapping for the key.
(!) :: (HasCallStack, Hashable k, MapRepr keys vals k v) => HashMap keys vals k v -> k -> v
(!) m k = case Champ.Internal.lookup k m of
    Just v  -> v
    Nothing -> error "Champ.HashMap.(!): key not found"
{-# INLINABLE (!) #-}
-- TODO: Proper exception type?

infixl 9 !

-- | \(O(\log n)\) Return the value to which the specified key is mapped,
-- or the default value if this map contains no mapping for the key.
findWithDefault :: (Eq k, Hashable k, MapRepr keys vals k v)
              => v          -- ^ Default value to return.
              -> k -> HashMap keys vals k v -> v
findWithDefault def k t = case Champ.Internal.lookup k t of
    Just v -> v
    _      -> def
{-# INLINABLE findWithDefault #-}

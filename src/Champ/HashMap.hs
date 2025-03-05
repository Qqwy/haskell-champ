module Champ.HashMap (
    -- * Data types
    -- ** Concrete types
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
    -- * Generic type
    Champ.Internal.HashMap,
    Champ.Internal.Storage.Storage(..),
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
    -- TODO isSubmapOf
    -- TODO isSubmapOfBy
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
    Champ.Internal.withCoercible,

    -- ** Lists
    Champ.Internal.fromList,
    Champ.Internal.toList,
    Champ.Internal.keys,
    Champ.Internal.elems,
    Champ.Internal.fromListWith,
    Champ.Internal.fromListWithKey,

    -- ** HashSets
    Champ.HashSet.keysSet,
    Champ.HashSet.keysSet'
) where

import GHC.Stack (HasCallStack)
import Champ.Internal (HashMap, MapRepr)
import Champ.Internal qualified
import Champ.Internal.Storage qualified
import Champ.HashSet qualified
import Data.Hashable (Hashable)

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

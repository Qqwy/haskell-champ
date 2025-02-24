module Champ.HashMap (
    -- * Data types
    -- ** Concrete types
    Champ.Internal.HashMapBL,
    Champ.Internal.HashMapBB,
    Champ.Internal.HashMapBU,
    Champ.Internal.HashMapUL,
    Champ.Internal.HashMapUB,
    Champ.Internal.HashMapUU,
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
    (!?),
    (!),
    findWithDefault,
    Champ.Internal.insert,
    Champ.Internal.insertWith,
    Champ.Internal.delete,
    Champ.Internal.adjust,
    -- TODO Champ.Internal.update,
    Champ.Internal.alter,
    -- TODO alterF
    -- TODO isSubmapOf
    -- TODO isSubmapOfBy
    -- * Combine
    -- ** Union
    Champ.Internal.union,
    -- TODO unionWith
    -- TODO unionWithKey
    Champ.Internal.unions,
    -- ** Compose
    -- TODO compose
    -- * Transformations
    Champ.Internal.map,
    Champ.Internal.map',
    -- TODO mapWithKey
    -- TODO traverseWithKey
    Champ.Internal.mapKeys,

    -- * Difference and intersection
    Champ.Internal.difference,
    Champ.Internal.differenceWith,
    -- TODO intersection
    -- TODO intersectionWith
    -- TODO intersectionWithKey

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
    -- TODO filter
    -- TODO filterWithKey
    -- TODO mapMaybe
    -- TODO mapMaybeWithKey

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
    -- TODO fromListWith
    -- TODO fromListWithKey

    -- ** HashSets
    Champ.HashSet.keysSet
    -- Champ.HashSet.keysSet'
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

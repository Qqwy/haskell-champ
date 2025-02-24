module Champ.HashMap (
    -- * Data types
    Champ.Internal.HashMap,
    Champ.Internal.HashMapBL,
    Champ.Internal.HashMapBB,
    Champ.Internal.HashMapBU,
    Champ.Internal.HashMapUL,
    Champ.Internal.HashMapUB,
    Champ.Internal.HashMapUU,
    -- * Construction
    Champ.Internal.empty,
    Champ.Internal.singleton,
    -- * Basic interface
    Champ.Internal.null,
    Champ.Internal.size,
    Champ.Internal.member,
    Champ.Internal.lookup,
    -- TODO (!?),
    -- TODO findWithDefault
    -- TODO lookupDefault
    Champ.Internal.insert,
    -- TODO insertWith
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
    Champ.Internal.foldl,
    Champ.Internal.foldr',
    Champ.Internal.foldl',
    Champ.Internal.foldrWithKey,
    -- TODO foldrWithKey'
    -- TODO foldlWithKey
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

import Champ.Internal qualified
import Champ.HashSet qualified

{-# LANGUAGE DataKinds #-}
module Champ.HashSet (
    HashSet,
    HashSetB,
    HashSetU,
    empty,
    singleton,
    size,
    insert,
    fromList,
    toList,
    member,
    map,
    map'
) where

import Prelude hiding (map)
import Champ.Internal qualified
import Storage (Storage(Unexistent), StrictStorage(..))
import Data.Hashable (Hashable)
import Data.List qualified
import Data.Coerce (coerce)

newtype HashSet elems e = HashSet (Champ.Internal.HashMap elems Unexistent e ())
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
toList = fmap fst . Champ.Internal.toList . coerce

empty :: (SetRepr elems e) => HashSet elems e
{-# INLINE empty #-}
empty = coerce Champ.Internal.empty

size :: (SetRepr elems e) => HashSet elems e -> Int
{-# INLINE size #-}
size = coerce Champ.Internal.size

singleton :: (SetRepr elems e) => e -> HashSet elems e
{-# INLINE singleton #-}
singleton e = coerce Champ.Internal.singleton e ()

member :: (SetRepr elems e, Hashable e) => e -> HashSet elems e -> Bool
{-# INLINE member #-}
member = coerce Champ.Internal.member

map :: (Hashable b, SetRepr elems a, SetRepr elems b) => (a -> b) -> HashSet elems a -> HashSet elems b
{-# INLINE map #-}
map = map'

map' :: (Hashable b, SetRepr as a, SetRepr bs b) => (a -> b) -> HashSet as a -> HashSet bs b
{-# INLINE map' #-}
map' fun = fromList . Data.List.map fun . toList

foldr :: (SetRepr elems e) => (e -> r -> r) -> r -> HashSet elems e -> r
{-# INLINE foldr #-}
foldr fun acc set = Champ.Internal.foldrWithKey (\e () r -> fun e r) acc (coerce set)

foldl' :: (SetRepr elems e) => (r -> e -> r) -> r -> HashSet elems e -> r
{-# INLINE foldl' #-}
foldl' fun acc set = Champ.Internal.foldlWithKey' (\r e () -> fun r e) acc (coerce set)

-- TODO: Implement the other foldXWithKey's as well,
-- so we can wrap them here

instance Foldable (HashSet Boxed) where
    {-# INLINE foldr #-}
    foldr = Champ.HashSet.foldr
    {-# INLINE foldl' #-}
    foldl' = Champ.HashSet.foldl'
    {-# INLINE length #-}
    length = Champ.HashSet.size

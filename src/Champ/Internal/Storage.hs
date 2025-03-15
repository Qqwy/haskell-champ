{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-from-core -ddump-to-file #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

#if __GLASGOW_HASKELL__ >= 906
{-# LANGUAGE TypeData #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

module Champ.Internal.Storage where

import Champ.Internal.Array qualified as Array
import Data.Kind (Type)
import Data.Tuple (Solo (MkSolo))
import Data.Functor.Identity (Identity(..))

-- | The different ways of storing data.
--
-- In a way similar to `RuntimeRep`,
-- except that it makes more sense to combine strict boxed/unboxed together
-- (as keys are always strict)
-- whereas `RuntimeRep` combines boxed types together (regardless of levity).
#if __GLASGOW_HASKELL__ >= 906
type data Storage 
#else
data Storage
#endif
  = Lazy -- ^ It might be a thunk, and therefore is definitely boxed. (AKA of kind `Type` AKA `TYPE '(BoxedRep Lifted)`)
  | Strict StrictStorage -- ^ It is definitely not a thunk, may or may not be boxed
  | Unexistent -- ^ The value has no runtime representation; used as value type for `Set`.

#if __GLASGOW_HASKELL__ >= 906
type data StrictStorage 
#else
data StrictStorage
#endif
  = Boxed -- ^ The value is boxed (AKA of kind `UnliftedType` AKA `TYPE '(BoxedRep Unlifted)`), by forcing the value of any `Type` and wrapping it in a `Data.Elevator.Strict` wrapper.
  | Unboxed -- ^ The value is of some unboxed kind, i.e. implement `Prim`.
  | Unlifted -- ^ Similar to 'Boxed', but for types that have a natural `UnliftedType` equivalent, i.e. implement `PrimUnlifted`

-- | Different kinds of values can be stored in different kinds of arrays:
--
-- * If the value has no runtime representation, we need no array at all.
-- * If the value is an unboxed value and therefore not interesting to the GC,
--   AKA it implements the `Prim` class, it can be stored in a `PrimArray`.
-- * If the value is `Strict Boxed`, we store it in a `StrictSmallArray`
--   and GHC knows that it won't need to do any 'is it a Thunk I still need to evaluate?' checks
--   when reading from it.
-- * If the value is `Lazy`, we use a normal small array. 
--   Most powerful/flexible for the user, but least optimizable.
type family ArrayOf (s :: Storage) = (r :: Type -> Type) | r -> s where
  ArrayOf Lazy = Array.SmallArray
  ArrayOf (Strict Boxed) = Array.StrictSmallArray
  ArrayOf (Strict Unlifted) = Array.SmallUnliftedArray'
  ArrayOf (Strict Unboxed) = Array.PrimArray
  ArrayOf Unexistent = Array.UnitArray

-- | Internal class, used to wrap a single value stored in a hashmap that is lazy in its values,
-- behind a `Data.Tuple.Solo`.
--
-- For any other kind of hashmap, this indirection is not required, 
-- so there the `Identity` newtype is used instead.
class Soloist (s :: Storage) where
  type SoloType s :: Type -> Type
  unSolo :: SoloType s a -> a
  solo :: a -> SoloType s a

instance Soloist Lazy where
  type SoloType Lazy = Data.Tuple.Solo
  unSolo (Data.Tuple.MkSolo x) = x
  solo = Data.Tuple.MkSolo

instance Soloist (Strict any) where
  type SoloType (Strict any) = Identity
  unSolo (Identity x) = x
  solo = Identity

instance Soloist Unexistent where
  type SoloType Unexistent = Identity
  unSolo (Identity x) = x
  solo = Identity

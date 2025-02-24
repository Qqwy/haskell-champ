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

module Champ.Internal.Storage where

import Champ.Internal.Array qualified as Array
import Data.Kind (Type)

-- | The different ways of storing data.
--
-- In a way similar to `RuntimeRep`,
-- except that it makes more sense to combine strict boxed/unboxed together
-- (as keys are always strict)
-- whereas `RuntimeRep` combines boxed types together (regardless of levity).
data Storage 
  = Lazy -- ^ It might be a thunk, and therefore is definitely boxed. (AKA of kind `Type` AKA `TYPE '(BoxedRep Lifted)`)
  | Strict StrictStorage -- ^ It is definitely not a thunk, may or may not be boxed
  | Unexistent -- ^ The value has no runtime representation; used as value type for `Set`.

data StrictStorage 
  = Boxed -- ^ The value is boxed (AKA of kind `UnliftedType` AKA `TYPE '(BoxedRep Unlifted)`)
  | Unboxed -- ^ The value is of some unboxed kind.

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
  ArrayOf (Strict Unboxed) = Array.PrimArray
  ArrayOf Unexistent = Array.UnitArray

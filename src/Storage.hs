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

module Storage where

import Array qualified
import Data.Kind (Type)

data Storage = Lazy | Strict StrictStorage

data StrictStorage = Boxed | Unboxed

type family ArrayOf (s :: Storage) = (r :: Type -> Type) | r -> s where
  ArrayOf Lazy = Array.SmallArray
  ArrayOf (Strict Boxed) = Array.StrictSmallArray
  ArrayOf (Strict Unboxed) = Array.PrimArray

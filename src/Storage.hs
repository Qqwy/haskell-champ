{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-stg-from-core -ddump-to-file #-}
module Storage where
import Data.Kind (Type)

import Array qualified

data Storage = Lazy | Strict StrictStorage
data StrictStorage = Boxed | Unboxed

type family ArrayOf (s :: Storage) = (r :: Type -> Type) | r -> s where
  ArrayOf Lazy = Array.SmallArray
  ArrayOf (Strict Boxed) = Array.StrictSmallArray
  ArrayOf (Strict Unboxed) = Array.PrimArray

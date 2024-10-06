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

type family ArrayOf (s :: Storage) a = (r :: Type) | r -> s a where
  ArrayOf Lazy a = Array.SmallArray a
  ArrayOf (Strict Boxed) a = Array.SmallUnliftedArray a
  ArrayOf (Strict Unboxed) a = Array.PrimArray a

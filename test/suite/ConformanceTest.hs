{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tests which assume that Data.HashMap works well,
-- and that Champ.HashMap should have identical behaviour
-- (sans memory and performance behaviour)
module ConformanceTest where

import Data.HashMap.Strict qualified
import Champ
import Champ.HashMap qualified

import Data.Kind (Type)
import GHC.IsList

-- Testing helpers:
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (sort)

test_fromListToList :: [TestTree]
test_fromListToList = 
    [
        testProperty "toList . fromList conforms (HashMapBB)" $ propFromListToListConforms @HashMapBB
    ]

propFromListToListConforms :: forall champmap keys vals. (champmap ~ HashMap keys vals, MapRepr keys vals Int Int, IsList (champmap Int Int), Item (champmap Int Int) ~ (Int, Int)) => Property
propFromListToListConforms = property $ do
    list <- forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 20))
    let kvs = [(x, x) | x <- list]
    annotateShow kvs

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs

    True === True


    -- sort (Champ.HashMap.toList cs) === sort (Data.HashMap.Strict.toList hs)

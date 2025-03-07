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
    [ testProperty "toList . fromList conforms (HashMapBL)" $ propFromListToListConforms @HashMapBL
    , testProperty "toList . fromList conforms (HashMapBB)" $ propFromListToListConforms @HashMapBB
    , testProperty "toList . fromList conforms (HashMapBU)" $ propFromListToListConforms @HashMapBU
    , testProperty "toList . fromList conforms (HashMapUL)" $ propFromListToListConforms @HashMapUL
    , testProperty "toList . fromList conforms (HashMapUB)" $ propFromListToListConforms @HashMapUB
    , testProperty "toList . fromList conforms (HashMapUU)" $ propFromListToListConforms @HashMapUU
    ]

test_lookup :: [TestTree]
test_lookup = 
    [ testProperty "lookup conforms (HashMapBL)" $ propLookupConforms @HashMapBL
    , testProperty "lookup conforms (HashMapBB)" $ propLookupConforms @HashMapBB
    , testProperty "lookup conforms (HashMapBU)" $ propLookupConforms @HashMapBU
    , testProperty "lookup conforms (HashMapUL)" $ propLookupConforms @HashMapUL
    , testProperty "lookup conforms (HashMapUB)" $ propLookupConforms @HashMapUB
    , testProperty "lookup conforms (HashMapUU)" $ propLookupConforms @HashMapUU
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

    sort (Data.HashMap.Strict.toList hs) === sort (Champ.HashMap.toList cs)

propLookupConforms :: forall champmap keys vals. (champmap ~ HashMap keys vals, MapRepr keys vals Int Int, IsList (champmap Int Int), Item (champmap Int Int) ~ (Int, Int)) => Property
propLookupConforms = property $ do
    list <- forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 0 20))
    let kvs = [(x, x) | x <- list]
    annotateShow kvs

    key <- forAll (Gen.int (Range.linear 0 20))

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs

    Data.HashMap.Strict.lookup key hs === Champ.HashMap.lookup key cs
    -- Champ.HashMap.lookup key cs === Champ.HashMap.lookup key cs


{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tests which assume that Data.HashMap works well,
-- and that Champ.HashMap should have identical behaviour
-- (sans memory and performance behaviour)
module ConformanceTest where

import Data.HashMap.Strict qualified
import Control.Monad (forM_)
import Champ
import Champ.HashMap qualified
import Champ.Internal qualified
import Champ.Internal.Storage qualified

import GHC.IsList

-- Testing helpers:
import Test.Tasty
import Test.Tasty.Hedgehog
import Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import Data.List (sort)

tests :: TestLimit
tests = 5000

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

test_filterWithKey :: [TestTree]
test_filterWithKey =
    [ testProperty "filterWithKey conforms (HashMapBL)" $ propFilterWithKeyConforms @HashMapBL
    , testProperty "filterWithKey conforms (HashMapBB)" $ propFilterWithKeyConforms @HashMapBB
    , testProperty "filterWithKey conforms (HashMapBU)" $ propFilterWithKeyConforms @HashMapBU
    , testProperty "filterWithKey conforms (HashMapUL)" $ propFilterWithKeyConforms @HashMapUL
    , testProperty "filterWithKey conforms (HashMapUB)" $ propFilterWithKeyConforms @HashMapUB
    , testProperty "filterWithKey conforms (HashMapUU)" $ propFilterWithKeyConforms @HashMapUU
    ]

propFromListToListConforms :: forall champmap keys vals. 
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
    , Item (champmap Int Int) ~ (Int, Int)
    , Show (Champ.Internal.Storage.ArrayOf (Strict keys) Int)
    , Show (Champ.Internal.Storage.ArrayOf vals Int)
    )
    => Property
propFromListToListConforms = withTests tests $ property $ do
    list <- forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 1 20))
    let kvs = [(x, x) | x <- list]
    annotateShow kvs

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs
    annotate (Champ.Internal.debugShow cs)

    sort (Data.HashMap.Strict.toList hs) === sort (Champ.HashMap.toList cs)

propLookupConforms :: forall champmap keys vals. 
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
    , Item (champmap Int Int) ~ (Int, Int)
    , Show (Champ.Internal.Storage.ArrayOf (Strict keys) Int)
    , Show (Champ.Internal.Storage.ArrayOf vals Int)
    )
    => Property
propLookupConforms = withTests tests $ property $ do
    list <- forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 1 20))
    let kvs = [(x, x) | x <- list]
    annotateShow kvs

    key <- forAll (Gen.int (Range.linear 1 20))

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs
    annotate (Champ.Internal.debugShow cs)

    Data.HashMap.Strict.lookup key hs === Champ.HashMap.lookup key cs
    -- Champ.HashMap.lookup key cs === Champ.HashMap.lookup key cs

propFilterWithKeyConforms :: forall champmap keys vals.
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
    , Item (champmap Int Int) ~ (Int, Int)
    , Show (Champ.Internal.Storage.ArrayOf (Strict keys) Int)
    , Show (Champ.Internal.Storage.ArrayOf vals Int)
    )
    => Property
propFilterWithKeyConforms = withTests tests $ property $ do
    let n = 1024
    listInp <- forAll $ Gen.list (Range.linear 0 n) $ do
        x <- (Gen.int (Range.linear 1 n))
        b <- Gen.bool
        pure (x, b)

    let ks = [x | (x, _) <- listInp]
    let kvs = zip ks ks
    let fn k _ = Data.HashMap.Strict.lookupDefault (error "impossible") k $
            Data.HashMap.Strict.fromList listInp
    annotateShow kvs

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs
    annotate (Champ.Internal.debugShow cs)

    let hsFiltered = Data.HashMap.Strict.filterWithKey fn hs
    let csFiltered = Champ.HashMap.filterWithKey fn cs
    annotateShow hsFiltered
    annotateShow csFiltered
    annotate (Champ.Internal.debugShow csFiltered)

    -- Check that the hashmaps contain the same things
    sort (Data.HashMap.Strict.toList hsFiltered) === sort (Champ.HashMap.toList csFiltered)
    -- Check that champ contains the correct structure.
    forM_ ks $ \k -> do
        Data.HashMap.Strict.lookup k hsFiltered === (Champ.HashMap.lookup k csFiltered)

    -- Check that we didn't mutate anything the unfiltered pointed to.
    sort (Data.HashMap.Strict.toList hs) === sort (Champ.HashMap.toList cs)
    -- Check that champ contains the correct structure.
    forM_ ks $ \k -> do
        Data.HashMap.Strict.lookup k hs === (Champ.HashMap.lookup k cs)

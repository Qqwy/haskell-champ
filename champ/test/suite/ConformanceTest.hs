{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- | Tests which assume that Data.HashMap works well,
-- and that Champ.HashMap should have identical behaviour
-- (sans memory and performance behaviour)
module ConformanceTest where

import Data.HashMap.Strict qualified
import Data.Hashable (Hashable(..))
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
tests = 1000

test_fromListToList :: TestTree
test_fromListToList = testGroup "toList . fromList"
    [ testProperty "conforms (HashMapBL)" $ propFromListToListConforms @HashMapBL
    , testProperty "conforms (HashMapBB)" $ propFromListToListConforms @HashMapBB
    , testProperty "conforms (HashMapBU)" $ propFromListToListConforms @HashMapBU
    , testProperty "conforms (HashMapUL)" $ propFromListToListConforms @HashMapUL
    , testProperty "conforms (HashMapUB)" $ propFromListToListConforms @HashMapUB
    , testProperty "conforms (HashMapUU)" $ propFromListToListConforms @HashMapUU
    ]

test_fromListToList' :: TestTree
test_fromListToList' = testGroup "toList . fromList (with conflicts)"
    [ testProperty "conforms (HashMapBL)" $ propFromListToListWithConflictsConforms @HashMapBL
    , testProperty "conforms (HashMapBB)" $ propFromListToListWithConflictsConforms @HashMapBB
    , testProperty "conforms (HashMapBU)" $ propFromListToListWithConflictsConforms @HashMapBU
    ]



propFromListToListConforms :: forall champmap keys vals. 
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
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

test_lookup :: TestTree
test_lookup = testGroup "lookup"
    [ testProperty "conforms (HashMapBL)" $ propLookupConforms @HashMapBL
    , testProperty "conforms (HashMapBB)" $ propLookupConforms @HashMapBB
    , testProperty "conforms (HashMapBU)" $ propLookupConforms @HashMapBU
    , testProperty "conforms (HashMapUL)" $ propLookupConforms @HashMapUL
    , testProperty "conforms (HashMapUB)" $ propLookupConforms @HashMapUB
    , testProperty "conforms (HashMapUU)" $ propLookupConforms @HashMapUU
    ]

propLookupConforms :: forall champmap keys vals. 
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
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

test_filterWithKey :: TestTree
test_filterWithKey = testGroup "filterWithKey"
    [ testProperty "conforms (HashMapBL)" $ propFilterWithKeyConforms @HashMapBL
    , testProperty "conforms (HashMapBB)" $ propFilterWithKeyConforms @HashMapBB
    , testProperty "conforms (HashMapBU)" $ propFilterWithKeyConforms @HashMapBU
    , testProperty "conforms (HashMapUL)" $ propFilterWithKeyConforms @HashMapUL
    , testProperty "conforms (HashMapUB)" $ propFilterWithKeyConforms @HashMapUB
    , testProperty "conforms (HashMapUU)" $ propFilterWithKeyConforms @HashMapUU
    ]

propFilterWithKeyConforms :: forall champmap keys vals.
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
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


test_mapMaybeWithKey :: TestTree
test_mapMaybeWithKey = testGroup "mapMaybeWithKey"
    [ testProperty "conforms (HashMapBL)" $ propMapMaybeWithKeyConforms @HashMapBL
    , testProperty "conforms (HashMapBB)" $ propMapMaybeWithKeyConforms @HashMapBB
    , testProperty "conforms (HashMapBU)" $ propMapMaybeWithKeyConforms @HashMapBU
    , testProperty "conforms (HashMapUL)" $ propMapMaybeWithKeyConforms @HashMapUL
    , testProperty "conforms (HashMapUB)" $ propMapMaybeWithKeyConforms @HashMapUB
    , testProperty "conforms (HashMapUU)" $ propMapMaybeWithKeyConforms @HashMapUU
    ]


propMapMaybeWithKeyConforms :: forall champmap keys vals.
    (champmap ~ HashMap keys vals
    , MapRepr keys vals Int Int
    , IsList (champmap Int Int)
    , Show (Champ.Internal.Storage.ArrayOf (Strict keys) Int)
    , Show (Champ.Internal.Storage.ArrayOf vals Int)
    )
    => Property
propMapMaybeWithKeyConforms = withTests tests $ property $ do
    let n = 1024
    listInp <- forAll $ Gen.list (Range.linear 0 n) $ do
        x <- (Gen.int (Range.linear 1 n))
        b <- Gen.bool
        pure (x, b)

    let ks = [x | (x, _) <- listInp]
        kvs = zip ks ks
        m = Data.HashMap.Strict.fromList listInp
        fn k v = case Data.HashMap.Strict.lookup k m of
            Just True -> Just v
            _ -> Nothing

    annotateShow kvs

    let hs = fromList kvs
    let cs = fromList kvs :: champmap Int Int

    annotateShow hs
    annotateShow cs
    annotate (Champ.Internal.debugShow cs)

    let hsFiltered = Data.HashMap.Strict.mapMaybeWithKey fn hs
    let csFiltered = Champ.HashMap.mapMaybeWithKey fn cs
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


-- | A datatype with a bad Hashable instance
--
-- Used for testing that dealing with hash conflicts
-- works correctly
data BadPair a b = BadPair a b
  deriving (Eq, Ord, Show)

instance (Hashable a, Eq a, Eq b) => Hashable (BadPair a b) where
    hashWithSalt salt (BadPair a _) = hashWithSalt salt a


propFromListToListWithConflictsConforms :: forall champmap keys vals. 
    (champmap ~ HashMap keys vals
    , MapRepr keys vals (BadPair Int Int) Int
    , IsList (champmap (BadPair Int Int) Int)
    , Show (Champ.Internal.Storage.ArrayOf (Strict keys) (BadPair Int Int))
    , Show (Champ.Internal.Storage.ArrayOf vals Int)
    )
    => Property
propFromListToListWithConflictsConforms = withTests tests $ property $ do
    list <- forAll $ Gen.list (Range.linear 0 200) (Gen.int (Range.linear 1 20))
    let kvs = [((BadPair (x `div` 2) x), x) | x <- list]
    annotateShow kvs

    let hs = fromList kvs
    let cs = fromList kvs :: champmap (BadPair Int Int) Int

    annotateShow hs
    annotateShow cs
    annotate (Champ.Internal.debugShow cs)

    sort (Data.HashMap.Strict.toList hs) === sort (Champ.HashMap.toList cs)

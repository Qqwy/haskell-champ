{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE MagicHash #-}

module Main (main) where

import Champ.HashMap qualified as Champ
import Champ.Internal qualified as Champ
import Data.Data (Proxy (..))
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Kind (Type)
import Hedgehog
import Hedgehog.Gen as Hedgehog
import Hedgehog.Range as Hedgehog
import Test.Tasty
import Test.Tasty.Hedgehog

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  setTestLimit 500 $
    testGroup "Tests" [properties]
  where
    setTestLimit n =
      adjustOption
        (\(HedgehogTestLimit _) -> HedgehogTestLimit (Just n))

properties :: TestTree
properties =
  testGroup
    "Properties"
    [constructionProps]

constructionProps :: TestTree
constructionProps =
  testGroup
    "construction"
    [ testGroup
        "fromList"
        $ let testFromList genAssocList finalize = do
                input <- forAll genAssocList
                finalize (Champ.fromList input)
                  === (HashMap.fromList input)
           in [ testOnChamp "HashMapBL, int8" (Proxy @Champ.HashMapBL) $
                  testFromList (bijectiveAssocs Hedgehog.int8),
                testOnChamp "HashMapBL, int16" (Proxy @Champ.HashMapBL) $
                  testFromList (bijectiveAssocs Hedgehog.int16),
                testOnChamp "HashMapBL, int32" (Proxy @Champ.HashMapBL) $
                  testFromList (bijectiveAssocs Hedgehog.int32),
                testOnChamp "HashMapBB, int32" (Proxy @Champ.HashMapBB) $
                  testFromList (bijectiveAssocs Hedgehog.int32),
                testOnChamp "HashMapBU, int32" (Proxy @Champ.HashMapBU) $
                  testFromList (bijectiveAssocs Hedgehog.int32),
                testOnChamp "HashMapUL, int32" (Proxy @Champ.HashMapUL) $
                  testFromList (bijectiveAssocs Hedgehog.int32),
                testOnChamp "HashMapUB, int32" (Proxy @Champ.HashMapUB) $
                  testFromList (bijectiveAssocs Hedgehog.int32),
                testOnChamp "HashMapUU, int32" (Proxy @Champ.HashMapUU) $
                  testFromList (bijectiveAssocs Hedgehog.int32)
              ]
    ]

bijectiveAssocs :: (MonadGen m, Bounded b, Integral b) => (Range b -> m a) -> m [(a, a)]
bijectiveAssocs genNum = do
  input <- randomBoundedList 2048 genNum
  pure $ zipWith (,) input input

testOnChamp ::
  forall (map :: Type -> Type -> Type) keys vals k v.
  (Hashable k, Champ.MapRepr keys vals k v, Champ.HashMap keys vals ~ map) =>
  TestName ->
  Proxy map ->
  ( (Champ.HashMap keys vals k v -> HashMap.HashMap k v) ->
    PropertyT IO ()
  ) ->
  TestTree
testOnChamp name _ prop = testProperty name $ property $ prop (toHM . id @(map k v))

randomBoundedList :: (MonadGen m, Bounded a, Integral a) => Int -> (Range a -> m b) -> m [b]
randomBoundedList listSize constr = Hedgehog.list (Hedgehog.linear 0 listSize) (constr Hedgehog.linearBounded)

toHM :: (Eq k, Hashable k, Champ.MapRepr keys vals k v) => Champ.HashMap keys vals k v -> HashMap.HashMap k v
toHM = HashMap.fromList . Champ.toList

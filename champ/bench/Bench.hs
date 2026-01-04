{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-stg-from-core -ddump-cmm -ddump-to-file #-}

import Control.DeepSeq (NFData, force)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified
import Data.HashMap.Strict qualified
import GHC.IsList
import Champ hiding (HashMap)
import Champ.HashMap qualified
import Champ.HashSet qualified
import Test.Tasty.Bench qualified as Bench
import Data.Text.Short (ShortText)
import Data.String (IsString(..))

main :: IO ()
main =
  Bench.defaultMain
    [ insertBench "small" [1..32]
    , insertBench "(powers of two)" (powersOfTwo 10)
    , lookupBench "small" [1..32]
    , lookupBench "(powers of two)" (powersOfTwo 10)
    , lookupByteArrayBench "small" [1..32]
    , lookupByteArrayBench "(powers of two)" (powersOfTwo 10)
    , foldrBench "small" [1..32]
    , foldrBench "(powers of two)" (powersOfTwo 10)
    , foldl'Bench "small" [1..32]
    , foldl'Bench "(powers of two)" (powersOfTwo 10)
    , filterWithKeyBench "small" [0..32]
    , filterWithKeyBench "(powers of two)" (powersOfTwo 13)
    , mapMaybeWithKeyBench "small" [0..32]
    , mapMaybeWithKeyBench "(powers of two)" (powersOfTwo 13)
    , toListBench "small" [1..32]
    , toListBench "(powers of two)" (powersOfTwo 10)
    ]

insertBench :: String -> [Int] -> Bench.Benchmark
insertBench s ns =
  Bench.bgroup ("insert " <> s) $
    mconcat
      [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.insert n n) (buildN n),
          Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.insert n n) (buildN n),
          Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapBL n),
          Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapBB n),
          Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapBU n),
          Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapUL n),
          Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapUB n),
          Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.insert n n) (buildN @HashMapUU n)
        ]
      | n <- ns
      ]

lookupBench :: String -> [Int] -> Bench.Benchmark
lookupBench s ns =
  Bench.bgroup ("Lookup " <> s) $
    mconcat
      [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.lookup (n)) (buildN n),
          Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.lookup (n)) (buildN n),
          Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapBL n),
          Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapBB n),
          Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapBU n),
          Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapUL n),
          Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapUB n),
          Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.lookup (n)) (buildN @HashMapUU n)
        ]
      | n <- ns
      ]

lookupByteArrayBench :: String -> [Int] -> Bench.Benchmark
lookupByteArrayBench s ns =
  Bench.bgroup ("Lookup ShortText " <> s) $
    mconcat
      [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.lookup (fromString (show n))) (buildBAN @HashMap n),
          Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.lookup (fromString (show n))) (buildBAN @HashMap n),
          Bench.bench ("HashMapBL." <> show n) $ Bench.nf  (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapBL n),
          Bench.bench ("HashMapBB." <> show n) $ Bench.nf  (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapBB n),
          Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapBU n),
          Bench.bench ("HashMapUlL." <> show n) $ Bench.nf (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapUlL n),
          Bench.bench ("HashMapUlB." <> show n) $ Bench.nf (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapUlB n),
          Bench.bench ("HashMapUlU." <> show n) $ Bench.nf (Champ.HashMap.lookup (fromString (show n))) (buildBAN @HashMapUlU n)
        ]
      | n <- ns
      ]

foldrBench :: String -> [Int] -> Bench.Benchmark
foldrBench s ns =
  Bench.bgroup ("Foldr (+) 0 " <> s) $
    mconcat
          [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.foldr (+) 0) (buildN @HashMap n),
              Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.foldr (+) 0) (buildN @HashMap n),
              Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapBL n),
              Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapBB n),
              Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapBU n),
              Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapUL n),
              Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapUB n),
              Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.foldr (+) 0) (buildN @HashMapUU n)
            ]
          | n <- ns
          ]

foldl'Bench :: String -> [Int] -> Bench.Benchmark
foldl'Bench s ns =
  Bench.bgroup ("foldl' (+) 0 " <> s) $
    mconcat
          [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.foldl' (+) 0) (buildN @HashMap n),
              Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.foldl' (+) 0) (buildN @HashMap n),
              Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapBL n),
              Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapBB n),
              Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapBU n),
              Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapUL n),
              Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapUB n),
              Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.foldl' (+) 0) (buildN @HashMapUU n)
            ]
          | n <- ns
          ]

toListBench :: String -> [Int] -> Bench.Benchmark
toListBench s ns =
  Bench.bgroup ("toList " <> s) $
     mconcat
          [ [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.toList) (buildN @HashMap n),
              Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.toList) (buildN @HashMap n),
              Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapBL n),
              Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapBB n),
              Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapBU n),
              Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapUL n),
              Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapUB n),
              Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.toList) (buildN @HashMapUU n)
            ]
          | n <- ns
          ]

filterWithKeyBench :: String -> [Int] -> Bench.Benchmark
filterWithKeyBench s ns = do
  let isEven _ v = even v
  Bench.bgroup ("filterWithKey " <> s) $
    mconcat
      [ benchmarksTranformOn
          (Data.HashMap.Lazy.filterWithKey isEven)
          (Data.HashMap.Strict.filterWithKey isEven)
          (Champ.HashMap.filterWithKey isEven)
          n
      | n <- ns
      ]

mapMaybeWithKeyBench :: String -> [Int] -> Bench.Benchmark
mapMaybeWithKeyBench s ns = do
  let isEven _ v = if even v then Just v else Nothing
  Bench.bgroup ("mapMaybeWithKey " <> s) $
    mconcat
      [ benchmarksTranformOn
          (Data.HashMap.Lazy.mapMaybeWithKey isEven)
          (Data.HashMap.Strict.mapMaybeWithKey isEven)
          (Champ.HashMap.mapMaybeWithKey isEven)
          n
      | n <- ns
      ]

benchmarksTranformOn ::
  (HashMap Int Int -> HashMap Int Int) ->
  (HashMap Int Int -> HashMap Int Int) ->
  ( forall keys vals.
    (MapRepr keys vals Int Int) => Champ.HashMap.HashMap keys vals Int Int -> Champ.HashMap.HashMap keys vals Int Int
  ) ->
  Int ->
  [Bench.Benchmark]
benchmarksTranformOn onLazy onStrict onChamp n =
  [ Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf onLazy (buildN @HashMap n),
    Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf onStrict (buildN @HashMap n),
    Bench.bench ("HashMapBL." <> show n) $ Bench.nf onChamp (buildN @HashMapBL n),
    Bench.bench ("HashMapBB." <> show n) $ Bench.nf onChamp (buildN @HashMapBB n),
    Bench.bench ("HashMapBU." <> show n) $ Bench.nf onChamp (buildN @HashMapBU n),
    Bench.bench ("HashMapUL." <> show n) $ Bench.nf onChamp (buildN @HashMapUL n),
    Bench.bench ("HashMapUB." <> show n) $ Bench.nf onChamp (buildN @HashMapUB n),
    Bench.bench ("HashMapUU." <> show n) $ Bench.nf onChamp (buildN @HashMapUU n)
  ]

-- myinsert :: Int -> Int -> MyLib.MapBL Int Int -> MyLib.MapBL Int Int
-- {-# NOINLINE myinsert #-}
-- myinsert = Champ.HashMap.insert

-- theirinsert :: Int -> Int -> HashMap Int Int -> HashMap Int Int
-- {-# NOINLINE theirinsert #-}
-- theirinsert = Data.HashMap.Lazy.insert

-- mylookup :: Int -> MyLib.MapBL Int Int -> Maybe Int
-- {-# NOINLINE mylookup #-}
-- mylookup = MyLib.lookup

-- theirlookup :: Int -> HashMap Int Int -> Maybe Int
-- {-# NOINLINE theirlookup #-}
-- theirlookup = Data.HashMap.Lazy.lookup

buildN :: forall map. (NFData (map Int Int), IsList (map Int Int), Item (map Int Int) ~ (Int, Int)) => Int -> (map Int Int)
buildN n = force $ fromList [(x, x) | x <- [0 .. (n - 1)]]

buildBAN :: forall map. (NFData (map ShortText Int), IsList (map ShortText Int), Item (map ShortText Int) ~ (ShortText, Int)) => Int -> (map ShortText Int)
buildBAN n = force $ fromList [(fromString (show x), x) | x <- [0 .. (n - 1)]]

powersOfTwo :: Int -> [Int]
powersOfTwo n = [2 ^ (x :: Int) | x <- [0 .. n]]

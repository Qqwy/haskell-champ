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
    [ insertSmallBench
    , insertBench
    , lookupSmallBench
    , lookupBench
    , lookupByteArraySmallBench
    , lookupByteArrayBench
    , foldrSmallBench
    , foldrBench
    , foldl'SmallBench
    , foldl'Bench
    , filterWithKeyBench [0..32]
    , filterWithKeyBench (0 : powersOfTwo 13)
    , mapMaybeWithKeyBench [0..32]
    , mapMaybeWithKeyBench (0 : powersOfTwo 13)
    , toListSmallBench
    , toListBench
    ]

insertSmallBench :: Bench.Benchmark
insertSmallBench =
  Bench.bgroup "insert (small)" $
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
      | n <- ([0..32])
      ]

insertBench :: Bench.Benchmark
insertBench =
  Bench.bgroup "insert (powers of two)" $
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
      | n <- ([0] <> powersOfTwo 10)
      ]

lookupSmallBench :: Bench.Benchmark
lookupSmallBench =
  Bench.bgroup "Lookup (small)" $
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
      | n <- [0..32]
      ]

lookupBench :: Bench.Benchmark
lookupBench =
  Bench.bgroup "Lookup (powers of two)" $
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
      | n <- powersOfTwo 10
      ]

lookupByteArraySmallBench :: Bench.Benchmark
lookupByteArraySmallBench =
  Bench.bgroup "Lookup ShortText (small)" $
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
      | n <- [0..32]
      ]

lookupByteArrayBench :: Bench.Benchmark
lookupByteArrayBench =
  Bench.bgroup "Lookup ShortText (powers of two)" $
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
      | n <- powersOfTwo 10
      ]

foldrSmallBench :: Bench.Benchmark
foldrSmallBench =
  Bench.bgroup "Foldr (+) 0 (small)" $
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
          | n <- [0..32]
          ]


foldrBench :: Bench.Benchmark
foldrBench =
  Bench.bgroup "Foldr (+) 0 (powers of two)" $
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
          | n <- ([0] <> powersOfTwo 10)
          ]

foldl'SmallBench :: Bench.Benchmark
foldl'SmallBench =
  Bench.bgroup "foldl' (+) 0 (small)" $
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
          | n <- [0..32]
          ]


foldl'Bench :: Bench.Benchmark
foldl'Bench =
  Bench.bgroup "foldl' (+) 0 (powers of two)" $
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
          | n <- ([0] <> powersOfTwo 10)
          ]

toListSmallBench :: Bench.Benchmark
toListSmallBench =
  Bench.bgroup "toList (small)" $
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
          | n <- [0..32]
          ]


toListBench :: Bench.Benchmark
toListBench =
  Bench.bgroup "toList (powers of two)" $
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
          | n <- ([0] <> powersOfTwo 10)
          ]


filterWithKeyBench :: [Int] -> Bench.Benchmark
filterWithKeyBench ns = do
  let isEven _ v = even v
  Bench.bgroup "filterWithKey (powers of two)" $
     mconcat
          [ [
              Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.filterWithKey isEven) (buildN @HashMap n),
              Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.filterWithKey isEven) (buildN @HashMap n),
              Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapBL n),
              Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapBB n),
              Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapBU n),
              Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapUL n),
              Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapUB n),
              Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.filterWithKey isEven) (buildN @HashMapUU n)
            ]
          | n <- ns
          ]


mapMaybeWithKeyBench :: [Int] -> Bench.Benchmark
mapMaybeWithKeyBench ns = do
  let isEven _ v =
        if even v
          then Just v
          else Nothing
  Bench.bgroup "mapMaybeWithKey (powers of two)" $
     mconcat
          [ [
              Bench.bench ("Data.HashMap.Lazy." <> show n) $ Bench.nf (Data.HashMap.Lazy.mapMaybeWithKey isEven) (buildN @HashMap n),
              Bench.bench ("Data.HashMap.Strict." <> show n) $ Bench.nf (Data.HashMap.Strict.mapMaybeWithKey isEven) (buildN @HashMap n),
              Bench.bench ("HashMapBL." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapBL n),
              Bench.bench ("HashMapBB." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapBB n),
              Bench.bench ("HashMapBU." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapBU n),
              Bench.bench ("HashMapUL." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapUL n),
              Bench.bench ("HashMapUB." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapUB n),
              Bench.bench ("HashMapUU." <> show n) $ Bench.nf (Champ.HashMap.mapMaybeWithKey isEven) (buildN @HashMapUU n)
            ]
          | n <- ns
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

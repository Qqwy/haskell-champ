{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-stg-from-core -ddump-cmm -ddump-to-file #-}

import Control.DeepSeq (NFData, force)
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap.Lazy
import Data.HashMap.Lazy qualified as HashMap.Strict
import GHC.IsList
import MyLib qualified
import Test.Tasty.Bench qualified as Bench

main :: IO ()
main =
  Bench.defaultMain
    [ insertSmallBench
    , insertBench
      -- foldrBench
      -- lookupBench
    ]

insertSmallBench :: Bench.Benchmark
insertSmallBench =
  Bench.bgroup "insert (small)" $
    mconcat
      [ [ Bench.bench ("HashMapLazy." <> show n) $ Bench.nf (HashMap.Lazy.insert n n) (buildN n),
          Bench.bench ("HashMapStrict." <> show n) $ Bench.nf (HashMap.Strict.insert n n) (buildN n),
          Bench.bench ("MapBL." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBL n),
          Bench.bench ("MapBB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBB n),
          Bench.bench ("MapBU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBU n),
          Bench.bench ("MapUL." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUL n),
          Bench.bench ("MapUB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUB n),
          Bench.bench ("MapUU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUU n)
        ]
      | n <- ([0..32])
      ]

insertBench :: Bench.Benchmark
insertBench =
  Bench.bgroup "insert (powers of two)" $
    mconcat
      [ [ Bench.bench ("HashMapLazy." <> show n) $ Bench.nf (HashMap.Lazy.insert n n) (buildN n),
          Bench.bench ("HashMapStrict." <> show n) $ Bench.nf (HashMap.Strict.insert n n) (buildN n),
          Bench.bench ("MapBL." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBL n),
          Bench.bench ("MapBB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBB n),
          Bench.bench ("MapBU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapBU n),
          Bench.bench ("MapUL." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUL n),
          Bench.bench ("MapUB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUB n),
          Bench.bench ("MapUU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @MyLib.MapUU n)
        ]
      | n <- ([0] <> powersOfTwo)
      ]

lookupBench :: Bench.Benchmark
lookupBench =
  Bench.bgroup "Looking up an element which exists in a hashmap of size N" $
    mconcat
      [ [ Bench.bench ("HashMap.Lazy." <> show n) $ Bench.nf (HashMap.Lazy.lookup (n)) (buildN n),
          Bench.bench ("HashMap.Strict." <> show n) $ Bench.nf (HashMap.Strict.lookup (n)) (buildN n),
          Bench.bench ("MapBL." <> show n) $ Bench.nf (mylookup (n)) (buildN @MyLib.MapBL n),
          Bench.bench ("MapBB." <> show n) $ Bench.nf (MyLib.lookup (n)) (buildN @MyLib.MapBB n),
          Bench.bench ("MapBU." <> show n) $ Bench.nf (MyLib.lookup (n)) (buildN @MyLib.MapBU n),
          Bench.bench ("MapUL." <> show n) $ Bench.nf (MyLib.lookup (n)) (buildN @MyLib.MapUL n),
          Bench.bench ("MapUB." <> show n) $ Bench.nf (MyLib.lookup (n)) (buildN @MyLib.MapUB n),
          Bench.bench ("MapUU." <> show n) $ Bench.nf (MyLib.lookup (n)) (buildN @MyLib.MapUU n)
        ]
      | n <- powersOfTwo
      ]

foldrBench :: Bench.Benchmark
foldrBench =
  Bench.bgroup "Foldr (+) 0 on a hashmap of size N" $
    let {-# INLINE bench #-}
        bench :: forall map. (NFData (map Int Int), IsList (map Int Int), Item (map Int Int) ~ (Int, Int), Foldable (map Int)) => Int -> Bench.Benchmarkable
        bench n = Bench.nf (foldr (+) 0) (buildN @map n)
     in mconcat
          [ [ Bench.bench ("HashMap." <> show n) $ bench @HashMap n,
              Bench.bench ("MapBL." <> show n) $ bench @MyLib.MapBL n,
              Bench.bench ("MapBB." <> show n) $ bench @MyLib.MapBB n,
              Bench.bench ("MapUL." <> show n) $ bench @MyLib.MapUL n,
              Bench.bench ("MapUB." <> show n) $ bench @MyLib.MapUB n
            ]
          | n <- ([0] <> powersOfTwo)
          ]

-- myinsert :: Int -> Int -> MyLib.MapBL Int Int -> MyLib.MapBL Int Int
-- {-# NOINLINE myinsert #-}
-- myinsert = MyLib.insert

-- theirinsert :: Int -> Int -> HashMap Int Int -> HashMap Int Int
-- {-# NOINLINE theirinsert #-}
-- theirinsert = HashMap.Lazy.insert

mylookup :: Int -> MyLib.MapBL Int Int -> Maybe Int
{-# NOINLINE mylookup #-}
mylookup = MyLib.lookup

-- theirlookup :: Int -> HashMap Int Int -> Maybe Int
-- {-# NOINLINE theirlookup #-}
-- theirlookup = HashMap.Lazy.lookup

buildN :: forall map. (NFData (map Int Int), IsList (map Int Int), Item (map Int Int) ~ (Int, Int)) => Int -> (map Int Int)
buildN n = force $ fromList [(x, x) | x <- [0 .. (n - 1)]]

powersOfTwo :: [Int]
powersOfTwo = [2 ^ (x :: Int) | x <- [0 .. 10]]

{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-stg-from-core -ddump-cmm -ddump-to-file #-}
import Test.Tasty.Bench qualified as Bench
import GHC.IsList
import Control.DeepSeq (force, NFData)
import MyLib qualified
import Data.HashMap.Lazy (HashMap)
import Data.HashMap.Lazy qualified as HashMap.Lazy
import Data.HashMap.Lazy qualified as HashMap.Strict

main :: IO ()
main = Bench.defaultMain 
  [ insertBench
  ]

insertBench :: Bench.Benchmark
insertBench = Bench.bgroup "Inserting a single (new) element in a hashmap of size N" $
  mconcat [
    [ Bench.bench ("HashMap.Lazy." <> show n) $ Bench.nf (theirinsert n n) (buildN n)
    , Bench.bench ("HashMap.Strict." <> show n) $ Bench.nf (HashMap.Strict.insert n n) (buildN n)
    , Bench.bench ("MapBL." <> show n) $ Bench.nf (myinsert n n) (buildN @(MyLib.MapBL _ _) n)
    , Bench.bench ("MapBB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @(MyLib.MapBB _ _) n)
    , Bench.bench ("MapBU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @(MyLib.MapBU _ _) n)
    , Bench.bench ("MapUL." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @(MyLib.MapUL _ _) n)
    , Bench.bench ("MapUB." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @(MyLib.MapUB _ _) n)
    , Bench.bench ("MapUU." <> show n) $ Bench.nf (MyLib.insert n n) (buildN @(MyLib.MapUU _ _) n)
    ] | n <- ([0] <> powersOfTwo)]

myinsert :: Int -> Int -> MyLib.MapBL Int Int -> MyLib.MapBL Int Int
{-# NOINLINE myinsert #-}
myinsert = MyLib.insert

theirinsert :: Int -> Int -> HashMap Int Int -> HashMap Int Int
{-# NOINLINE theirinsert #-}
theirinsert = HashMap.Lazy.insert

buildN :: (NFData map, IsList map, Item map ~ (Int, Int)) => Int -> map
buildN n = force $ fromList [(x, x) | x <- [0..(n - 1)]]

powersOfTwo :: [Int]
powersOfTwo = [2 ^ (x :: Int) | x <- [0..10]]

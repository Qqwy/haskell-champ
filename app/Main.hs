module Main where

import Champ.Internal qualified (someFunc)

main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  Champ.Internal.someFunc

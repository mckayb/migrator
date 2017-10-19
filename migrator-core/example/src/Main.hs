module Main where

import Migrator.Core.Language (test)

main :: IO ()
main = do
  test
  putStrLn "hello world"

module Main where

import Prelude (IO, (>>=))
import System.Environment (getArgs)
import Migrator.Discover.Run (run)

main :: IO ()
main = getArgs >>= run

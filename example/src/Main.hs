{-# OPTIONS_GHC -F -pgmF migrator-discover #-}

module Main where

main :: IO ()
main = migrator_run_migrations
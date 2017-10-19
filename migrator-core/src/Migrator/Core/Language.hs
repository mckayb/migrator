{-# LANGUAGE DataKinds #-}

module Migrator.Core.Language where

test :: IO ()
test = putStrLn "hello world"

data Object = Table | Column

data Expr a = 
  Create a | Alter a | Drop a

type Migration = Expr Object

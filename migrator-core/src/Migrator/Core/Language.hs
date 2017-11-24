{-# LANGUAGE OverloadedStrings #-}

module Migrator.Core.Language where

import Prelude (String, IO)
data Migration = Sql String String
type Schema = [Migration]

data Direction = Up | Down
type Query = String -> IO ()

run :: Query -> Direction -> Migration -> IO ()
run q Up (Sql up _) = q up
run q Down (Sql _ down) = q down

-- Need a few things:
-- First, I need a way to run queries on the database
-- Second, I need a way to keep track of which queries have already been ran

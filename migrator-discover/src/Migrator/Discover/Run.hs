{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Migrator.Discover.Run (run) where

import Prelude (String, IO, ShowS, showString, shows, fmap, (.))
import System.Environment (getProgName)
import System.IO (FilePath, hPutStrLn, hPrint, stderr, writeFile)
import System.Directory (listDirectory)
import System.FilePath.Posix (dropExtension)
import Data.String (IsString, fromString)
import Data.List (sort)
import Debug.Trace (trace)

instance IsString ShowS where
  fromString = showString

run :: [String] -> IO ()
run args = do
    hPrint stderr args
    name <- getProgName
    migrationFiles <- listDirectory "./migrations"
    let modules = fmap dropExtension (sort migrationFiles)
    case args of
        src : _ : dst : args' -> do
            hPutStrLn stderr name
            hPrint stderr src
            hPrint stderr modules
            hPrint stderr args'
            writeFile dst (mkModule src modules)
        _ -> hPutStrLn stderr "Error"

mkModule :: FilePath -> [String] -> String
mkModule src migrations =
    ( "{-# LINE 1 " . shows src . " #-}\n"
    . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
    . showString "module Main where\n"
    . showString (migrationImports migrations)
    . showString "main :: IO ()\n"
    . showString "main = putStrLn \"Preprocessed! Yay Again Dood22!\""
    ) "\n"
  where
    migrationImports migs = let x = trace "BLOOBITY" in ""

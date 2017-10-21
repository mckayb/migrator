{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Migrator.Discover.Run (run) where

import Prelude (String, IO, ShowS, showString, shows, fmap, foldl, (.))
import System.IO (FilePath, hPutStrLn, stderr, writeFile)
import System.Directory (listDirectory)
import System.FilePath.Posix (takeDirectory, dropExtension)
import Data.String (IsString, fromString)
import Data.List (sort)
import Data.Semigroup ((<>))

instance IsString ShowS where
  fromString = showString

run :: [String] -> IO ()
run args =
    case args of
        -- src : _ : dst : args' -> do
        src : _ : dst : _ -> do
            migrationFiles <- listDirectory (takeDirectory src <> "/Migrations")
            let modules = fmap dropExtension (sort migrationFiles)
            -- hPutStrLn stderr (mkModule src modules)
            writeFile dst (mkModule src modules)
        _ -> hPutStrLn stderr "Error"

mkModule :: FilePath -> [String] -> String
mkModule src migrations =
    ( "{-# LINE 1 " . shows src . " #-}\n"
    . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
    . showString "module Main where\n"
    . showString (migrationImports migrations)
    . showString "\n"
    . showString "main :: IO ()\n"
    . showString "main = do\n"
    . showString (migrationBodies migrations)
    ) "\n"

migrationImports :: [String] -> String
migrationImports = migModuleText imports
  where imports mig = "import qualified Migrations." <> mig <> " (main)\n"

migrationBodies :: [String] -> String
migrationBodies = migModuleText body
  where body mig = "  Migrations." <> mig <> ".main\n"

migModuleText :: (String -> String) -> [String] -> String
migModuleText f migs = foldl (<>) "" (fmap f migs)

{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Migrator.Discover.Run (run) where

import Prelude (Eq, Show, String, IO, ShowS, showString, shows, fmap, foldl, (.))
import System.IO (FilePath, hPutStrLn, hPrint, stderr, writeFile)
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
        src : _ : dst : args' -> do
            migrationFiles <- listDirectory (takeDirectory src <> "/Migrations")
            let modules = fmap dropExtension (sort migrationFiles)
            hPrint stderr mainBody
            hPutStrLn stderr (mkModule src modules)
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
    . showString "  "
    . showString (migrationBodies migrations)
    ) "\n"

qualifiedImport :: String -> String
qualifiedImport mig = "import qualified Migrations." <> mig <> " (main)\n"

migrationImports :: [String] -> String
migrationImports migs = foldl (<>) "" (fmap qualifiedImport migs)

migrationBody :: String -> String
migrationBody mig = "Migrations." <> mig <> ".main"

migrationBodies :: [String] -> String
migrationBodies migs = foldl (<>) "" (fmap migrationBody migs)

mainBody :: String
mainBody = "main = do\n"
    <> "putStrLn \"Preprocessed!\""

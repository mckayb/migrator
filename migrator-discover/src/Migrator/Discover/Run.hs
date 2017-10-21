{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Migrator.Discover.Run (run) where

import Prelude (String, IO, ShowS, showString, shows, fmap, readFile, unlines, lines, filter, not, (/=), (&&), (.))
import System.IO (FilePath, hPutStrLn, stderr, writeFile)
import System.Directory (listDirectory)
import System.FilePath.Posix (takeDirectory, dropExtension)
import Data.String (IsString, fromString)
import Data.List (sort)
import Data.Semigroup ((<>))
import Data.Text (pack, isPrefixOf)

instance IsString ShowS where
  fromString = showString

importsFromBody :: [String] -> [String]
importsFromBody = filter (\x -> x /= "" && "import " `isPrefixOf` pack x && not ("module " `isPrefixOf` pack x))

nonImportsFromBody :: [String] -> [String]
nonImportsFromBody = filter (\x -> x /= "" && not ("{-" `isPrefixOf` pack x) && not ("import " `isPrefixOf` pack x) && not ("module " `isPrefixOf` pack x))

moduleFromBody :: [String] -> [String]
moduleFromBody = filter (\x -> "module " `isPrefixOf` pack x)

run :: [String] -> IO ()
run args =
    case args of
        src : _ : dst : _ -> do
            -- 1) Read the input file
            linesInput <- fmap lines (readFile src)

            -- 2) Separate the input file by imports and body
            let moduleName = unlines (moduleFromBody linesInput)
            let originalImports = importsFromBody linesInput
            let originalBody = nonImportsFromBody linesInput

            -- 3) Gather the migration imports and body
            migrationFiles <- listDirectory (takeDirectory src <> "/Migrations")
            let migrationModules = fmap (("Migrations." <>) . dropExtension) (sort migrationFiles)

            let migrationImports = fmap (("import qualified " <>) . (<> " (main)")) migrationModules
            let migrationBody = ["migrator_run_migrations :: IO ()", "migrator_run_migrations=do"] <> fmap (("  " <>) . (<> ".main")) migrationModules

            -- 4) Combine the original and migration parts
            let imports = unlines (originalImports <> migrationImports)
            let body = unlines (originalBody <> migrationBody)

            -- 5) Put it all together
            writeFile dst (mkModule src moduleName imports body)
        _ -> hPutStrLn stderr "Error"

mkModule :: FilePath -> String -> String -> String -> String
mkModule src moduleName imports body =
    ( "{-# LINE 1 " . shows src . " #-}\n"
    . showString "{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}\n"
    . showString moduleName
    . showString imports
    . showString body
    ) "\n"
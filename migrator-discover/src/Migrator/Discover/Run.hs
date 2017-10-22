{-# LANGUAGE FlexibleInstances, FlexibleContexts, OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Migrator.Discover.Run (run) where

import Prelude (String, IO, ShowS, showString, shows, fmap, readFile, unlines, null, lines, filter, not, (/=), (&&), (.))
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
            let moduleNameArr = moduleFromBody linesInput
            let moduleName = if null moduleNameArr then "module Main where \n" else unlines moduleNameArr
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
    . mainImports
    . showString body
    . mainBody
    ) "\n"

mainBody :: String -> String
mainBody = 
      showString "main :: IO ()\n"
    . showString "main = do\n"
    . showString "  cmdLineArgs <- getArgs\n"
    . showString "  case parseArgs cmdLineArgs of\n"
    . showString "    Help -> putStrLn \"Help\"\n"
    . showString "    Version -> putStrLn \"Version\"\n"
    . showString "    Cmd Options {cmd = cmd, args = args, flags = flags} ->\n"
    . showString "      case cmd of\n"
    . showString "      \"migrate\" -> putStrLn \"Migrating...\"\n"
    . showString "      \"rollback\" -> putStrLn \"Rolling Back...\"\n"
    . showString "      \"reset\" -> putStrLn \"Resetting...\"\n"
    . showString "      \"_\" -> putStrLn \"Help\"\n"
    . showString "    Error -> do\n"
    . showString "      putStrLn \"Error\"\n"
    . showString "      putStrLn \"Help\"\n\n"
    . showString "parseArgs :: [String] -> ParseResult\n"
    . showString "parseArgs args = \n"
    . showString "  case args of\n"
    . showString "    [] -> Help\n"
    . showString "    [\"--help\"] -> Help\n"
    . showString "    [\"--version\"] -> Version\n"
    . showString "    _ ->\n"
    . showString "      if null cmds then Error else Cmd Options {cmd = head cmds, args = tail cmds, flags = flags}\n"
    . showString "  where\n"
    . showString "    isPrefix pre str = take (length pre) str == pre\n"
    . showString "    cmds = filter (not . isPrefix \"-\") args\n"
    . showString "    flags = filter (isPrefix \"-\") args\n\n"
    . showString "data Options = Options\n"
    . showString "  { cmd :: String\n"
    . showString "  , args :: [String]\n"
    . showString "  , flags :: [String]\n"
    . showString "  } deriving (Show, Eq)\n\n"
    . showString "data ParseResult = Help | Version | Cmd Options | Error\n"

mainImports :: String -> String
mainImports = showString "import System.Environment (getArgs)\n"
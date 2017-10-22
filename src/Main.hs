{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Prelude ( Show, Eq, IO, String , putStrLn, print, null
               , filter, head, tail, take, length, filter, not
               , (.), (==)
               )
import System.Environment (getArgs)
import Migrator.Core.Language (test)

main :: IO ()
main = do
  cmdLineArgs <- getArgs
  case parseArgs cmdLineArgs of
    Help -> putStrLn "Help"
    Version -> putStrLn "Version"
    Cmd Options {cmd, args, flags} ->
      case cmd of
        "migrate" -> putStrLn "Migrating"
        "rollback" -> putStrLn "Rolling Back"
        "reset" -> putStrLn "Reset"
        _ -> do
          print cmdLineArgs
          putStrLn "Help"
    Error -> do
      putStrLn "Error"
      putStrLn "Help"

parseArgs :: [String] -> ParseResult
parseArgs args =
  case args of
    [] -> Help
    ["--help"] -> Help
    ["--version"] -> Version
    _ ->
      if null cmds
        then Error
        else Cmd Options {cmd = head cmds, args = tail cmds, flags}
  where
    isPrefix pre str = take (length pre) str == pre
    cmds = filter (not . isPrefix "-") args
    flags = filter (isPrefix "-") args

data Options = Options
  { cmd :: String
  , args :: [String]
  , flags :: [String]
  } deriving (Show, Eq)

data ParseResult
  = Help
  | Version
  | Cmd Options
  | Error

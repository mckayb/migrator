module Migrator.Discover.Parse where

import Prelude (IO, String, putStrLn, null, head, tail, take, filter, not, length, (==), (.))
import Migrator.Discover.Data ( Options(Options)
                              , ParseResult(Help, Version, Cmd, Error)
                              , cmd, args, flags
                              )

parse :: [String] -> IO ()
parse args' =
  case parseArgs args' of
    Help -> putStrLn "Help"
    Version -> putStrLn "Version"
    Cmd Options {cmd = cmd', args = args'', flags = flags'} ->
      case cmd' of
      "migrate" -> putStrLn "Migrating..."
      "rollback" -> putStrLn "Rolling Back..."
      "reset" -> putStrLn "Resetting..."
      _ -> putStrLn "Help"
    Error -> do
      putStrLn "Error"
      putStrLn "Help"

parseArgs :: [String] -> ParseResult
parseArgs args' =
  case args' of
    [] -> Help
    ["--help"] -> Help
    ["--version"] -> Version
    _ ->
      if null cmds then Error else Cmd Options {cmd = head cmds, args = tail cmds, flags = flags'}
  where
    isPrefix pre str = take (length pre) str == pre
    cmds = filter (not . isPrefix "-") args'
    flags' = filter (isPrefix "-") args'

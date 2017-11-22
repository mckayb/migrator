module Migrator.Discover.Data where

data Options = Options
  { cmd :: String
  , args :: [String]
  , flags :: [String]
  } deriving (Show, Eq)

data ParseResult = Help | Version | Cmd Options | Error
  deriving (Show, Eq)
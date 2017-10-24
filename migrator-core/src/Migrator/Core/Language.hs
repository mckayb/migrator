module Migrator.Core.Language where

test :: IO ()
test = putStrLn "hello world"

type ColumnName = String
type TableName = String

data ColumnDef = ColumnDef { columnName :: ColumnName, columnType :: String, columnOpts :: Maybe [String] } deriving (Eq, Show)
data TableDef = TableDef { tableName :: TableName, tableColumns :: [ColumnDef], tableOpts :: Maybe [String] } deriving (Eq, Show)

data Action =
  AddColumn TableName ColumnDef
  | DropColumn TableName ColumnName
  | AddTable TableDef
  | DropTable TableName
  | AlterColumn TableName ColumnName ColumnDef
  | AlterTable TableName TableDef
  deriving (Eq, Show)
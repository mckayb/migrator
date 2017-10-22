{-# LANGUAGE DataKinds #-}

module Migrator.Core.Language where

test :: IO ()
test = putStrLn "hello world"

data CreateObject a =
  CreateDatabase String (Maybe [Options a])
  | CreateTable String (Maybe [Options a])
  | CreateColumn String String (Maybe [Options a])

data DropObject =
  DropDatabase String
  | DropTable String
  | DropColumn String

data Expr a b = 
  Create a b | Alter a b | Drop a

data Options a = Default a | Nullable

type Add = Create
type Modify = Alter


-- mig = Alter (Table "blah") Add (Column "col_name" "col_type" Just [Default "blue", Nullable])
-- mig = Alter (Table "blah") Create (Column "col_name" "col_type" Just [Default "blue", Nullable])
-- mig = Alter (Table "blah") Modify (Column "col_name" "col_type" Just [Default "blue", Nullable])
-- mig = Alter (Table "blah") Drop (Column "col_name")

-- mig = Create (Table "blah" Nothing)
-- mig = Create (Database "blah" Nothing)

-- mig = Drop (Table "blah")
-- mig = Drop (Database "blah")
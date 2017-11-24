module Migrations.Bar where

import Migrator.Core.Language (Migration(Sql))

main :: Migration
main = Sql up down

up :: String
up =
  "create table films (\
    \code char(5) CONSTRAINT firstkey PRIMARY KEY,\
    \title varchar(40) NOT NULL,\
    \did integer NOT NULL,\
    \date_prod date,\
    \kind varchar(10),\
    \len interval hour to minute \
  \)"

down :: String
down = "drop table films"
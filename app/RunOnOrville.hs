module RunOnOrville where

import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O

import           Schema

runMigrations :: O.OrvilleEnv Postgres.Connection -> IO ()
runMigrations env =
  O.runOrville (O.migrateSchema allSchemas) env


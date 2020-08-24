{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
module Main where


import qualified Database.Beam                 as B
import qualified Database.Beam.Sqlite          as B

import           Database.Beam.Migrate          ( CheckedDatabaseSettings
                                                , defaultMigratableDbSettings
                                                )
import           Database.Beam.Migrate.Simple   ( autoMigrate )
import           Database.Beam.Sqlite.Migrate   ( migrationBackend )

import qualified Database.SQLite.Simple        as SL

import           OpenFEC.Beam.Sqlite.CustomFields
                                                ( )
import           OpenFEC.Beam.Types


-- create the migration
openFEC_DbMigratable :: CheckedDatabaseSettings B.Sqlite OpenFEC_DB
openFEC_DbMigratable = defaultMigratableDbSettings

{-
openFEC_DbMigration :: forall syntax be
                       . B.IsSql92DdlCommandSyntax syntax
                    => CheckedDatabaseSettings be OpenFEC_DB
openFEC_DbMigration = defaultMigratableDbSettings @syntax
-}

main :: IO ()
main = do
  conn <- SL.open "../DBs/FEC-test.db"
  B.runBeamSqliteDebug putStrLn conn $ do
    autoMigrate migrationBackend openFEC_DbMigratable -- migrate to current structure, if possible.
    return ()



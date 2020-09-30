module RunOnOrville where

import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O

import           Schema
import           Types

runMigrations :: O.OrvilleEnv Postgres.Connection -> IO ()
runMigrations env =
  O.runOrville (O.migrateSchema allSchemas) env

insertNewRecord :: O.OrvilleEnv Postgres.Connection -> RankRecord () -> IO (RankRecord RankId)
insertNewRecord env rankValue =
  O.runOrville (O.insertRecord rankTable $ rankValue) env

checkAndUpdateTotalRank :: ExtensionNameId
                        -> Rank
                        -> O.OrvilleEnv Postgres.Connection
                        -> IO ()
checkAndUpdateTotalRank extenId (Rank newRankInt) orvilleEnv = do
  mbRankTotalRecord <- findRankTotalRecord extenId orvilleEnv
  case mbRankTotalRecord of
    [] -> do
      putStrLn "No record found"
      _ <- O.runOrville
             (O.insertRecord rankTotalTable $
                RankTotalRecord
                  { rankTotalId = ()
                  , extensionId = extenId
                  , rankCount   = 1
                  , rankSum     = RankSum newRankInt
                  }
             )
             orvilleEnv
      pure ()
    [rankTotalRecord] -> do
      let
        oldRankSum = (rankSumInt $ rankSum rankTotalRecord)
      O.runOrville
        (O.updateRecord
          rankTotalTable
          (rankTotalId rankTotalRecord)
          (rankTotalRecord { rankTotalId = ()
                           , rankCount   = (rankCount rankTotalRecord + 1)
                           , rankSum     = (RankSum $ newRankInt + oldRankSum)
                           }
          )
        )
        orvilleEnv
    _ -> putStrLn "Error multiple records found"

findRankTotalRecord :: ExtensionNameId
                    -> O.OrvilleEnv Postgres.Connection
                    -> IO [RankTotalRecord RankTotalId]
findRankTotalRecord eId orvilleEnv = do
  O.runOrville
    ( O.selectAll
      rankTotalTable
      (O.where_ (extensionIdNameField O..== eId))
    )
    orvilleEnv


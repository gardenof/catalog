{-# LANGUAGE OverloadedStrings #-}
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)

import           Css
import           LanguageExtension
import           Response
import           RunOnOrville
import           Schema
import           SchemaData
import           Types

main :: IO ()
main = do
  orvilleEnv <- createCatalogOrvilleEnv
  runMigrations orvilleEnv
  putStrLn $ "http://localhost:8080/"
  run 8080 (app orvilleEnv)

app :: O.OrvilleEnv Postgres.Connection
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app orvilleEnv request respond = do
  case rawPathInfo request of
    "/"                  -> newMainPath           orvilleEnv respond
    "/NegativeLiterals"  -> languageExtensionPath orvilleEnv respond negativeliteralsInfo Nothing
    "/OverloadedStrings" -> languageExtensionPath orvilleEnv respond overLoadedStringInfo Nothing
    "/ranked"            -> rankPath              orvilleEnv request respond
    "/mainCss"           -> respond mainCssPath
    "/firstInsert"       -> insertPath orvilleEnv respond
    _                    -> respond notFound


newMainPath :: O.OrvilleEnv Postgres.Connection
            -> (Response -> IO ResponseReceived)
            -> IO ResponseReceived
newMainPath orvilleEnv respond = do
  fristTenExtensions <- selectFristTenExtensions orvilleEnv
  respond $ newMain fristTenExtensions

insertPath :: O.OrvilleEnv Postgres.Connection
           -> (Response -> IO ResponseReceived)
           -> IO ResponseReceived
insertPath orvilleEnv respond = do
  _ <- traverse (insertExtension orvilleEnv) firstSetOfExtensions

  respond mainPath

rankTotalAverage :: [RankTotalRecord RankTotalId] -> Float
rankTotalAverage rankRecordList = do
  case rankRecordList of
    [] -> 0
    [(RankTotalRecord _ _ rtrCount rtrSum)] -> do
      let
        totalSum = rankSumInt rtrSum
        totalCount = rankCountInt rtrCount
      (fromIntegral totalSum) / (fromIntegral totalCount)
    _ -> 0

languageExtensionPath :: O.OrvilleEnv Postgres.Connection
                      -> (Response -> IO ResponseReceived)
                      -> LanguageExtension
                      -> Maybe String
                      -> IO ResponseReceived
languageExtensionPath orvilleEnv respond lang mbErrorMessage = do
  foundRankTotalRecord <- findRankTotalRecord (extension lang) orvilleEnv

  case mbErrorMessage of
    Nothing ->
      respond $
        languageExtensionRes
          lang
          (rankTotalAverage foundRankTotalRecord)
    Just eMessage ->
      respond $
        languageExtensionErrorRes
          lang
          (rankTotalAverage foundRankTotalRecord)
          eMessage

indexPath :: O.OrvilleEnv Postgres.Connection
         -> (Response -> IO ResponseReceived)
         -> Maybe String
         -> IO ResponseReceived
indexPath orvilleEnv respond mbErrorMessage = do
  foundRankTotalRecord <- findRankTotalRecord
                            (extension overLoadedStringInfo)
                            orvilleEnv

  case mbErrorMessage of
    Nothing ->
      respond (index $ rankTotalAverage foundRankTotalRecord)
    Just eMessage ->
      respond (indexParameterError (rankTotalAverage foundRankTotalRecord) eMessage)

rankPath :: O.OrvilleEnv Postgres.Connection
         -> Request
         -> (Response -> IO ResponseReceived)
         -> IO ResponseReceived
rankPath orvilleEnv request respond = do
  parsedBody      <- parseRequestBody lbsBackEnd request
  let mbRankValue = lookup "rankSelect" (fst parsedBody)
      mbLEValue   = lookup "exetnsion"  (fst parsedBody)

  case (validRank mbRankValue, validExtension mbLEValue) of
    (Just rankValue, Just langExt) -> do
      _ <- insertNewRecord orvilleEnv (RankRecord () rankValue)
      _ <- checkAndUpdateTotalRank
             langExt
             rankValue
             orvilleEnv
      respond (thankYouRes $ (show $ rankInt rankValue))
    (_, _) ->
      indexPath orvilleEnv respond (Just errorMessage)

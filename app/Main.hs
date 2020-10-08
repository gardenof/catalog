{-# LANGUAGE OverloadedStrings #-}
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)

import           Css
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
    "/"                  -> indexPath     orvilleEnv respond Nothing
    "/NegativeLiterals"  -> extensionPath orvilleEnv respond (ExtensionNameId "NegativeLiterals" ) Nothing
    "/OverloadedStrings" -> extensionPath orvilleEnv respond (ExtensionNameId "OverloadedStrings") Nothing
    "/ranked"            -> rankPath      orvilleEnv request respond
    "/mainCss"           -> respond mainCssPath
    "/firstInsert"       -> insertPath orvilleEnv respond
    _                    -> respond notFound


indexPath :: O.OrvilleEnv Postgres.Connection
          -> (Response -> IO ResponseReceived)
          -> Maybe String
          -> IO ResponseReceived
indexPath orvilleEnv respond maybeMessage = do
  fristTenExtensions <- selectFristTenExtensions orvilleEnv
  case maybeMessage of
    Nothing ->
      respond $ indexRes fristTenExtensions
    Just message ->
      respond $ indexResWithMessage fristTenExtensions message


extensionPath :: O.OrvilleEnv Postgres.Connection
              -> (Response -> IO ResponseReceived)
              -> ExtensionNameId
              -> Maybe String
              -> IO ResponseReceived
extensionPath orvilleEnv respond extensionNameId maybeMessage = do
  mbExetnsionRecord <- selectExtension orvilleEnv extensionNameId
  case (maybeMessage, mbExetnsionRecord) of
    (Nothing, Just exetnsionRecord) -> do
      foundRankTotalRecord
        <- findRankTotalRecord extensionNameId orvilleEnv
      respond $
        extensionRes
          exetnsionRecord
          (rankTotalAverage foundRankTotalRecord)
    (message, _) ->
      indexPath orvilleEnv respond message

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
      indexPath
        orvilleEnv
        respond $ Just $ thanksYouRankMessage rankValue langExt
    (_, _) ->
      indexPath orvilleEnv respond Nothing

thanksYouRankMessage :: Rank -> ExtensionNameId -> String
thanksYouRankMessage (Rank rankVal) (ExtensionNameId extensionNameIdVal) =
  "Thank you for rating "
  <> (show extensionNameIdVal)
  <> " a "
  <> (show rankVal)

insertPath :: O.OrvilleEnv Postgres.Connection
           -> (Response -> IO ResponseReceived)
           -> IO ResponseReceived
insertPath orvilleEnv respond = do
  _ <- traverse (insertExtension orvilleEnv) firstSetOfExtensions
  indexPath orvilleEnv respond Nothing

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

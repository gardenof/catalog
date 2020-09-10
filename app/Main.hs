{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Int (Int32)
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import           Text.Read (readMaybe)

import           Html
import           LanguageExtension
import           Schema
import           Types

main :: IO ()
main = do
  orvilleEnv <- createCatalogOrvilleEnv
  O.runOrville (O.migrateSchema allSchemas) orvilleEnv
  putStrLn $ "http://localhost:8080/"
  run 8080 (app orvilleEnv)

app :: O.OrvilleEnv Postgres.Connection
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app orvilleEnv request respond = do
  case rawPathInfo request of
    "/"           -> indexPath orvilleEnv respond Nothing
    "/plainIndex" -> respond plainIndex
    "/about"      -> respond aboutUs
    "/ranked"     -> rankPath orvilleEnv request respond
    _             -> respond notFound

indexPath :: O.OrvilleEnv Postgres.Connection
         -> (Response -> IO ResponseReceived)
         -> Maybe String
         -> IO ResponseReceived
indexPath orvilleEnv respond mbErrorMessage = do
  rankRecordList <- O.runOrville (O.selectAll rankTable mempty) orvilleEnv
  case mbErrorMessage of
    Nothing ->
      respond (index $ avgRank rankRecordList )
    Just eMessage ->
      respond (indexParameterError (avgRank rankRecordList) eMessage)

rankPath :: O.OrvilleEnv Postgres.Connection
         -> Request
         -> (Response -> IO ResponseReceived)
         -> IO ResponseReceived
rankPath orvilleEnv request respond = do
  parsedBody      <- parseRequestBody lbsBackEnd request
  let mbRankValue = lookup "rankSelect" (fst parsedBody)

  case validRank mbRankValue of
    Nothing ->
      indexPath orvilleEnv respond (Just errorMessage)
    Just rankValue -> do
      _ <- O.runOrville
             (O.insertRecord rankTable $ RankRecord () rankValue)
             orvilleEnv
      _ <- checkAndUpdateTotalRank
             ( extension overLoadedStringInfo )
             rankValue
             orvilleEnv
      respond (thankYouRes $ (show $ rankInt rankValue))

checkAndUpdateTotalRank :: ExtensionId
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



findRankTotalRecord :: ExtensionId
                    -> O.OrvilleEnv Postgres.Connection
                    -> IO [RankTotalRecord RankTotalId]
findRankTotalRecord eId orvilleEnv = do
  O.runOrville
    ( O.selectAll
      rankTotalTable
      (O.where_ (extensionIdField O..== eId))
    )
    orvilleEnv

validRank :: Maybe ByteString -> Maybe Rank
validRank maBs =
  Rank <$> validNumberValue maBs

validNumberValue :: Maybe ByteString -> Maybe Int32
validNumberValue mbByteString =
  case mbByteString of
    Nothing         -> Nothing
    Just byteString -> readMaybe $ unpack byteString

avgRank :: [RankRecord RankId] -> Float
avgRank list =
    (fromIntegral $ sum $ map getRank list) / (fromIntegral $ length list)

getRank :: RankRecord RankId -> Int32
getRank rankRecord =
  rankInt $ rank rankRecord

index :: Float -> Response
index rankAvg = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ libraryView overLoadedStringInfo rankAvg)

indexParameterError :: Float -> String -> Response
indexParameterError rankAvg message = responseLBS
    status422
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ libraryViewError overLoadedStringInfo message rankAvg )

errorMessage :: String
errorMessage =
  "Sorry something went wrong we didn't get your rank, please try again."

plainIndex :: Response
plainIndex = responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, PlainText"

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

aboutUs :: Response
aboutUs = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml aboutUsHtml)

thankYouRes :: String -> Response
thankYouRes selectedRank = do
  responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ thanksForRankHtml selectedRank)

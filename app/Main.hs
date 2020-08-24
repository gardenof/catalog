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

  case validNumberValue mbRankValue of
    Nothing ->
      indexPath orvilleEnv respond (Just errorMessage)
    Just rankValue -> do
      _ <- O.runOrville
             (O.insertRecord rankTable $ RankRecord () (Rank {rankInt = rankValue}))
             orvilleEnv
      respond (thankYouRes $ (show rankValue))

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
    (BHRU.renderHtml $ thanksForRankHtml $ selectedRank)

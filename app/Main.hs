{-# LANGUAGE OverloadedStrings #-}
import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Int (Int32)
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import           Schema
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU

import           LanguageExtension
import           Html

main :: IO ()
main = do
  orvilleEnv <- sqlEnv
  O.runOrville (O.migrateSchema allSchemas) orvilleEnv
  putStrLn $ "http://localhost:8080/"
  run 8080 (app orvilleEnv)

app :: O.OrvilleEnv Postgres.Connection
    -> Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app orvilleEnv request respond = do
  case rawPathInfo request of
    "/"           -> mainPath orvilleEnv respond
    "/plainIndex" -> respond plainIndex
    "/about"      -> respond aboutUs
    "/ranked"     -> rankPath orvilleEnv request respond
    _             -> respond notFound

mainPath :: O.OrvilleEnv Postgres.Connection
         -> (Response -> IO ResponseReceived)
         -> IO ResponseReceived
mainPath orvilleEnv respond = do
  rankRecordList <- O.runOrville (O.selectAll rankTable mempty) orvilleEnv
  respond (index $ avgRank rankRecordList )

rankPath :: O.OrvilleEnv Postgres.Connection
         -> Request
         -> (Response -> IO ResponseReceived)
         -> IO ResponseReceived
rankPath orvilleEnv request respond = do
  parsedBody      <- parseRequestBody lbsBackEnd request
  let mbRankValue = lookup "rankSelect" (fst parsedBody)

  case mbRankValue of
    Nothing ->
      respond notFound

    Just rankValue -> do
      let rankValueInt = (read(unpack rankValue)::Int32)
      _ <- O.runOrville
            (O.insertRecord rankTable $ RankRecord () (Rank {rankInt = rankValueInt}))
              orvilleEnv
      respond (thankYouRes $ rankValue)

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

thankYouRes :: ByteString -> Response
thankYouRes selectedRank = do
  responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ thanksForRankHtml $ unpack selectedRank)

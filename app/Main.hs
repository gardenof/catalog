{-# LANGUAGE OverloadedStrings #-}
import qualified Database.HDBC.PostgreSQL as Postgres
import qualified Database.Orville.PostgreSQL as O
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd)
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU

import           Html
import           LanguageExtension
import           Schema
import           Types
import           RunOnOrville

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
    "/"                  -> respond mainPath
    "/NegativeLiterals"  -> languageExtensionPath orvilleEnv respond negativeliteralsInfo Nothing
    "/OverloadedStrings" -> languageExtensionPath orvilleEnv respond overLoadedStringInfo Nothing
    "/about"             -> respond aboutUs
    "/ranked"            -> rankPath orvilleEnv request respond
    "/mainCss"           -> respond mainCssPath
    _                    -> respond notFound

mainPath :: Response
mainPath = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml mainHtml )

mainCssPath :: Response
mainCssPath = responseLBS
    status200
    [("Content-Type", "text/css")]
    " h2 {font-size: 34px;} \
    \ \
    \ h3 {font-size: 28px}\
    \ \
    \ h2, h3 { font-family: inherit ;\
    \ margin-top: 22px; margin-bottom: 11px; font-weight: 500; line-height: 1.1; color: inherit }\
    \ .Extension { padding-left: 15px; padding-right: 15px }\
    \ "

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

languageExtensionRes :: LanguageExtension -> Float -> Response
languageExtensionRes lang rankAvg = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ languageExtensionView lang rankAvg)

languageExtensionErrorRes :: LanguageExtension -> Float -> String -> Response
languageExtensionErrorRes lang rankAvg message = responseLBS
    status422
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ languageExtensionErrorView lang message rankAvg )

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
      _ <- O.runOrville
             (O.insertRecord rankTable $ RankRecord () rankValue)
             orvilleEnv
      _ <- checkAndUpdateTotalRank
             langExt
             rankValue
             orvilleEnv
      respond (thankYouRes $ (show $ rankInt rankValue))
    (_, _) ->
      indexPath orvilleEnv respond (Just errorMessage)

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

index :: Float -> Response
index rankAvg = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ languageExtensionView overLoadedStringInfo rankAvg)

indexParameterError :: Float -> String -> Response
indexParameterError rankAvg message = responseLBS
    status422
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ languageExtensionErrorView overLoadedStringInfo message rankAvg )

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

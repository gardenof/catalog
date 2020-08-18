{-# LANGUAGE OverloadedStrings #-}
import           Schema
import           Data.ByteString.Char8 (ByteString, unpack)
import qualified Database.Orville.PostgreSQL as O
import           Network.HTTP.Types
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.Wai.Parse
import           Network.Wai.Parse (parseRequestBody)
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import           Data.Int (Int32)

main :: IO ()
main = do
  orvilleEnv <- sqlEnv
  O.runOrville (O.migrateSchema allSchemas) orvilleEnv
  putStrLn $ "http://localhost:8080/"
  run 8080 app

app :: Request
    -> (Response -> IO ResponseReceived)
    -> IO ResponseReceived
app request respond = do
  case rawPathInfo request of
    "/"           -> mainPath respond
    "/plainIndex" -> respond plainIndex
    "/about"      -> respond aboutUs
    "/ranked"     -> rankPath request respond
    _             -> respond notFound

mainPath :: (Response -> IO ResponseReceived) -> IO ResponseReceived
mainPath respond = do
  orvilleEnv     <- sqlEnv
  rankRecordList <- O.runOrville (O.selectAll rankTable mempty) orvilleEnv
  respond (index $ avgRank rankRecordList )

rankPath :: Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived
rankPath request respond = do
  orvilleEnv      <- sqlEnv
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

aboutUsHtml :: H.Html
aboutUsHtml = H.docTypeHtml $ do
    H.head $ do
        H.title "About Us"
    H.body $ do
        H.p "About Us Page"
        H.p "trying to provide information"

thanksForRankHtml :: String -> H.Html
thanksForRankHtml rankValue = H.docTypeHtml $ do
  H.head $ do
    H.title "Thank You"
  H.body $ do
    H.p "Thank you for Rank"
    H.p $ H.toHtml $ "Your ranked it a " <> rankValue

libraryView :: HaskellLanguage -> Float -> H.Html
libraryView hl avg = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ title hl
  H.body $ do
    H.p $ H.toHtml $ "Name : " <> (title hl)
    H.p $ H.toHtml $ "Descriptiion : " <> (description hl)
    H.p $ H.toHtml $ "Ussage : " <> (ussage hl)
    H.p $ H.toHtml $ "Rank Avg : " <> (show avg)
    H.p "Url : "
    H.a $ H.toHtml (url hl)
    H.p rankSelect

rankSelect :: H.Html
rankSelect =
  H.body $ do
    H.label $ H.toHtml ("Rank this page: "::String)
    H.form H.! A.action "/ranked" H.! A.method "post" $ do
      H.select H.! A.name "rankSelect" $ do
        H.option H.! A.value "5" $ H.toHtml ("5 star"::String)
        H.option H.! A.value "4" $ H.toHtml ("4 star"::String)
        H.option H.! A.value "3" $ H.toHtml ("3 star"::String)
        H.option H.! A.value "2" $ H.toHtml ("2 star"::String)
        H.option H.! A.value "1" $ H.toHtml ("1 star"::String)
      H.input H.! A.type_ "submit"

data HaskellLanguage = HaskellLanguage
  { title       :: String
  , description :: String
  , ussage      :: String
  , url         :: String
  }

overLoadedStringInfo :: HaskellLanguage
overLoadedStringInfo =
  HaskellLanguage
    { title       = "OverloadedStrings"
    , description = "GHC supports overloaded string literals."
    , ussage      = "{-# LANGUAGE OverloadedStrings #-}"
    , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"
    }

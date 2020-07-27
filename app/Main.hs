{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)
import Network.Wai.Parse

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import Data.ByteString.Char8 (unpack)
import Network.Wai.Parse (parseRequestBody)

import Schema (sqlEnv, allSchemas)
import qualified  Database.Orville.PostgreSQL as O
import qualified  Database.Orville.PostgreSQL.Raw as ORaw
import qualified  Database.HDBC.PostgreSQL as Postgres

main :: IO ()
main = do
  orvilleEnv <- sqlEnv
  O.runOrville (O.migrateSchema allSchemas) orvilleEnv
  putStrLn $ "http://localhost:8080/"
  run 8080 (app orvilleEnv)

app :: O.OrvilleEnv Postgres.Connection -> Application
app orvilleEnv request respond = do
  parsedBody <- parseRequestBody lbsBackEnd request

  --to stop the free twice problem
  O.runOrville (ORaw.selectSql "SELECT 1" [] (pure ())) orvilleEnv

  respond $
    case rawPathInfo request of
      "/"           -> index
      "/plainIndex" -> plainIndex
      "/about"      -> aboutUs
      "/ranked"     -> (thankYouRes $ fst parsedBody)
      _             -> notFound

index :: Response
index = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ libraryView overLoadedStringInfo)

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

thankYouRes :: [Param] -> Response
thankYouRes params = do
  let mbRankValue = lookup "rankSelect" params
  case mbRankValue of
    Nothing ->
      notFound
    Just rankValue ->
      responseLBS
        status200
        [("Content-Type", "text/html")]
        (BHRU.renderHtml $ thanksForRankHtml $ unpack rankValue)

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

libraryView :: HaskellLanguage -> H.Html
libraryView hl = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ title hl
  H.body $ do
    H.p $  H.toHtml $ "Name : " <> (title hl)
    H.p $  H.toHtml $ "Descriptiion : " <> (description hl)
    H.p $  H.toHtml $ "Ussage : " <> (ussage hl)
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

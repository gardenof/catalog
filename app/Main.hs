{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app

app :: Application
app request respond = respond $
  case rawPathInfo request of
    ""       -> index
    "/"       -> index
    "/raw/"   -> plainIndex
    "/blaze/" -> blazeIndex
    "/about/" -> aboutUs
    _         -> notFound

index :: Response
index = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ mainTwo overLoadedStringInfo)

plainIndex :: Response
plainIndex = responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, PlainText"

blazeIndex :: Response
blazeIndex = responseLBS
    status200
    [("Content-Type", "text/html")]
    numbersToText

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

numbersToText :: BSL.ByteString
numbersToText =
  BHRU.renderHtml (numbers 5)

numbers :: Int -> H.Html
numbers n = H.docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    H.body $ do
        H.p "A list of natural numbers:"
        H.ul $ forM_ [1 .. n] (H.li . H.toHtml)

aboutUsHtml :: H.Html
aboutUsHtml = H.docTypeHtml $ do
    H.head $ do
        H.title "About Us"
    H.body $ do
        H.p "trying to provide information"

mainTwo :: HaskellLanguage -> H.Html
mainTwo hl = H.docTypeHtml $ do
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
    H.select $ do
      H.option $ H.toHtml ("5 star"::String)
      H.option $ H.toHtml ("4 star"::String)
      H.option $ H.toHtml ("3 star"::String)
      H.option $ H.toHtml ("2 star"::String)
      H.option $ H.toHtml ("1 star"::String)
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
    "OverloadedStrings"
    "GHC supports overloaded string literals."
    "{-# LANGUAGE OverloadedStrings #-}"
    "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"




{-# LANGUAGE OverloadedStrings #-}
import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp (run)

import Control.Monad (forM_)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import qualified Data.Text as T
import qualified Data.ByteString.Lazy as BSL


app :: Application
app _ respond = do
    putStrLn "I've done some IO here"
    respond $ responseLBS
        status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8080/"
    run 8080 app3

appTwo :: Application
appTwo _ respond = respond index

index :: Response
index = responseFile
    status200
    [("Content-Type", "text/html")]
    "index.html"
    Nothing

app3 :: Application
app3 request respond = respond $
  case rawPathInfo request of
    "/"       -> index
    "/raw/"   -> plainIndex
    "/blaze/" -> blazeIndex
    _         -> notFound

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

numbersToText :: BSL.ByteString
numbersToText =
  BHRU.renderHtml (numbers 5)

--Blaze
numbers :: Int -> H.Html
numbers n = H.docTypeHtml $ do
    H.head $ do
        H.title "Natural numbers"
    H.body $ do
        H.p "A list of natural numbers:"
        H.ul $ forM_ [1 .. n] (H.li . H.toHtml)

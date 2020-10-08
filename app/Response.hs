{-# LANGUAGE OverloadedStrings #-}
module Response where

import           Network.HTTP.Types
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import           Network.Wai

import           Html
import           Types

indexRes :: [ExtensionRecord ExtensionId] -> Response
indexRes extensions = responseLBS
  status200
  [("Content-Type", "text/html")]
  (BHRU.renderHtml (indexHtml extensions))

indexResWithMessage :: [ExtensionRecord ExtensionId] -> String -> Response
indexResWithMessage extensions message = responseLBS
  status200
  [("Content-Type", "text/html")]
  (BHRU.renderHtml (indexHtmlWithMessage extensions message))

extensionRes :: ExtensionRecord ExtensionId -> Float -> Response
extensionRes extensionRecord rankAvg = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ extensionView extensionRecord rankAvg)

notFound :: Response
notFound = responseLBS
    status404
    [("Content-Type", "text/plain")]
    "404 - Not Found"

thankYouRes :: String -> Response
thankYouRes selectedRank = do
  responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ thanksForRankHtml selectedRank)

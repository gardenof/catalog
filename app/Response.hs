{-# LANGUAGE OverloadedStrings #-}
module Response where

import           Network.HTTP.Types
import qualified Text.Blaze.Html.Renderer.Utf8 as BHRU
import           Network.Wai

import           Html
import           LanguageExtension
import           Types

newMain :: [ExtensionRecord ExtensionId] -> Response
newMain extensions = responseLBS
  status200
  [("Content-Type", "text/html")]
  (BHRU.renderHtml (newMainHtml extensions))

mainPath :: Response
mainPath = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml mainHtml )

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

index :: Float -> Response
index rankAvg = responseLBS
    status200
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $ languageExtensionView overLoadedStringInfo rankAvg)

indexParameterError :: Float -> String -> Response
indexParameterError rankAvg message = responseLBS
    status422
    [("Content-Type", "text/html")]
    (BHRU.renderHtml $
      languageExtensionErrorView overLoadedStringInfo message rankAvg )

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

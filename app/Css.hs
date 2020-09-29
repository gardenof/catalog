{-# LANGUAGE OverloadedStrings #-}
module Css where

import           Network.Wai
import           Network.HTTP.Types
import           Clay
import           Data.Text.Lazy.Encoding

mainCssPath :: Response
mainCssPath = responseLBS
    status200
    [("Content-Type", "text/css")]
    (encodeUtf8 (render mainCss))

mainCss :: Css
mainCss = do
  let
    fontSet   = fontFamily ["Raleway", "Helvetica", "Arial"] [monospace]
    marginSet = do
      marginTop $ px 22
      marginBottom $ px 11

  h2 ? do
    (fontSize $ px 34 )
    fontSet
    marginSet
  h3 ? do
    (fontSize $ px 28 )
    fontSet
    marginSet
  ".Extension" ? do
      (paddingRight $ px 15)
      (paddingLeft $ px 15)

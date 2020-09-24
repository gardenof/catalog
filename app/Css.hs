{-# LANGUAGE OverloadedStrings #-}
module Css where

import           Network.Wai
import           Network.HTTP.Types

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


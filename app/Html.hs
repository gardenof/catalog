{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           Types


indexHtml :: [ExtensionRecord ExtensionId] -> H.Html
indexHtml extensions = H.docTypeHtml $ do
  H.head $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "mainCss"
  H.body $ do
    H.div H.! A.class_ "container" $
      H.div H.! A.class_ "Extensions" $ do
        H.h2 $ "Language Extensions"
        extensionDivs extensions

indexHtmlWithMessage ::  [ExtensionRecord ExtensionId] -> String -> H.Html
indexHtmlWithMessage extensions message = H.docTypeHtml $ do
  H.head $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "mainCss"
  H.body $ do
    H.p $ H.toHtml $ "Message = " <> message
    H.div H.! A.class_ "container" $
      H.div H.! A.class_ "Extensions" $ do
        H.h2 $ "Language Extensions"
        extensionDivs extensions

extensionDivs :: [ExtensionRecord ExtensionId] -> H.Html
extensionDivs list =
  case list of
    [] -> H.toHtml (""::String)
    x:xs -> do
      mainExtensionDiv x
      extensionDivs xs

mainExtensionDiv :: ExtensionRecord ExtensionId -> H.Html
mainExtensionDiv extensionRecord = do
  let extensionNameId = (extensionIdToText $ extensionRecordNameId extensionRecord)
      extensionName = (extensionNameToText $ extensionRecordName extensionRecord)
      extensionDescription = (extensionDescriptionToText $ extensionRecordDescription extensionRecord)
  H.toHtml $ H.div H.! A.class_ "Extension" $ do
    H.h3 $ H.a
      H.! A.type_ "hidden" H.! A.name "exetnsion" H.! A.value (H.toValue extensionNameId)
      H.! A.href (H.toValue extensionNameId) $ (H.toHtml extensionName)
    H.p $ H.toHtml extensionDescription

extensionView :: ExtensionRecord ExtensionId -> Float -> H.Html
extensionView extensionRecord avg = H.docTypeHtml $ do
  let extensionNameId      = (extensionIdToText $ extensionRecordNameId extensionRecord)
      extensionName        = (extensionNameToText $ extensionRecordName extensionRecord)
      extensionDescription = (extensionDescriptionToText $ extensionRecordDescription extensionRecord)
  H.head $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "mainCss"
  H.body $ do
    H.toHtml $ H.div H.! A.class_ "container" $ do
      H.h2 $ H.toHtml $  extensionName
      H.toHtml $ H.div H.! A.class_ "Extension" $ do
        H.p $ H.toHtml $ "Rank Avg : " <> (show avg)
        H.p $ H.toHtml $ "Description : " <> extensionDescription
        H.p $ H.toHtml $ "Unique identifier: " <> extensionNameId
        H.p $ rankSelectView extensionRecord

rankSelectView :: ExtensionRecord ExtensionId -> H.Html
rankSelectView extensionRecord = do
  let extensionNameId  = (extensionIdToText $ extensionRecordNameId extensionRecord)
  H.body $ do
    H.label $ H.toHtml ("Rank this page: "::String)
    H.form H.! A.action "/ranked" H.! A.method "post" $ do
      H.select H.! A.name "rankSelect" $ do
        H.option H.! A.value "5" $ H.toHtml ("5 star"::String)
        H.option H.! A.value "4" $ H.toHtml ("4 star"::String)
        H.option H.! A.value "3" $ H.toHtml ("3 star"::String)
        H.option H.! A.value "2" $ H.toHtml ("2 star"::String)
        H.option H.! A.value "1" $ H.toHtml ("1 star"::String)
      H.input H.! A.type_ "hidden" H.! A.name "exetnsion" H.! A.value (H.toValue (extensionNameId))
      H.input H.! A.type_ "submit"

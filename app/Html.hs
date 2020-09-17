{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           LanguageExtension

mainHtml:: H.Html
mainHtml = H.docTypeHtml $ do
  H.head $ do
    H.title "Language Extensions"
  H.body $ do
    H.p $ "Language Extensions"
    H.p $ H.a H.! A.href "OverloadedStrings" $ "Overloaded string literals"
    H.p $ H.a H.! A.href "NegativeLiterals" $ "Negative literals"

aboutUsHtml :: H.Html
aboutUsHtml = H.docTypeHtml $ do
    H.head $ do
        H.title $ "About Us"
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

languageExtensionView :: LanguageExtension -> Float -> H.Html
languageExtensionView lE avg = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ name lE
  H.body $ do
    H.p $ H.toHtml $ "Name : " <> (name lE)
    H.p $ H.toHtml $ "Descriptiion : " <> (description lE)
    H.p $ H.toHtml $ "Ussage : " <> (ussage lE)
    H.p $ H.toHtml $ "Rank Avg : " <> (show avg)
    H.p "Url : "
    H.a $ H.toHtml (url lE)
    H.p $ rankSelect lE

languageExtensionErrorView :: LanguageExtension -> String -> Float -> H.Html
languageExtensionErrorView lE errorString avg = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ name lE
  H.body $ do
    H.p $ H.toHtml $ "Error Message = " <> errorString
    H.p $ H.toHtml $ "Name : " <> (name lE)
    H.p $ H.toHtml $ "Descriptiion : " <> (description lE)
    H.p $ H.toHtml $ "Ussage : " <> (ussage lE)
    H.p $ H.toHtml $ "Rank Avg : " <> (show avg)
    H.p "Url : "
    H.a $ H.toHtml (url lE)
    H.p $ rankSelect lE

rankSelect :: LanguageExtension -> H.Html
rankSelect languageExtension =
  H.body $ do
    H.label $ H.toHtml ("Rank this page: "::String)
    H.form H.! A.action "/ranked" H.! A.method "post" $ do
      H.select H.! A.name "rankSelect" $ do
        H.option H.! A.value "5" $ H.toHtml ("5 star"::String)
        H.option H.! A.value "4" $ H.toHtml ("4 star"::String)
        H.option H.! A.value "3" $ H.toHtml ("3 star"::String)
        H.option H.! A.value "2" $ H.toHtml ("2 star"::String)
        H.option H.! A.value "1" $ H.toHtml ("1 star"::String)
      H.input H.! A.type_ "hidden" H.! A.name "exetnsion" H.! A.value (H.toValue (extensionIdToText $ extension languageExtension))
      H.input H.! A.type_ "submit"

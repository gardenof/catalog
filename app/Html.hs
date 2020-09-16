{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           LanguageExtension

negativeliteralsHtml :: LanguageExtension -> H.Html
negativeliteralsHtml hl = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ name hl
  H.body $ do
    H.p $ H.toHtml $ "Name : " <> (name hl)
    H.p $ H.toHtml $ "Descriptiion : " <> (description hl)
    H.p $ H.toHtml $ "Ussage : " <> (ussage hl)

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

libraryView :: LanguageExtension -> Float -> H.Html
libraryView hl avg = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ name hl
  H.body $ do
    H.p $ H.toHtml $ "Name : " <> (name hl)
    H.p $ H.toHtml $ "Descriptiion : " <> (description hl)
    H.p $ H.toHtml $ "Ussage : " <> (ussage hl)
    H.p $ H.toHtml $ "Rank Avg : " <> (show avg)
    H.p "Url : "
    H.a $ H.toHtml (url hl)
    H.p rankSelect

libraryViewError :: LanguageExtension -> String -> Float -> H.Html
libraryViewError hl errorString avg = H.docTypeHtml $ do
  H.head $ do
    H.title $ H.toHtml $ name hl
  H.body $ do
    H.p $ H.toHtml $ "Error Message = " <> errorString
    H.p $ H.toHtml $ "Name : " <> (name hl)
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

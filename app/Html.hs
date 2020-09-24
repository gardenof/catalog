{-# LANGUAGE OverloadedStrings #-}
module Html where

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import           LanguageExtension

mainHtml:: H.Html
mainHtml = H.docTypeHtml $ do
  H.head $ do
    H.link H.! A.rel "stylesheet" H.! A.type_ "text/css" H.! A.href "mainCss"
  H.body $ do
    H.toHtml $ H.div H.! A.class_ "container" $
      H.toHtml $ H.div H.! A.class_ "Extensions" $ do
        H.h2 $ "Language Extensions"
        H.toHtml $ H.div H.! A.class_ "Extension" $ do
          H.h3 $ H.a H.! A.href "OverloadedStrings" $ "Overloaded string literals"
          H.p
            " GHC supports overloaded string literals.\
            \ Normally a string literal has type String,\
            \ but with overloaded string literals enabled\
            \ (with OverloadedStrings) a string literal has\
            \ type (IsString a) => a.\
            \ "
        H.toHtml $ H.div H.! A.class_ "Extension" $ do
          H.h3 $ H.a H.! A.href "NegativeLiterals" $"Negative literals"
          H.p
            " The literal -123 is, according to Haskell98 and Haskell\
            \ 2010, two tokens, a unary minus (-) and the number 123,\
            \ and is desugared as negate (fromInteger 123). The language\
            \ extension NegativeLiterals causes it to be treated as a\
            \ single token and desugared as fromInteger (-123).\
            \ "

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

module LanguageExtension where

data HaskellLanguage = HaskellLanguage
  { title       :: String
  , description :: String
  , ussage      :: String
  , url         :: String
  }

overLoadedStringInfo :: HaskellLanguage
overLoadedStringInfo =
  HaskellLanguage
    { title       = "OverloadedStrings"
    , description = "GHC supports overloaded string literals."
    , ussage      = "{-# LANGUAGE OverloadedStrings #-}"
    , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"
    }

module LanguageExtension where

data HaskellLanguage = HaskellLanguage
  { description :: String
  , extension   :: String
  , name        :: String
  , url         :: String
  , ussage      :: String
  }

overLoadedStringInfo :: HaskellLanguage
overLoadedStringInfo =
  HaskellLanguage
  { description = "GHC supports overloaded string literals."
  , extension   = "OverloadedStrings"
  , name        = "Overloaded string literals"
  , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"
  , ussage      = "{-# LANGUAGE OverloadedStrings #-}"
  }

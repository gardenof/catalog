module LanguageExtension where

import qualified Data.Text as T

newtype ExtensionId = ExtensionId T.Text
  deriving Show

extensionIdToString :: ExtensionId -> T.Text
extensionIdToString (ExtensionId extensionIdString) =
  extensionIdString

data HaskellLanguage = HaskellLanguage
  { description :: String
  , extension   :: ExtensionId
  , name        :: String
  , url         :: String
  , ussage      :: String
  }

overLoadedStringInfo :: HaskellLanguage
overLoadedStringInfo =
  HaskellLanguage
  { description = "GHC supports overloaded string literals."
  , extension   = ExtensionId (T.pack "OverloadedStrings")
  , name        = "Overloaded string literals"
  , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"
  , ussage      = "{-# LANGUAGE OverloadedStrings #-}"
  }

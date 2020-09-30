module LanguageExtension where

import qualified Data.Text as T
import           Types

extensionIdToText :: ExtensionNameId -> T.Text
extensionIdToText (ExtensionNameId extensionIdText) =
  extensionIdText

data LanguageExtension = LanguageExtension
  { description :: String
  , extension   :: ExtensionNameId
  , name        :: String
  , url         :: String
  , ussage      :: String
  }

overLoadedStringInfo :: LanguageExtension
overLoadedStringInfo =
  LanguageExtension
  { description = "GHC supports overloaded string literals."
  , extension   = ExtensionNameId (T.pack "OverloadedStrings")
  , name        = "Overloaded string literals"
  , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/overloaded_strings.html"
  , ussage      = "{-# LANGUAGE OverloadedStrings #-}"
  }

negativeliteralsInfo :: LanguageExtension
negativeliteralsInfo =
  LanguageExtension
  { description = "Enable negative numeric literals."
  , extension   = ExtensionNameId (T.pack "NegativeLiterals")
  , name        = "Negative literals"
  , url         = "https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/negative_literals.html"
  , ussage      = "{-# LANGUAGE NegativeLiterals #-}"
  }

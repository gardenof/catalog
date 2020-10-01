module SchemaData where

import qualified  Data.Text as T

import            Types

firstSetOfExtensions :: [ExtensionRecord ()]
firstSetOfExtensions =
  [overLoadedStringExtension, negativeLiteralsExtension]

overLoadedStringExtension :: ExtensionRecord ()
overLoadedStringExtension =
  ExtensionRecord
    { extensionRecordId
        = ()
    , extensionRecordNameId
        = ExtensionNameId (T.pack "OverloadedStrings")
    , extensionRecordName
        = ExtensionName (T.pack "Overloaded string literals")
    , extensionRecordDescription
        = ExtensionDescription (T.pack "GHC supports overloaded string literals.")
    }

negativeLiteralsExtension :: ExtensionRecord ()
negativeLiteralsExtension =
  ExtensionRecord
    { extensionRecordId
        = ()
    , extensionRecordNameId
        = ExtensionNameId (T.pack "NegativeLiterals")
    , extensionRecordName
        = ExtensionName (T.pack "Negative literals")
    , extensionRecordDescription
        = ExtensionDescription (T.pack "Enable negative numeric literals.")
    }

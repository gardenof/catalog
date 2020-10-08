{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import           Data.ByteString.Char8 (ByteString, unpack)
import           Data.Int (Int32)
import           Text.Read (readMaybe)
import qualified Data.Text as T

data RankRecord key = RankRecord
  { rankId :: key
  , rank   :: Rank
  }

data RankTotalRecord key =
  RankTotalRecord
    { rankTotalId   :: key
    , extensionId   :: ExtensionNameId
    , rankCount     :: RankCount
    , rankSum       :: RankSum
    } deriving Show

newtype ExtensionNameId = ExtensionNameId T.Text deriving Show

newtype Rank = Rank
  { rankInt :: Int32
  } deriving (Show, Eq, Ord)

newtype RankCount = RankCount
  { rankCountInt :: Int32
  } deriving (Show, Eq, Ord, Num)

newtype RankId = RankId
  { rankIdInt :: Int32
  } deriving (Show, Eq, Ord)

newtype RankSum = RankSum
  { rankSumInt :: Int32
  } deriving (Show, Eq, Ord)

newtype RankTotalId = RankTotalId
  { rankTotalIdInt :: Int32
  } deriving (Show, Eq, Ord)

validRank :: Maybe ByteString -> Maybe Rank
validRank maBs =
  Rank <$> validNumberValue maBs

validExtension :: Maybe ByteString -> Maybe ExtensionNameId
validExtension maBs =
  ExtensionNameId <$>  (T.pack <$> validString maBs)

validString :: Maybe ByteString -> Maybe String
validString maBs =
  unpack <$> maBs

validNumberValue :: Maybe ByteString -> Maybe Int32
validNumberValue mbByteString =
  case mbByteString of
    Nothing         -> Nothing
    Just byteString -> readMaybe $ unpack byteString

errorMessage :: String
errorMessage =
  "Sorry something went wrong, please try again."

newtype ExtensionId = ExtensionId
  { extensionIdInt :: Int32
  } deriving (Show, Eq, Ord)

extensionIdToText :: ExtensionNameId -> T.Text
extensionIdToText (ExtensionNameId extensionIdText) =
  extensionIdText

newtype ExtensionName = ExtensionName T.Text

extensionNameToText :: ExtensionName -> T.Text
extensionNameToText (ExtensionName extensionName) =
  extensionName

newtype ExtensionDescription = ExtensionDescription T.Text

extensionDescriptionToText :: ExtensionDescription -> T.Text
extensionDescriptionToText (ExtensionDescription extensionDescription) =
  extensionDescription

data ExtensionRecord key =
  ExtensionRecord
    { extensionRecordId           :: key
    , extensionRecordNameId       :: ExtensionNameId
    , extensionRecordName         :: ExtensionName
    , extensionRecordDescription :: ExtensionDescription
    }

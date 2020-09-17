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
    , extensionId   :: ExtensionId
    , rankCount     :: RankCount
    , rankSum       :: RankSum
    } deriving Show

newtype ExtensionId = ExtensionId T.Text deriving Show

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

validExtension :: Maybe ByteString -> Maybe ExtensionId
validExtension maBs =
  ExtensionId <$>  (T.pack <$> validString maBs)

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


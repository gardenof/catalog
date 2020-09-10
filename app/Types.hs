{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Types where

import            Data.Int (Int32)
import qualified  Data.Text as T

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

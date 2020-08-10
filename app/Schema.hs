module Schema where

import            Data.Int (Int32)
import qualified  Database.Orville.PostgreSQL as O
import qualified  Database.Orville.PostgreSQL.Connection as O

sqlEnv :: IO (O.OrvilleEnv O.Connection)
sqlEnv =
  O.newOrvilleEnv <$> sqlInfo

sqlInfo :: IO (O.Pool O.Connection)
sqlInfo =
  O.createConnectionPool 1 60 5 "host=catalog-db port=5432 user=postgres password= master"

allSchemas :: O.SchemaDefinition
allSchemas = [ O.Table rankTable ]

data RankRecord key = RankRecord
  { rankId :: key
  , rank   :: Rank
  }

newtype RankId = RankId
  { rankIdInt :: Int32
  } deriving (Show, Eq, Ord)

newtype Rank = Rank
  { rankInt :: Int32
  } deriving (Show, Eq, Ord)

rankIdField :: O.FieldDefinition RankId
rankIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType rankIdInt RankId

rankField :: O.FieldDefinition Rank
rankField =
  O.int32Field "rank" `O.withConversion`
  O.convertSqlType rankInt Rank

rankTable :: O.TableDefinition
  (RankRecord RankId) (RankRecord ()) RankId
rankTable =
  O.mkTableDefinition $
    O.TableParams
      { O.tblName = "ranks"
      , O.tblPrimaryKey = rankIdField
      , O.tblMapper =
          RankRecord
            <$> O.readOnlyField rankIdField
            <*> O.attrField rank rankField
      , O.tblGetKey = rankId
      , O.tblSafeToDelete = []
      , O.tblComments = O.noComments
      }

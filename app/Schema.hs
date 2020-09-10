module Schema where

import qualified  Database.Orville.PostgreSQL as O
import qualified  Database.Orville.PostgreSQL.Connection as O

import            LanguageExtension
import            Types

createCatalogOrvilleEnv :: IO (O.OrvilleEnv O.Connection)
createCatalogOrvilleEnv =
  O.newOrvilleEnv <$> createCatalogConnectionPool

createCatalogConnectionPool :: IO (O.Pool O.Connection)
createCatalogConnectionPool =
  O.createConnectionPool 1 60 5 "host=catalog-db port=5432 user=postgres password= master"

allSchemas :: O.SchemaDefinition
allSchemas = [ O.Table rankTable
             , O.Table rankTotalTable
             ]

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

rankTotalTable :: O.TableDefinition
  (RankTotalRecord RankTotalId) (RankTotalRecord ()) RankTotalId
rankTotalTable =
  O.mkTableDefinition $
    O.TableParams
      { O.tblName = "rank_total"
      , O.tblPrimaryKey = rankTotalIdField
      , O.tblMapper =
          RankTotalRecord
            <$> O.readOnlyField rankTotalIdField
            <*> O.attrField extensionId extensionIdField
            <*> O.attrField rankCount rankCountField
            <*> O.attrField rankSum rankSumField
      , O.tblGetKey = rankTotalId
      , O.tblSafeToDelete = []
      , O.tblComments = O.noComments
      }

rankTotalIdField :: O.FieldDefinition RankTotalId
rankTotalIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType rankTotalIdInt RankTotalId

rankCountField :: O.FieldDefinition RankCount
rankCountField =
  O.int32Field "rank_count" `O.withConversion`
  O.convertSqlType rankCountInt RankCount

rankSumField :: O.FieldDefinition RankSum
rankSumField =
  O.int32Field "rank_sum" `O.withConversion`
  O.convertSqlType rankSumInt RankSum

extensionIdField :: O.FieldDefinition ExtensionId
extensionIdField =
  O.textField "extension_id" 35 `O.withConversion`
  O.convertSqlType extensionIdToString ExtensionId

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
             , O.Table extensionTable
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
            <*> O.attrField extensionId extensionIdNameField
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

extensionIdNameField :: O.FieldDefinition ExtensionNameId
extensionIdNameField =
  O.textField "extension_id_name" 35 `O.withConversion`
  O.convertSqlType extensionIdToText ExtensionNameId

extensionNameField :: O.FieldDefinition ExtensionName
extensionNameField =
  O.textField "extension_name" 54 `O.withConversion`
  O.convertSqlType extensionNameToText ExtensionName

extensionDescriptionField :: O.FieldDefinition ExtensionDescription
extensionDescriptionField =
  O.textField "extension_id" 342 `O.withConversion`
  O.convertSqlType extensionDescriptionToText ExtensionDescription

extensionTable :: O.TableDefinition
  (ExtensionRecord ExtensionId) (ExtensionRecord()) ExtensionId
extensionTable =
  O.mkTableDefinition $
    O.TableParams
      { O.tblName = "extension"
      , O.tblPrimaryKey = extensionIdField
      , O.tblMapper =
        ExtensionRecord
          <$> O.readOnlyField extensionIdField
          <*> O.attrField extensionRecordNameId (extensionIdNameField `O.withFlag` O.Unique)
          <*> O.attrField extensionRecordName extensionNameField
          <*> O.attrField extensionRecordDescription extensionDescriptionField
      , O.tblGetKey = extensionRecordId
      , O.tblSafeToDelete = []
      , O.tblComments = O.noComments
      }

extensionIdField :: O.FieldDefinition ExtensionId
extensionIdField =
  O.automaticIdField "id" `O.withFlag` O.PrimaryKey `O.withConversion`
  O.convertSqlType extensionIdInt ExtensionId

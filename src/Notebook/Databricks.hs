{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}
module Notebook.Databricks where

import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as B
import           Data.Default
import qualified Data.HashMap.Lazy    as H
import           Data.List            ((\\))
import           Data.Monoid          ((<>))
import           Data.Text            (Text)
import qualified Data.UUID            as UUID
import           Notebook.Utils

data DBNotebook = DBN { _dbnCommands    :: [DBCommand]
                      -- , _dbnDashboards   :: Maybe [Value]
                      -- , _dbnIPythonMeta  :: Maybe Value
                      -- , _dbnInputWidgets :: Maybe Value
                      , _dbnName        :: Text
                      , _dbnGuid        :: UUID.UUID
                      -- , _dbnVersion      :: Maybe Value -- some sort of enum
                      , _dbnLanguage    :: Text
                      -- , _dbnGlobalVars   :: Maybe Value
                      -- , _dbnOrigID       :: Maybe Value -- Integer?
                      , _dbnOtherFields :: H.HashMap Text Value}
  deriving Show

-- instance Default DBNotebook where
--   def = DBN [] Nothing Nothing Nothing "" def Nothing "md" Nothing Nothing H.empty

instance Default DBNotebook where
  def = DBN [] "" def "md" H.empty

data DBCommand = DBC { --_dbcCustomPlotOptions :: Maybe Value
                     -- , _dbcErrorSummary      :: Maybe Value
                     -- , _dbcHeight            :: Maybe Value
                     -- , _dbcDiffDeletes       :: Maybe Value
                     --,
                       _dbcCommandTitle      :: Maybe Text
                     -- , _dbcState             :: Maybe Value
                     , _dbcCommand           :: Text
                     , _dbcResults           :: Maybe DBResult
                     -- , _dbcCommandVersion    :: Maybe Value
                     -- , _dbcXColumns          :: Maybe Value
                     -- , _dbcStartTime         :: Maybe Value
                     -- , _dbcIPythonMetadata   :: Maybe Value
                     -- , _dbcError             :: Maybe Value
                     -- , _dbcPivotAggregation  :: Maybe Value
                     -- , _dbcWidth             :: Maybe Value
                     -- , _dbcNuid              :: Maybe Text
                     -- , _dbcPivotColumns      :: Maybe Value
                     -- , _dbcInputWidgets      :: Maybe Value
                     -- , _dbcSubtype           :: Maybe Value
                     -- , _dbcYColumns          :: Maybe Value
                     -- , _dbcShowCommandTitle  :: Maybe Value
                     , _dbcGuid              :: UUID.UUID
                     -- , _dbcCommandType       :: Maybe Value
                     -- , _dbcCollapsed         :: Maybe Value
                     -- , _dbcVersion           :: Maybe Value
                     -- , _dbcLatestUser        :: Maybe Value
                     -- , _dbcBindings          :: Maybe Value
                     , _dbcHideCommandCode   :: Maybe Bool
                     -- , _dbcDisplayType       :: Maybe Value
                     -- , _dbcGlobalVars        :: Maybe Value
                     -- , _dbcCommentThread     :: Maybe Value
                     -- , _dbcWorkflows         :: Maybe Value
                     -- , _dbcParentHierarchy   :: Maybe Value
                     , _dbcHideCommandResult :: Maybe Bool
                     -- , _dbcFinishTime        :: Maybe Value
                     -- , _dbcCommentsVisible   :: Maybe Value
                     -- , _dbcOrigId            :: Maybe Value
                     -- , _dbcSubmitTime        :: Maybe Value
                     -- , _dbcDiffInserts       :: Maybe Value
                     , _dbcPosition          :: Double
                     -- , _dbcStreamStates      :: Maybe Value
                     , _dbcOtherFields       :: H.HashMap Text Value}
  deriving Show

instance Default DBCommand where
  -- def = DBC Nothing Nothing Nothing Nothing Nothing Nothing "" Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing def Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing 0 Nothing H.empty
  def = DBC Nothing "" Nothing def Nothing  Nothing 0 H.empty

data DBResult = DBR { -- _dbrAddedWidgets   :: Maybe Value
                    _dbrData           :: Maybe Value
                    -- , _dbrArguments      :: Maybe Value
                    -- , _dbrRemovedWidgets :: Maybe Value
                    -- , _dbrType           :: Maybe Value
                    -- , _dbrDatasetInfos   :: Maybe Value
                    --,
                    , _dbrOtherFields    :: H.HashMap Text Value}
  deriving Show

instance Default DBResult where
  -- def = DBR Nothing Nothing Nothing Nothing Nothing Nothing H.empty
  def = DBR Nothing H.empty

makeLenses ''DBNotebook
makeLenses ''DBCommand
makeLenses ''DBResult


dbnotebookKeysKnown :: [Text]
dbnotebookKeysKnown = ["commands", "dashboards", "iPythonMetadata", "inputWidgets", "name", "guid", "version", "language", "globalVars", "origId"]

dbnotebookKeys :: [Text]
dbnotebookKeys = ["commands", "name", "guid", "language"]

instance ToJSON DBNotebook where
  toJSON dbn = object $ [ "commands" .= (dbn^.dbnCommands)
                        , "name" .= (dbn^.dbnName)
                        , "guid" .= (dbn^.dbnGuid)
                        , "language" .= (dbn^.dbnLanguage) ]
               ++ [ k .= v | (k, v) <- H.toList (dbn^.dbnOtherFields) ]

  toEncoding dbn = pairs $ ( "commands" .= (dbn^.dbnCommands)
                             <> "name" .= (dbn^.dbnName)
                             <> "guid" .= (dbn^.dbnGuid)
                             <> "language" .= (dbn^.dbnLanguage) )
                   <> mconcat [ k .= v | (k, v) <- H.toList (dbn^.dbnOtherFields) ]

instance FromJSON DBNotebook where
  parseJSON = withObject "DBNotebook" $ \v -> DBN
    <$> v .: "commands"
    <*> v .: "name"
    <*> v .: "guid"
    <*> v .: "language"
    <*> return (remaining dbnotebookKeys v)

dbcommandKeysKnown :: [Text]
dbcommandKeysKnown = ["customPlotOptions", "errorSummary", "height", "diffDeletes", "commandTitle", "state", "command", "results", "commandVersion", "xColumns", "startTime", "iPythonMetadata", "error", "pivotAggregation", "width", "nuid", "pivotColumns", "inputWidgets", "subtype", "yColumns", "showCommandTitle", "guid", "commandType", "collapsed", "version", "latestUser", "bindings", "hideCommandCode", "displayType", "globalVars", "commentThread", "workflows", "parentHierarchy", "hideCommandResult", "finishTime", "commentsVisible", "origId", "submitTime", "diffInserts", "position", "streamStates"]

dbcommandKeys :: [Text]
dbcommandKeys = ["commandTitle", "command", "results",  "guid", "hideCommandCode", "hideCommandResult", "position"]

instance ToJSON DBCommand where
  toJSON dbc = objectMaybe $ [ "commandTitle" .=? (dbc^.dbcCommandTitle)
                             , "command" .= (dbc^.dbcCommand)
                             , "results" .=? (dbc^.dbcResults)
                             , "guid" .= (dbc^.dbcGuid)
                             , "hideCommandCode" .=? (dbc^.dbcHideCommandCode)
                             , "hideCommandResult" .=? (dbc^.dbcHideCommandResult)
                             , "position" .= (dbc^.dbcPosition) ]
               ++ [ k .= v | (k, v) <- H.toList (dbc^.dbcOtherFields) ]

  toEncoding dbc = pairs $ ( "commandTitle" .=? (dbc^.dbcCommandTitle)
                             <> "command" .= (dbc^.dbcCommand)
                             <> "results" .=? (dbc^.dbcResults)
                             <> "guid" .= (dbc^.dbcGuid)
                             <> "hideCommandCode" .=? (dbc^.dbcHideCommandCode)
                             <> "hideCommandResult" .=? (dbc^.dbcHideCommandResult)
                             <> "position" .= (dbc^.dbcPosition))
                   <> mconcat [ k .= v | (k, v) <- H.toList (dbc^.dbcOtherFields) ]

instance FromJSON DBCommand where
  parseJSON = withObject "DBCommand" $ \v -> DBC
    <$> v .: "commandTitle"
    <*> v .: "command"
    <*> v .: "results"
    <*> v .: "guid"
    <*> v .: "hideCommandCode"
    <*> v .: "hideCommandResult"
    <*> v .: "position"
    <*> return (remaining dbcommandKeys v)


dbresultKeysKnown :: [Text]
dbresultKeysKnown = ["addedWidgets", "data", "arguments", "removedWidgets", "type", "datasetInfos"]

dbresultKeys :: [Text]
dbresultKeys = ["data"]

instance ToJSON DBResult where
  toJSON dbr = objectMaybe $ [ "data" .=? (dbr^.dbrData)
                               --"datasetInfos" .=? (dbr^.dbrDatasetInfos)
                             ]
               ++ [ k .= v | (k, v) <- H.toList (dbr^.dbrOtherFields) ]

  toEncoding dbr = pairs $ ( "data" .=? (dbr^.dbrData)
                             --"addedWidgets" .=? (dbr^.dbrAddedWidgets)
                             )
                   <> mconcat [ k .= v | (k, v) <- H.toList (dbr^.dbrOtherFields) ]

instance FromJSON DBResult where
  parseJSON = withObject "DBResult" $ \v -> DBR
    <$> v .:? "data"
    <*> return (remaining dbresultKeys v)
    -- <$> v .:? "addedWidgets"
    -- <*> v .:? "data"
    -- <*> return (remaining dbresultKeys v)

remaining :: Eq k => [k] -> H.HashMap k v -> H.HashMap k v
remaining ks = H.filterWithKey (\k _ -> not (k `elem` ks))

data Field = Notebook | Command | Result

checkNotebook :: DBNotebook -> [(Field, Text)]
checkNotebook dbn =
  let novelNotebook = (H.keys (dbn^.dbnOtherFields)) \\ dbnotebookKeysKnown
  in concatMap checkCommand (dbn^.dbnCommands) ++ map (Notebook,) novelNotebook

checkCommand :: DBCommand -> [(Field, Text)]
checkCommand dbc =
  let novelCommand = (H.keys (dbc^.dbcOtherFields)) \\ dbcommandKeysKnown
  in maybe [] checkResult (dbc^.dbcResults) ++ map (Command,) novelCommand

checkResult :: DBResult -> [(Field, Text)]
checkResult dbr =
  map (Result,) ((H.keys (dbr^.dbrOtherFields)) \\ dbresultKeysKnown)

{- -- Only for Debug
fromByteString :: ByteString -> Either String DBNotebook
fromByteString s = case eitherDecode s of
  Right a -> if null (checkNotebook a) then a else error "Notebook contains unknown fields" ++ (show (checkNotebook a))
  left -> left
-}

-- {- -- Only for Release
fromByteString :: ByteString -> Either String DBNotebook
fromByteString = eitherDecode
-- -}

toByteString :: DBNotebook -> ByteString
toByteString = encode

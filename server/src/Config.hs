{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
  ( AppConfig (..)
  , AppCtx (..)
  , AppError (..)
  , EBirdConf (..)
  , FlickrConf (..)
  , mkConfig
  , readConfig
  , initialiseAppCtx
  ) where

import           Common
import           System.Environment         (lookupEnv)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString            as B
import qualified Data.Text                  as T
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG

defaultConfigFileEnv :: String
defaultConfigFileEnv = "BIRDISM_CONFIG_FILE"

defaultConfigFilepath :: FilePath
defaultConfigFilepath = "./birdism_conf.json"

defaultServerPort :: Int
defaultServerPort = 8888

newtype EBirdConf
  = EBirdConf { ebcToken :: Text }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''EBirdConf)

data FlickrConf
  = FlickrConf
  { fcKey    :: !Text
  , fcSecret :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''FlickrConf)

-- | The configuration required for the app. The database URL and external service API keys
data AppConfig
  = AppConfig
  { acDatabaseUrl :: !Text
  , acServerPort  :: !(Maybe Int)
  , acEbird       :: !EBirdConf
  , acFlickr      :: !FlickrConf
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''AppConfig)

-- | The sort of global, immutable environment available to the entire app via the Reader monad.
-- Things like various API keys, the database connection, in-memory caches etc.
data AppCtx
  = AppCtx
  { axDbConn     :: !PG.Connection
  , axServerPort :: !Int
  -- ^ database connection. TODO: change it to a pool
  , axEbirdConf  :: !EBirdConf
  , axFlickrConf :: !FlickrConf
  }

data AppError
  = AESearchError !Text
  | AEDbError !Text
  | AEConfigError !Text
  deriving (Show)

instance J.ToJSON AppError where
  toJSON a = case a of
    AESearchError e -> encodeErr "search-error" e
    AEDbError e     -> encodeErr "db-error" e
    AEConfigError e -> encodeErr "config-error" e

-- -- Our own monad!
-- type App = ReaderT AppCtx (ExceptT AppError IO)

encodeErr :: Text -> Text -> J.Value
encodeErr code e =
  J.object [ "code" J..= code
           , "error" J..= e
           ]

mkConfig :: Text -> Text -> Text -> Text -> AppConfig
mkConfig dbUrl ebToken fKey fSecret =
  AppConfig { acDatabaseUrl = dbUrl
            , acServerPort = Just defaultServerPort
            , acEbird = EBirdConf ebToken
            , acFlickr = FlickrConf fKey fSecret
            }

readConfig :: (MonadIO m, MonadError AppError m) => m AppConfig
readConfig = do
  env <- liftIO $ lookupEnv defaultConfigFileEnv
  let filepath = fromMaybe defaultConfigFilepath env
  -- its OK to fail at runtime for now. later we can use 'try' to catch exceptions from this
  configFile <- liftIO $ B.readFile filepath
  case J.eitherDecodeStrict configFile of
    Left e  -> throwError $ AEConfigError (T.pack $ "FATAL ERROR: error parsing config file: " ++ e)
    Right c -> return c

initialiseAppCtx :: MonadIO m => AppConfig -> m AppCtx
initialiseAppCtx (AppConfig dbUrl port ebird flickr) = do
  conn <- liftIO $ PG.connectPostgreSQL (T.encodeUtf8 dbUrl)
  return $ AppCtx conn (fromMaybe defaultServerPort port) ebird flickr

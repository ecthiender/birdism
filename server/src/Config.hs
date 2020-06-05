{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Config
  -- ( AppConfig (..)
  -- , AppCtx (..)
  -- , AppError (..)
  -- , EBirdConf (..)
  -- , FlickrConf (..)
  -- , HasEBirdConf
  -- , HasFlickrConf
  -- , HasDbConfig
  -- , HasAppCtx
  -- , mkConfig
  -- , readConfig
  -- , initialiseAppCtx
  -- )
  where

import           Common
import           Control.Lens
import           System.Environment         (lookupEnv)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString            as B
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG

defaultConfigFileEnv :: String
defaultConfigFileEnv = "BIRDISM_CONFIG_FILE"

defaultConfigFilepath :: FilePath
defaultConfigFilepath = "./birdism_conf.json"

defaultServerPort :: Int
defaultServerPort = 8888

data EBirdConf
  = EBirdConf { _ebcToken :: Text }
  deriving (Show, Eq)

makeClassy ''EBirdConf

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''EBirdConf)

data FlickrConf
  = FlickrConf
  { _fcKey    :: !Text
  , _fcSecret :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''FlickrConf)

makeClassy ''FlickrConf

-- | The configuration required for the app. The database URL and external service API keys
data AppConfig
  = AppConfig
  { acDatabaseUrl :: !Text
  , acServerPort  :: !(Maybe Int)
  , acEbird       :: !EBirdConf
  , acFlickr      :: !FlickrConf
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''AppConfig)

data DbConfig
  = DbConfig
  { _dbConnection :: !PG.Connection
  , _dbPoolSize   :: !Int
  , _dbStripes    :: !Int
  }

makeClassy ''DbConfig

-- | The sort of global, immutable environment available to the entire app via the Reader monad.
-- Things like various API keys, the database connection, in-memory caches etc.
data AppCtx
  = AppCtx
  { _axDbConn            :: !DbConfig
  -- ^ database connection. TODO: change it to a pool

  , _axServerPort        :: !Int

  , _axBirdFamiliesCache :: !(Map.HashMap Text Text)
  -- ^ a global cache of all the bird families. The HTTP API can just read from this and return; it
  -- doesn't need to hit the database. This rarely changes, hence this is totally safe. It prepares
  -- the cache only on startup. If you ever need to bust this cache, just restart the server. ,

  , _axRegionsCache      :: !(Map.HashMap Text Text)
  -- ^ a global cache of all the regions. The HTTP API can just read from this and return; it
  -- doesn't need to hit the database. This rarely changes, hence this is totally safe. It prepares
  -- the cache only on startup. If you ever need to bust this cache, just restart the server. ,

  , _axEbirdConf         :: !EBirdConf
  , _axFlickrConf        :: !FlickrConf
  }

makeClassy ''AppCtx

instance HasEBirdConf AppCtx where
  eBirdConf = appCtx . axEbirdConf

instance HasFlickrConf AppCtx where
  flickrConf = appCtx . axFlickrConf

instance HasDbConfig AppCtx where
  dbConfig = appCtx . axDbConn

-- instance HasBirdFamiliesCache AppCtx where
--   birdFamilesCache = appCtx . axBirdFamilesCache

data DbError
  = PostgresError !Text
  | UnknownDbError !Text
  deriving (Show)

makeClassyPrisms ''DbError

$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase
                   , J.sumEncoding = J.TaggedObject "code" "error"
                   } ''DbError)

data EbirdError
  = EbirdErrorSearch !Text
  | EbirdErrorParseResponse !Text
  deriving (Show)

makeClassyPrisms ''EbirdError

$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase
                   , J.sumEncoding = J.TaggedObject "code" "error"
                   } ''EbirdError)

data AppError
  = AEEbirdError !EbirdError
  | AEDbError !DbError
  | AEConfigError !Text
  deriving (Show)

makeClassyPrisms ''AppError

instance AsEbirdError AppError where
  _EbirdError = _AEEbirdError . _EbirdError

instance AsDbError AppError where
  _DbError = _AEDbError . _DbError

instance J.ToJSON AppError where
  toJSON a = case a of
    AEEbirdError e  -> J.toJSON e
    AEDbError e     -> J.toJSON e
    AEConfigError e -> J.toJSON e

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
    Left e  -> throwError $ AEConfigError ("error parsing config file: " <> T.pack e)
    Right c -> return c

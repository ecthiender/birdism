module Birdism.Config
  ( AppConfig (..)
  , AppCtx (..)
  , axAppCache
  , AppError (..)
  , EbirdError (..)
  , AsEbirdError (..)
  , EBirdConf (..)
  , ebcToken
  , FlickrConf (..)
  , HasFlickrConf
  , HasEBirdConf
  , DbConfig (..)
  , dbConnection
  , HasDbConfig
  , HasAppCtx
  , HasBirdismCache
  , mkConfig
  , readConfig
  , encodeErr
  , defaultServerPort
  )
  where

import           Control.Lens               (makeClassy, makeClassyPrisms)
import           System.Environment         (lookupEnv)

import qualified Data.Aeson                 as J
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Data.ByteString.Char8      as B
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG

import           Birdism.Cache              (BirdismCache, HasBirdismCache(..))
import           Birdism.Common
import           Service.Flickr.Context     (AsFlickrError (..), FlickrConf (..), FlickrError,
                                             HasFlickrConf (..))

configEnv :: String
configEnv = "BIRDISM_CONFIG"

defaultServerPort :: Int
defaultServerPort = 8888

data EBirdConf
  = EBirdConf { _ebcToken :: Text }
  deriving (Show, Eq, Generic)

makeClassy ''EBirdConf

instance J.ToJSON EBirdConf where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

instance J.FromJSON EBirdConf where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

-- | The configuration required for the app. The database URL and external service API keys
data AppConfig
  = AppConfig
  { acDatabaseUrl :: !Text
  , acServerPort  :: !(Maybe Int)
  , acEbird       :: !EBirdConf
  , acFlickr      :: !FlickrConf
  } deriving (Show, Generic)

instance J.ToJSON AppConfig where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

instance J.FromJSON AppConfig where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

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
  { _axDbConn     :: !DbConfig
  -- ^ database connection. TODO: change it to a pool
  , _axServerPort :: !Int
  , _axAppCache   :: !BirdismCache
  , _axEbirdConf  :: !EBirdConf
  , _axFlickrConf :: !FlickrConf
  }

makeClassy ''AppCtx

instance HasEBirdConf AppCtx where
  eBirdConf = appCtx . axEbirdConf

instance HasFlickrConf AppCtx where
  flickrConf = appCtx . axFlickrConf

instance HasDbConfig AppCtx where
  dbConfig = appCtx . axDbConn

instance HasBirdismCache AppCtx where
  birdismCache = appCtx . axAppCache

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
  | EbirdErrorUnexpected !Text
  deriving (Show)

makeClassyPrisms ''EbirdError

$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase
                   , J.sumEncoding = J.TaggedObject "code" "error"
                   } ''EbirdError)

data AppError
  = AEEbirdError !EbirdError
  | AEFlickrError !FlickrError
  | AEDbError !DbError
  | AEConfigError !Text
  deriving (Show)

makeClassyPrisms ''AppError

instance AsEbirdError AppError where
  _EbirdError = _AEEbirdError . _EbirdError

instance AsDbError AppError where
  _DbError = _AEDbError . _DbError

instance AsFlickrError AppError where
  _FlickrError = _AEFlickrError . _FlickrError

instance J.ToJSON AppError where
  toJSON a = case a of
    AEEbirdError e  -> J.toJSON e
    AEFlickrError e -> J.toJSON e
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
  env <- liftIO $ lookupEnv configEnv
  case env of
    Nothing -> throwError $ AEConfigError $ "env var '" <> T.pack configEnv <> "' not found"
    Just config -> case J.eitherDecodeStrict (B.pack config) of
      Left e  -> throwError $ AEConfigError $ "error parsing config file: " <> T.pack e
      Right c -> return c

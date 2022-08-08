-- | Code related to initialization

module Birdism.Init where

import qualified Data.ByteString.Char8      as BC
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG

import           Data.FileEmbed             (embedFile)
import           Data.String                (fromString)

import           Birdism.Common
import           Birdism.Config
import           Birdism.Data
import           Service.Flickr.Context     (FlickrContext (..), mkFlickrCache)

initialiseAppCtx :: MonadIO m => AppConfig -> m AppCtx
initialiseAppCtx (AppConfig dbUrl port ebird flickr) = do
  conn <- liftIO $ PG.connectPostgreSQL (T.encodeUtf8 dbUrl)
  let dbConf = DbConfig conn 10 1
  initialiseDatabase conn
  -- create an in-memory cache of family names and region, as they don't change
  families <- runReaderT getFamilyNames dbConf
  regions  <- runReaderT getRegionNames dbConf
  flickrCache <- mkFlickrCache
  let flickrCtx = FlickrContext flickr flickrCache
  return $ AppCtx dbConf (fromMaybe defaultServerPort port) (FamiliesCache families) (RegionsCache regions) ebird flickrCtx

initialiseDatabase :: MonadIO m => PG.Connection -> m ()
initialiseDatabase conn = do
  let q = fromString $ BC.unpack dbSchema
  liftIO $ putStrLn "executing init db"
  void $ liftIO $ PG.execute_ conn q
  where
    dbSchema = $(embedFile "res/schema.sql")

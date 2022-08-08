-- | Code related to initialization

module Birdism.Init where

import qualified Data.ByteString.Char8      as BC
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Birdism.Cache as Cache

import           Data.FileEmbed             (embedFile)
import           Data.String                (fromString)

import           Birdism.Common
import           Birdism.Config
import           Birdism.Data

initialiseAppCtx :: MonadIO m => AppConfig -> m AppCtx
initialiseAppCtx (AppConfig dbUrl port ebird flickr) = do
  conn <- liftIO $ PG.connectPostgreSQL (T.encodeUtf8 dbUrl)
  let dbConf = DbConfig conn 10 1
  initialiseDatabase conn
  -- create an in-memory cache of family names and region, as they don't change
  regions  <- runReaderT getRegionNames dbConf
  families <- runReaderT getFamilyNames dbConf
  cache <- Cache.newCache regions families
  return $ AppCtx dbConf (fromMaybe defaultServerPort port) cache ebird flickr

initialiseDatabase :: MonadIO m => PG.Connection -> m ()
initialiseDatabase conn = do
  let q = fromString $ BC.unpack dbSchema
  liftIO $ putStrLn "executing init db"
  void $ liftIO $ PG.execute_ conn q
  where
    dbSchema = $(embedFile "res/schema.sql")

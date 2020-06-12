-- | Code related to initialization
{-# LANGUAGE TemplateHaskell #-}

module Init where

import qualified Data.ByteString.Char8      as BC
import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG

import           Data.FileEmbed             (embedFile)
import           Data.String                (fromString)

import           Common
import           Config
import           Lib

initialiseAppCtx :: MonadIO m => AppConfig -> m AppCtx
initialiseAppCtx (AppConfig dbUrl port ebird flickr) = do
  conn <- liftIO $ PG.connectPostgreSQL (T.encodeUtf8 dbUrl)
  let dbConf = DbConfig conn 10 1
  initialiseDatabase conn
  families <- runReaderT getFamilyNames dbConf
  regions <- runReaderT getRegionNames dbConf
  return $ AppCtx dbConf (fromMaybe defaultServerPort port) families regions ebird flickr

initialiseDatabase :: MonadIO m => PG.Connection -> m ()
initialiseDatabase conn = do
  let q = fromString $ BC.unpack dbSchema
  liftIO $ putStrLn "executing init db"
  void $ liftIO $ PG.execute_ conn q
  where
    dbSchema = $(embedFile "res/schema.sql")

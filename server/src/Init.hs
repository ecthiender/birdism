-- | Code related to initialization
module Init where

import qualified Data.Text.Encoding         as T
import qualified Database.PostgreSQL.Simple as PG

import           Common
import           Config
import           Lib

initialiseAppCtx :: MonadIO m => AppConfig -> m AppCtx
initialiseAppCtx (AppConfig dbUrl port ebird flickr) = do
  conn <- liftIO $ PG.connectPostgreSQL (T.encodeUtf8 dbUrl)
  let dbConf = DbConfig conn 10 1
  families <- runReaderT getFamilyNames dbConf
  regions <- runReaderT getRegionNames dbConf
  return $ AppCtx dbConf (fromMaybe defaultServerPort port) families regions ebird flickr

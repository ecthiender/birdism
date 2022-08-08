-- | A common module to hold all Flickr related common types. Required by
-- multiple modules

module Service.Flickr.Context where

import           Birdism.Common      (Generic, MonadIO, Text, liftIO)
import           Control.Lens        (makeClassy)

import qualified Data.Aeson          as J
import qualified Data.Aeson.Casing   as J
import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef

data FlickrConf
  = FlickrConf
  { _fcKey    :: !Text
  , _fcSecret :: !Text
  } deriving (Show, Generic)

instance J.ToJSON FlickrConf where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

instance J.FromJSON FlickrConf where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

makeClassy ''FlickrConf

type Cache = Map.HashMap Text [Text]

newtype FlickrCache =
  FlickrCache { unFlickrCache :: IORef.IORef Cache }

makeClassy ''FlickrCache

mkFlickrCache :: MonadIO m => m FlickrCache
mkFlickrCache = FlickrCache <$> liftIO (IORef.newIORef mempty)

lookupFlickrCache :: MonadIO m => Text -> FlickrCache -> m (Maybe [Text])
lookupFlickrCache key store = do
  cache <- liftIO $ IORef.readIORef $ unFlickrCache store
  pure $ Map.lookup key cache

writeFlickrCache :: MonadIO m => Text -> [Text] -> FlickrCache -> m ()
writeFlickrCache key val store = do
  liftIO $ IORef.modifyIORef' (unFlickrCache store) $ \cache ->
    Map.insert key val cache

data FlickrContext
  = FlickrContext
  { _fcxConf :: !FlickrConf
  , _fcxCache :: !FlickrCache
  }

makeClassy ''FlickrContext

-- | Caching mechanism

module Birdism.Cache
  ( BirdismCache
  , HasBirdismCache (..)
  , newCache
  , getRegionsCache
  , writeRegionsCache
  , getFamiliesCache
  , writeFamiliesCache
  , lookupFlickrCache
  , writeFlickrCache
  , lookupEbirdCache
  , writeEbirdCache
  ) where

import           Birdism.Common      (MonadIO, Text, liftIO, (<&>))
import           Control.Lens        (makeClassy)
import Birdism.Types (Region, Family)

import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef

data CacheData
  = CacheData
  { cdRegionsCache :: [Region]
  , cdFamiliesCache :: [Family]
  , cdFlickrCache :: Map.HashMap Text [Text]
  , cdEbirdCache :: Map.HashMap Text [Text]
  }

newtype BirdismCache =
  BirdismCache { unBirdisimCache :: IORef.IORef CacheData }

makeClassy ''BirdismCache

newCache :: MonadIO m => [Region] -> [Family] -> m BirdismCache
newCache regions families = do
  store <- liftIO $ IORef.newIORef $ CacheData regions families mempty mempty
  pure $ BirdismCache store

getRegionsCache :: MonadIO m => BirdismCache -> m [Region]
getRegionsCache store = lookupp store <&> cdRegionsCache

writeRegionsCache :: MonadIO m => [Region] -> BirdismCache -> m ()
writeRegionsCache regions store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdRegionsCache = regions }

getFamiliesCache :: MonadIO m => BirdismCache -> m [Family]
getFamiliesCache store = lookupp store <&> cdFamiliesCache

writeFamiliesCache :: MonadIO m => [Family] -> BirdismCache -> m ()
writeFamiliesCache families store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdFamiliesCache = families }

lookupFlickrCache :: MonadIO m => Text -> BirdismCache -> m (Maybe [Text])
lookupFlickrCache key store = do
  cache <- lookupp store <&> cdFlickrCache
  pure $ Map.lookup key cache

writeFlickrCache :: MonadIO m => Text -> [Text] -> BirdismCache -> m ()
writeFlickrCache key val store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdFlickrCache = Map.insert key val $ cdFlickrCache cache }

lookupEbirdCache :: MonadIO m => Text -> BirdismCache -> m (Maybe [Text])
lookupEbirdCache key store = do
  cache <- lookupp store <&> cdEbirdCache
  pure $ Map.lookup key cache

writeEbirdCache :: MonadIO m => Text -> [Text] -> BirdismCache -> m ()
writeEbirdCache key val store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdEbirdCache = Map.insert key val $ cdEbirdCache cache }

lookupp :: MonadIO m => BirdismCache -> m CacheData
lookupp store = liftIO $ IORef.readIORef $ unBirdisimCache store

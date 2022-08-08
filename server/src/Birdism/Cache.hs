-- | Caching mechanism

module Birdism.Cache
  ( BirdismCache
  , HasBirdismCache (..)
  , newCache
  , lookupFlickrCache
  , writeFlickrCache
  , lookupEbirdCache
  , writeEbirdCache
  ) where

import           Birdism.Common      (MonadIO, Text, liftIO, (<&>))
import           Birdism.Types       (SpeciesCode)
import           Control.Lens        (makeClassy)

import qualified Data.HashMap.Strict as Map
import qualified Data.IORef          as IORef

data CacheData
  = CacheData
  { cdFlickrCache :: !(Map.HashMap Text [Text])
  , cdEbirdCache  :: !(Map.HashMap Text [SpeciesCode])
  }

newtype BirdismCache =
  BirdismCache { unBirdisimCache :: IORef.IORef CacheData }

makeClassy ''BirdismCache

newCache :: MonadIO m => m BirdismCache
newCache = do
  store <- liftIO $ IORef.newIORef $ CacheData mempty mempty
  pure $ BirdismCache store

lookupFlickrCache :: MonadIO m => Text -> BirdismCache -> m (Maybe [Text])
lookupFlickrCache key store = do
  cache <- lookupp store <&> cdFlickrCache
  pure $ Map.lookup key cache

writeFlickrCache :: MonadIO m => Text -> [Text] -> BirdismCache -> m ()
writeFlickrCache key val store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdFlickrCache = Map.insert key val $ cdFlickrCache cache }

lookupEbirdCache :: MonadIO m => Text -> BirdismCache -> m (Maybe [SpeciesCode])
lookupEbirdCache key store = do
  cache <- lookupp store <&> cdEbirdCache
  pure $ Map.lookup key cache

writeEbirdCache :: MonadIO m => Text -> [SpeciesCode] -> BirdismCache -> m ()
writeEbirdCache key val store =
  liftIO $ IORef.modifyIORef' (unBirdisimCache store) $ \cache ->
    cache { cdEbirdCache = Map.insert key val $ cdEbirdCache cache }

lookupp :: MonadIO m => BirdismCache -> m CacheData
lookupp store = liftIO $ IORef.readIORef $ unBirdisimCache store

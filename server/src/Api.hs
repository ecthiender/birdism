{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Api where

import           Common
import           Control.Lens

import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J

import           Config
import           Lib
import           Types

data SearchRequest
  = SearchRequest
  { _srqRegion :: !RegionCode
  , _srqFamily :: !ScientificName
  -- ^ 'ScientificName' of the family
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''SearchRequest)

newtype SearchResponse
  = SearchResponse { _srpResult :: SearchResult }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''SearchResponse)

processSearch
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasFlickrConf r
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , HasAppCtx r
     )
  => SearchRequest -> m SearchResponse
processSearch (SearchRequest region familySciName) = do
  fams <- asks (^. axBirdFamiliesCache)
  let found = find (\f -> _fScientificName f == familySciName) (unFamiliesCache fams)
  case found of
    Nothing     -> throwError $ (_EbirdErrorSearch #) $ "Invalid family"
    Just family -> SearchResponse <$> getCorpus region family

getFamilies
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m FamilyNames
getFamilies =
  asks (^. axBirdFamiliesCache) >>= pure . unFamiliesCache

getRegions
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m RegionNames
getRegions =
  asks (^. axRegionsCache) >>= pure . unRegionsCache

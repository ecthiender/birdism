{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Api where

import           Common
import           Control.Lens

import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J

import           Config
import           Lib

data SearchRequest
  = SearchRequest
  { _srqRegion :: !Region
  , _srqFamily :: !Family
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
     )
  => SearchRequest -> m SearchResponse
processSearch (SearchRequest region family) =
  SearchResponse <$> getCorpus region family

getFamilies
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m FamilyNames
getFamilies =
  asks (^. axBirdFamiliesCache)

getRegions
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m RegionNames
getRegions =
  asks (^. axRegionsCache)

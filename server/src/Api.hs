{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}

module Api where

import           Common

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
  :: (MonadIO m, MonadReader AppCtx m, MonadError AppError m)
  => SearchRequest -> m SearchResponse
processSearch (SearchRequest region family) =
  SearchResponse <$> getCorpus region family

getFamilies :: (MonadReader AppCtx m, MonadIO m) => m FamilyNames
getFamilies = getFamilyNames

getRegions :: (MonadReader AppCtx m, MonadIO m) => m RegionNames
getRegions = getRegionNames

{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Birdism.Api where

import           Control.Lens

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J

import           Birdism.Common
import           Birdism.Config
import           Birdism.Lib
import           Birdism.Types
import qualified Data.Text         as T

newtype ApiResponse a
  = ApiResponse { _arpResult :: a }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''ApiResponse)


data SearchRequest
  = SearchRequest
  { _srqRegion :: !RegionCode
  , _srqFamily :: !ScientificName
  -- ^ 'ScientificName' of the family
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''SearchRequest)

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
  => SearchRequest -> m (ApiResponse SearchResult)
processSearch (SearchRequest region familySciName) = do
  withFamily (familySciName, _fScientificName) (fmap ApiResponse . getCorpus region)

processSpeciesSearch
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , HasAppCtx r
     , MonadError e m
     , AsEbirdError e
     )
  => SearchRequest -> m (ApiResponse [Bird])
processSpeciesSearch (SearchRequest region familySciName) = do
  withFamily (familySciName, _fScientificName) (fmap ApiResponse . getSpeciesByRegionFamily region)

processImageSearch
  :: (MonadReader r m, HasFlickrConf r, MonadIO m)
  => [Bird] -> m (ApiResponse SearchResult)
processImageSearch birds = do
  ApiResponse <$> getImagesBySpecies birds

newtype GetFamilyScientificNameRequest
  = GetFamilyScientificNameRequest { _gfsnrName :: CommonName }
  deriving (Show, Eq, Generic)

instance J.FromJSON GetFamilyScientificNameRequest where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

newtype GetFamilyScientificNameResponse
  = GetFamilyScientificNameResponse { _gfsnrFamilies :: [Family] }
  deriving (Show, Eq, Generic)

instance J.ToJSON GetFamilyScientificNameResponse where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

getFamilyScientificName
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , HasAppCtx r
     , MonadError e m
     , AsEbirdError e
     )
  => GetFamilyScientificNameRequest -> m (ApiResponse GetFamilyScientificNameResponse)
getFamilyScientificName (GetFamilyScientificNameRequest commonName) = do
  fams <- asks (^. axBirdFamiliesCache)
  let isSubStr v1 v2 = T.isInfixOf (T.toLower $ uCommonName v1) (T.toLower $ uCommonName v2)
      found = filter (isSubStr commonName . _fCommonName) (unFamiliesCache fams)
  pure $ ApiResponse $ GetFamilyScientificNameResponse found

withFamily
  :: (MonadReader r m, HasAppCtx r, MonadError e m, AsEbirdError e, Eq b)
  => (b, Family -> b) -> (Family -> m a) -> m a
withFamily (val, selector) action = do
  fams <- asks (^. axBirdFamiliesCache)
  let found = find (\f -> selector f == val) (unFamiliesCache fams)
  case found of
    Nothing     -> throwError $ _EbirdErrorSearch # "Invalid family"
    Just family -> action family

getFamilies
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m FamilyNames
getFamilies =
  asks (^. axBirdFamiliesCache) <&> unFamiliesCache

getRegions
  :: ( MonadIO m
     , MonadReader r m
     , HasAppCtx r
     )
  => m RegionNames
getRegions =
  asks (^. axRegionsCache) <&> unRegionsCache

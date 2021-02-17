{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Birdism.Api where

import           Control.Lens
import           Servant

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Text         as T

import           Birdism.Common
import           Birdism.Config
import           Birdism.Lib
import           Birdism.Types


type BirdismHttpAPI
  =    "api" :> "ping" :> Get '[PlainText] Text
  -- the v1 API
  :<|> "api" :> "v1" :> "regions" :> Get '[JSON] RegionNames
  :<|> "api" :> "v1" :> "families" :> Get '[JSON] FamilyNames
  :<|> "api" :> "v1" :> "family" :> "scientific-name" :> ReqBody '[JSON] GetFamilyScientificNameRequest :> Post '[JSON] GetFamilyScientificNameResponse
  :<|> "api" :> "v1" :> "search" :> ReqBody '[JSON] SearchRequest :> Post '[JSON] SearchResult
  :<|> "api" :> "v1" :> "search" :> "species" :> ReqBody '[JSON] SearchRequest :> Post '[JSON] [Bird]
  :<|> "api" :> "v1" :> "search" :> "images" :> ReqBody '[JSON] [Bird] :> Post '[JSON] SearchResult


-- | The HTTP server implementing the above API
birdismApiServer
  :: (MonadError AppError m, MonadReader AppCtx m, MonadIO m)
  => ServerT BirdismHttpAPI m
birdismApiServer
     = pingApiHandler
  :<|> getRegions
  :<|> getFamilies
  :<|> getFamilyScientificName
  :<|> processSearch
  :<|> processSpeciesSearch
  :<|> processImageSearch

serverProxy :: Proxy BirdismHttpAPI
serverProxy = Proxy

pingApiHandler :: Monad m => m Text
pingApiHandler = pure "pong"

newtype ApiResponse a
  = ApiResponse { _arpResult :: a }
  deriving (Show, Eq, Generic)

instance J.ToJSON a => J.ToJSON (ApiResponse a) where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

data SearchRequest
  = SearchRequest
  { _srqRegion :: !RegionCode
  , _srqFamily :: !ScientificName
  -- ^ 'ScientificName' of the family
  } deriving (Show, Eq, Generic)

instance J.FromJSON SearchRequest where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

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
  => SearchRequest -> m SearchResult
processSearch (SearchRequest regionCode familySciName) = do
  region <- validateRegion regionCode
  family <- validateFamily familySciName
  getCorpus region family

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
  => SearchRequest -> m [Bird]
processSpeciesSearch (SearchRequest regionCode familySciName) = do
  region <- validateRegion regionCode
  family <- validateFamily familySciName
  getSpeciesByRegionFamily region family

processImageSearch
  :: (MonadReader r m, HasFlickrConf r, MonadIO m)
  => [Bird] -> m SearchResult
processImageSearch = getImagesBySpecies

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
  => GetFamilyScientificNameRequest -> m GetFamilyScientificNameResponse
getFamilyScientificName (GetFamilyScientificNameRequest commonName) = do
  fams <- asks (^. axBirdFamiliesCache)
  let isSubStr v1 v2 = T.isInfixOf (T.toLower $ uCommonName v1) (T.toLower $ uCommonName v2)
      found = filter (isSubStr commonName . _fCommonName) (unFamiliesCache fams)
  pure $ GetFamilyScientificNameResponse found

validateRegion
  :: (MonadReader s m, HasAppCtx s, MonadError e m, AsEbirdError e)
  => RegionCode -> m Region
validateRegion region = do
  regionCache <- asks (^. axRegionsCache)
  let found = find (\x -> _rRegionCode x == region) (unRegionsCache regionCache)
  case found of
    Nothing -> throwError $ _EbirdErrorSearch # "Invalid region"
    Just r  -> return r

validateFamily
  :: (MonadReader s m, HasAppCtx s, MonadError e m, AsEbirdError e)
  => ScientificName -> m Family
validateFamily family = do
  fams <- asks (^. axBirdFamiliesCache)
  let found = find (\f -> _fScientificName f == family) (unFamiliesCache fams)
  case found of
    Nothing  -> throwError $ _EbirdErrorSearch # "Invalid family"
    Just fam -> return fam

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

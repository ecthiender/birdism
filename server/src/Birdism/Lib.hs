{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Birdism.Lib
  ( getCorpus
  , getSpeciesByRegionFamily
  , getImagesBySpecies
  , Family
  , Region (..)
  , SearchResult
  , CommonName
  , ImgUrl
  ) where

import           Control.Lens

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson.Casing        as J
import qualified Data.Aeson.TH            as J

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types

import qualified Birdism.Data             as Data
import qualified Service.Ebird            as ServiceEbird
import qualified Service.Flickr.Photos    as ServiceFlickr


type ImgUrl = Text

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
data SearchResultItem
  = SearchResultItem
  { _srrCommonName :: !CommonName
  , _srrImageUrls  :: ![ImgUrl]
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''SearchResultItem)

type SearchResult = [SearchResultItem]

getSpeciesByRegionFamily
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , AsEbirdError e
     )
  => RegionCode -> Family -> m [Bird]
getSpeciesByRegionFamily regCode family = do
  -- get all species that belongs to the family
  allSpecies <- Data.getAllSpecies family
  -- get all the available species in the region
  foundSpecies <- ServiceEbird.getSpeciesListByRegion regCode
  matchedSpecies <- traverse Data.makeBirdFromCode $ filter (`elem` allSpecies) foundSpecies
  pure $ catMaybes matchedSpecies

getImagesBySpecies
  :: (MonadReader s m, HasFlickrConf s, MonadIO m)
  => [Bird] -> m [SearchResultItem]
getImagesBySpecies = getImages

getCorpus
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , AsEbirdError e
     )
  => RegionCode -> Family -> m SearchResult
getCorpus regCode family = do
  matchedSpecies <- getSpeciesByRegionFamily regCode family
  getImages matchedSpecies

-- Given a list of 'Bird's, get the final search result, by combining the common names and a list
-- of image URLs into a hashmap
getImages :: (MonadReader s m, HasFlickrConf s, MonadIO m) => [Bird] -> m [SearchResultItem]
getImages birds = do
  let commonNames = map bCommonName birds
  zipWith SearchResultItem commonNames <$> getImageUrls
  where
    getImageUrls = do
      apiKey <- asks (^. fcKey)
      liftIO $ Async.forConcurrently birds $
        ServiceFlickr.searchPhotos apiKey . (uCommonName . bCommonName)

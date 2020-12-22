{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Lib
  ( getCorpus
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

import           Common
import           Config
import           Flickr.Photos            (searchPhotos)
import           Types

import qualified Data
import qualified Service.Ebird            as ServiceEbird


type ImgUrl = Text

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
data SearchResultItem
  = SearchResultItem
  { _srrCommonName :: !CommonName
  , _srrImageUrls  :: ![ImgUrl]
  } deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''SearchResultItem)

type SearchResult = [SearchResultItem]

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
  -- get all species that belongs to the family
  allSpecies <- Data.getSpecies family
  -- get all the available species in the region
  foundSpecies <- ServiceEbird.getSpeciesListByRegion regCode
  matchedSpecies <- traverse Data.makeBirdFromCode $ filter (`elem` allSpecies) foundSpecies
  getImages $ catMaybes matchedSpecies
  where
    -- Given a list of 'Bird's, get the final search result, by combining the common names and a list
    -- of image URLs into a hashmap
    getImages birds = do
      let commonNames = map bComName birds
      urls <- getImageUrls birds
      return $ map (\(cn, iurls) -> SearchResultItem cn iurls) $ zip commonNames urls

    getImageUrls birds = do
      apiKey <- asks (^. fcKey)
      liftIO $ Async.forConcurrently birds $
        searchPhotos apiKey . (uCommonName . bComName)

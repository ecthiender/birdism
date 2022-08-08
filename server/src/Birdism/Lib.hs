module Birdism.Lib
  ( getCorpus
  , getSpeciesByRegionFamily
  , getImagesBySpecies
  , Family
  , Region (..)
  , SearchResult
  , SearchResultItem (..)
  , CommonName
  , ImgUrl
  ) where

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types
import           Service.Flickr.Photos    (FlickrError)

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson               as J

import qualified Birdism.Data             as Data
import qualified Service.Ebird            as ServiceEbird
import qualified Service.Flickr.Photos    as ServiceFlickr

type ImgUrl = Text

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
data SearchResultItem
  = SearchResultItem
  { _srrCommonName :: !CommonName
  , _srrImageUrls  :: !(Either FlickrError [ImgUrl])
  } deriving (Show)

instance J.ToJSON SearchResultItem where
  toJSON (SearchResultItem name urls) =
    case urls of
      Left err ->
        J.object [ "common_name" J..= name
                 , "image_urls" J..= err
                 ]
      Right us ->
        J.object [ "common_name" J..= name
                 , "image_urls" J..= us
                 ]

type SearchResult = [SearchResultItem]

getSpeciesByRegionFamily
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , AsEbirdError e
     )
  => Region -> Family -> m [Bird]
getSpeciesByRegionFamily region family = do
  -- get all species that belongs to the family
  allSpecies <- Data.getAllSpecies family
  -- get all the available species in the region
  foundSpecies <- ServiceEbird.getSpeciesListByRegion (_rRegionCode region)
  matchedSpecies <- traverse Data.makeBirdFromCode $ filter (`elem` allSpecies) foundSpecies
  pure $ catMaybes matchedSpecies

getImagesBySpecies
  :: ( MonadReader s m
     , HasFlickrContext s
     , MonadIO m
     )
  => [CommonName] -> m [SearchResultItem]
getImagesBySpecies = getImages

getCorpus
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrContext r
     , AsEbirdError e
     )
  => Region -> Family -> m SearchResult
getCorpus region family =
  getSpeciesByRegionFamily region family >>= getImages . map bCommonName

-- Given a list of 'Bird's, get the final search result, by combining the common
-- names and a list of image URLs into a hashmap
getImages
  :: ( MonadReader r m
     , HasFlickrContext r
     , MonadIO m
     )
  => [CommonName] -> m [SearchResultItem]
getImages birds = do
  zipWith SearchResultItem birds <$> getImageUrls
  where
    getImageUrls = do
      r <- ask
      liftIO $ Async.forConcurrently birds $
        flip runReaderT r . runExceptT . ServiceFlickr.searchPhotos . uCommonName

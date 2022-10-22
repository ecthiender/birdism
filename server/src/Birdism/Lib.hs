module Birdism.Lib
  ( getCorpus
  , getSpeciesByRegionFamily
  , getImagesBySpecies
  , getImagesOfManySpecies
  , Family
  , Region (..)
  , SearchResult
  , SearchResultItem (..)
  , CommonName
  , ImgUrl (..)
  ) where

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types
import           Service.Flickr.Context   (FlickrError, AsFlickrError)

import qualified Control.Concurrent.Async as Async
import qualified Data.Aeson               as J

import qualified Birdism.Data             as Data
import qualified Service.Ebird            as ServiceEbird
import qualified Service.Flickr.Photos    as ServiceFlickr
import Control.Lens ((#))

newtype ImgUrl = ImgUrl { unImgUrl :: Text }
  deriving stock (Show, Eq)
  deriving newtype (J.ToJSON, J.FromJSON)

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
     , HasBirdismCache r
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
  :: ( MonadReader r m
     , MonadError e m
     , AsFlickrError e
     , AsApiError e
     , HasDbConfig r
     , HasFlickrConf r
     , HasBirdismCache r
     , MonadIO m
     )
  => SpeciesCode -> m [ImgUrl]
getImagesBySpecies spCode = do
  maybeBird <- Data.makeBirdFromCode spCode
  case maybeBird of
    Nothing -> throwError $ _ApiErrorSearchInvalidSpeciesCode # ()
    Just bird -> do
      let commonName = uCommonName $ bCommonName bird
      (fmap.fmap) ImgUrl $ ServiceFlickr.searchPhotos commonName {- HLINT ignore "Use <$>" -}

getImagesOfManySpecies
  :: ( MonadReader r m
     , HasFlickrConf r
     , HasBirdismCache r
     , MonadIO m
     )
  => [CommonName] -> m [SearchResultItem]
getImagesOfManySpecies = getImages

getCorpus
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , HasBirdismCache r
     , AsEbirdError e
     )
  => Region -> Family -> m SearchResult
getCorpus region family =
  getSpeciesByRegionFamily region family >>= getImages . map bCommonName

-- Given a list of 'Bird's, get the final search result, by combining the common
-- names and a list of image URLs into a hashmap
getImages
  :: ( MonadReader r m
     , HasFlickrConf r
     , HasBirdismCache r
     , MonadIO m
     )
  => [CommonName] -> m [SearchResultItem]
getImages birds = do
  urlTxts <- getImageUrls
  let urls = (fmap.fmap.fmap) ImgUrl urlTxts
  pure $ zipWith SearchResultItem birds urls
  where
    getImageUrls = do
      r <- ask
      liftIO $ Async.forConcurrently birds $
        flip runReaderT r . runExceptT . ServiceFlickr.searchPhotos . uCommonName

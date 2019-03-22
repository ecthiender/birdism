{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getCorpus
    , Family
    , Region (..)
    , Corpus
    , CommonName
    , ImgUrl
    , getImageUrls
    , App
    ) where

import           Control.Monad          (when)
import           Control.Monad.Except   (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, ReaderT, ask)
import           Data.Text              (Text)
import           Data.Time.Clock        (getCurrentTime)

import qualified Data.Aeson             as J
import qualified Data.HashMap.Strict    as Map
import qualified Data.Text              as T

import           Config
import           Ebird.Region           (ChecklistObservation (..),
                                         SubRegion (..), SubRegions (..),
                                         getSubRegions, searchCheckLists)
import           Flickr.Photos          (searchPhotos)
import           Types


type ImgUrl = Text

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
type Corpus = Map.HashMap CommonName [ImgUrl]


data AppError
  = AESearchError Text
  deriving (Show)

instance J.ToJSON AppError where
  toJSON a = case a of
    AESearchError e -> J.object [ "code" J..= ("search-error" :: Text)
                                , "error" J..= e
                                ]

-- Our own monad!
type App = ReaderT AppConfig (ExceptT AppError IO)

-- type alias our quite-used constriaints. we wan't IO capability and Reader of
-- our AppConfig
type Search m = (MonadIO m, MonadReader AppConfig m, MonadError AppError m)

sampleBird :: Bird
sampleBird = Bird "Poephila cincta atropygialis" "Black-throated Finch" "bktfin1" Nothing Nothing "Waxbills and Allies"

getCorpus :: Search m => Region -> Family -> m Corpus
getCorpus region family = do
  allSpecies <- getSpecies family
  regcode    <- getRegionCode region
  checklist  <- getChecklist regcode family
  let matchedSpecies = filter (\s -> bSpCode s `elem` spCodes allSpecies) (cBirds checklist)
  getImages matchedSpecies

  where
    spCodes = map bSpCode

getRegionCode :: Search m => Region -> m RegionCode
getRegionCode (Region region) = do
  subregions <- getSubRegions
  let isRegion x = T.toLower region == T.toLower (_srName x)
  let narrRegions = filter isRegion $ unSubRegions subregions
  liftIO $ print narrRegions
  when (null narrRegions) $
    throwError $ AESearchError $ "could not find region '" <> region <> "'"
  return $ _srCode $ head narrRegions

getChecklist :: Search m => RegionCode -> Family -> m Checklist
getChecklist rc family = do
  checklists <- searchCheckLists rc
  liftIO $ print checklists
  let birds = map checklistToBird checklists
  return $ Checklist rc birds
  where
    checklistToBird (ChecklistObservation scName comName spCode) =
      Bird scName comName spCode Nothing Nothing family

getSpecies :: Search m => Family -> m [Bird]
getSpecies family = do
  let q = "select sp_code from taxonomy_db where lower(family_common_name) "
        <> "like :family:"
      param = T.toLower family
  ct <- liftIO getCurrentTime
  liftIO $ print ct
  return [sampleBird]

getImages :: Search m => [Bird] -> m Corpus
getImages birds = do
  let cns = map bComName birds
  urls <- mapM (getImageUrls . bComName) birds
  let x = (head cns, head urls)
  liftIO $ print x
  return $ Map.fromList $ zip cns urls

getImageUrls :: Search m => CommonName -> m [ImgUrl]
getImageUrls cn = do
  config <- ask
  let apiKey = fcKey $ acFlickr config
  liftIO $ searchPhotos apiKey cn

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( getCorpus
    , Family
    , Region (..)
    ) where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Reader   (ReaderT, ask)
import           Data.Time.Clock        (getCurrentTime)

import qualified Data.Map.Strict        as M
import qualified Data.Text              as T
import qualified Network.Wreq           as W

import           Config


type ScientificName = T.Text
type CommonName     = T.Text
type SpeciesCode    = T.Text

type Family         = T.Text
type RegionCode     = T.Text
type ImgUrl         = T.Text

data Bird = Bird
  { bScName        :: !ScientificName
  , bComName       :: !CommonName
  , bSpCode        :: !SpeciesCode
  , bCategory      :: !(Maybe T.Text)
  , bOrder         :: !(Maybe T.Text)
  , bFamilyComName :: !T.Text
  } deriving (Show, Eq)

newtype Region = Region { getRegion :: T.Text }
  deriving (Show)

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
type Corpus = M.Map CommonName [ImgUrl]

data Checklist
  = Checklist
  { cRegion :: RegionCode
  , cBirds  :: [Bird]
  } deriving (Show, Eq)


-- Our own monad!
type App = ReaderT AppConfig IO

sampleBird :: Bird
sampleBird = Bird "Poephila cincta atropygialis" "Black-throated Finch" "bktfin1" Nothing Nothing "Waxbills and Allies"


getCorpus :: Region -> Family -> App Corpus
getCorpus region family = do
  allSpecies <- getSpecies family
  checklist  <- getChecklist =<< getRegionCode region
  let matchedSpecies = filter (\s -> bSpCode s `elem` spCodes allSpecies) (cBirds checklist)
  getImageUrls matchedSpecies
  where spCodes = map bSpCode

getRegionCode :: Region -> App RegionCode
getRegionCode region = do
  r <- ask
  liftIO $ print $ (ebcToken . acEBird) r
  ct <- liftIO getCurrentTime
  liftIO $ print ct
  return "US-NY-109"

getChecklist :: RegionCode -> App Checklist
getChecklist rc = do
  ct <- liftIO getCurrentTime
  liftIO $ print ct
  return $ Checklist rc [sampleBird]

getSpecies :: Family -> App [Bird]
getSpecies family = do
  ct <- liftIO getCurrentTime
  liftIO $ print ct
  return [sampleBird]

getImageUrls :: [Bird] -> App Corpus
getImageUrls birds = do
  ct <- liftIO getCurrentTime
  liftIO $ print ct
  let cn = bComName $ head birds
  return $ M.fromList [(cn, ["img-url-1"])]

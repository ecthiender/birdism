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
  , FamilyNames
  , RegionNames
  , getImageUrls
  , getFamilyNames
  , getRegionNames
  , getSpeciesList
  ) where

import           Common
import           Control.Lens

import qualified Control.Concurrent.Async   as Async
import qualified Data.Aeson.Casing          as J
import qualified Data.Aeson.TH              as J
import qualified Database.PostgreSQL.Simple as PG

import           Config
import           Ebird.Region               (ChecklistObservation (..), searchCheckLists)
import           Flickr.Photos              (searchPhotos)
import           Types


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

getSpeciesList
  :: ( MonadIO m
     , MonadReader r m
     , MonadError e m
     , HasDbConfig r
     , HasEBirdConf r
     , HasFlickrConf r
     , AsEbirdError e
     )
  => RegionName -> Family -> m [Bird]
getSpeciesList region family = do
  allSpecies <- getSpecies family
  debugTrace (Just "ALL SPECIES") allSpecies
  liftIO $ print $ length allSpecies
  regcode    <- getRegionCode region
  debugTrace (Just "REGION CODE") regcode
  checklist  <- getChecklist regcode family
  debugTrace (Just "CHECKLIST") (map bSpCode $ cBirds checklist)
  liftIO $ print $ length (cBirds checklist)
  let matchedSpecies = filter (\s -> bSpCode s `elem` allSpecies) $ cBirds checklist
  debugTrace (Just "MATCHED SPECIES") matchedSpecies
  return matchedSpecies

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
  allSpecies <- getSpecies family
  debugTrace (Just "ALL SPECIES") $ map uSpeciesCode allSpecies
  liftIO $ print $ length allSpecies
  -- regcode  <- getRegionCode region
  -- debugTrace (Just "REGION CODE") regcode
  checklist  <- getChecklist regCode family
  debugTrace (Just "CHECKLIST") (map (uSpeciesCode . bSpCode) $ cBirds checklist)
  liftIO $ print $ length (cBirds checklist)
  let matchedSpecies = filter (\s -> bSpCode s `elem` allSpecies) $ cBirds checklist
  debugTrace (Just "MATCHED SPECIES") matchedSpecies
  liftIO $ print $ length matchedSpecies
  getImages matchedSpecies

getRegionCode
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , MonadError e m
     , AsEbirdError e
     )
  => RegionName -> m RegionCode
getRegionCode (RegionName region) = do
  conn <- asks (^. dbConnection)
  let q = "SELECT region_code FROM region WHERE region_name = ?"
  res <- liftIO $ PG.query conn q (PG.Only region)
  case res of
    []      -> throwError $ (_EbirdErrorSearch #) $ "could not find region '" <> region <> "'"
    (reg:_) -> return $ (RegionCode . PG.fromOnly) reg

-- | Given a 'RegionCode'
getChecklist
  :: ( MonadIO m
     , MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     )
  => RegionCode -> Family -> m Checklist
getChecklist rc family = do
  checklists <- searchCheckLists rc
  --liftIO $ print checklists
  let birds = map checklistToBird checklists
  return $ Checklist rc birds
  where
    checklistToBird (ChecklistObservation spCode comName sciName) =
      Bird (ScientificName sciName) (CommonName comName) (SpeciesCode spCode)
           Nothing Nothing (uCommonName $ _fCommonName family)


-- | Given a 'Family' name, fetch a list of species belonging to that family. This is fetched from
-- our database, which contains the entire taxonomy for now
getSpecies
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     )
  => Family -> m [SpeciesCode]
getSpecies (Family _scName family) = do
  r <- ask
  let conn = r ^. dbConnection
  let q = "SELECT species_code FROM taxonomy WHERE family_common_name = ?"
  res <- liftIO $ PG.query conn q (PG.Only $ uCommonName family)
  return $ (SpeciesCode . PG.fromOnly) <$> res

-- | Given a list of 'Bird's, get the final search result, by combining the common names and a list
-- of image URLs into a hashmap
getImages
  :: ( MonadIO m
     , MonadReader r m
     , HasFlickrConf r
     )
  => [Bird] -> m SearchResult
getImages birds = do
  let commonNames = map bComName birds
  urls <- getImageUrls birds
  return $ map (\(cn, iurls) -> SearchResultItem cn iurls) $ zip commonNames urls

-- | Uses 'Async' to concurrently and asynchronously get images from 'searchPhotos' service
getImageUrls
  :: ( MonadIO m
     , MonadReader r m
     , HasFlickrConf r
     )
  => [Bird] -> m [[ImgUrl]]
getImageUrls birds = do
  apiKey <- asks (^. fcKey)
  liftIO $ Async.forConcurrently birds $
    searchPhotos apiKey . (uCommonName . bComName)

------------ list of bird families of the world ------------------
type FamilyNames = [Family]

getFamilyNamesQuery :: PG.Query
getFamilyNamesQuery =
  "SELECT DISTINCT family_scientific_name, family_common_name FROM taxonomy"

-- | TODO: handle postgres errors
getFamilyNames :: (MonadIO m, MonadReader r m, HasDbConfig r) => m FamilyNames
getFamilyNames = do
  conn <- asks (^. dbConnection)
  res <- liftIO $ PG.query_ conn getFamilyNamesQuery
  let familyCommonNames = map (\(x, y) -> Family (ScientificName x) (CommonName y)) $
                          filter (\(x,y) -> x /= "" && y /= "") $
                          map (\(x,y) -> (fromMaybe "" x, fromMaybe "" y)) res
  return familyCommonNames

----------------- list of regions ---------------
type RegionNames = [Region]

getRegionNamesQuery :: PG.Query
getRegionNamesQuery =
  "SELECT DISTINCT region_code, region_name FROM region"

getRegionNames
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     )
  => m RegionNames
getRegionNames = do
  conn <- asks (^. dbConnection)
  res <- liftIO $ PG.query_ conn getRegionNamesQuery
  let regionNames = map (\(x, y)-> Region (RegionCode x) (RegionName y)) $
                    filter (\(x,y) -> x /= "" && y /= "") $
                    map (\(x,y) -> (fromMaybe "" x, fromMaybe "" y)) res
  return regionNames

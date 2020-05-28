{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
  , App
  ) where

import           Control.Monad.Except       (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)

import qualified Control.Concurrent.Async   as Async
import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG

import           Config
import           Ebird.Region               (ChecklistObservation (..), RRegion (..),
                                             SubRegions (..), getSubRegions, searchCheckLists)
import           Flickr.Photos              (searchPhotos)
import           Types


type ImgUrl = Text

-- Corpus is the result type of this program. This basically means the corpus
-- (images of birds) for the end user to study
type SearchResult = Map.HashMap CommonName [ImgUrl]

data AppError
  = AESearchError !Text
  | AEDbError !Text
  deriving (Show)

instance J.ToJSON AppError where
  toJSON a = case a of
    AESearchError e -> encodeErr "search-error" e
    AEDbError e     -> encodeErr "db-error" e

encodeErr :: Text -> Text -> J.Value
encodeErr code e =
  J.object [ "code" J..= code
           , "error" J..= e
           ]

-- Our own monad!
type App = ReaderT AppConfig (ExceptT AppError IO)

-- type alias our quite-used constriaints. we want IO capability and Reader of our AppConfig
type MonadApp m = (MonadIO m, MonadReader AppConfig m, MonadError AppError m)

getSpeciesList :: MonadApp m => Region -> Family -> m [Bird]
getSpeciesList region family = do
  allSpecies <- getSpecies family
  debug "ALL SPECIES" allSpecies
  liftIO $ print $ length allSpecies
  regcode    <- getRegionCode region
  debug "REGION CODE" regcode
  checklist  <- getChecklist regcode family
  debug "CHECKLIST" (map bSpCode $ cBirds checklist)
  liftIO $ print $ length (cBirds checklist)
  let matchedSpecies = filter (\s -> bSpCode s `elem` allSpecies) $ cBirds checklist
  debug "MATCHED SPECIES" matchedSpecies
  return matchedSpecies

getCorpus :: MonadApp m => Region -> Family -> m SearchResult
getCorpus region family = do
  allSpecies <- getSpecies family
  debug "ALL SPECIES" allSpecies
  liftIO $ print $ length allSpecies
  regcode    <- getRegionCode region
  debug "REGION CODE" regcode
  checklist  <- getChecklist regcode family
  debug "CHECKLIST" (map bSpCode $ cBirds checklist)
  liftIO $ print $ length (cBirds checklist)
  let matchedSpecies = filter (\s -> bSpCode s `elem` allSpecies) $ cBirds checklist
  debug "MATCHED SPECIES" matchedSpecies
  liftIO $ print $ length matchedSpecies
  getImages matchedSpecies

debug banner matter = do
  liftIO $ putStrLn "===============================>>>>>>>>"
  liftIO $ putStrLn "===============================>>>>>>>>"
  liftIO $ putStrLn (banner <> " ....")
  liftIO $ print matter
  liftIO $ putStrLn "<<<<<===============================>>>>>>>>"
  liftIO $ putStrLn "<<<<<===============================>>>>>>>>"


getRegionCode :: MonadApp m => Region -> m RegionCode
getRegionCode (Region region) = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  let q = "SELECT region_code FROM region WHERE region_name = ?"
  res <- liftIO $ PG.query conn q (PG.Only region)
  case res of
    []      -> throwError $ AESearchError $ "could not find region '" <> region <> "'"
    (reg:_) -> return $ (RegionCode . PG.fromOnly) reg


-- | Given a 'RegionCode'
getChecklist :: MonadApp m => RegionCode -> Family -> m Checklist
getChecklist rc family = do
  checklists <- searchCheckLists rc
  --liftIO $ print checklists
  let birds = map checklistToBird checklists
  return $ Checklist rc birds
  where
    checklistToBird (ChecklistObservation spCode comName sciName) =
      Bird (ScientificName sciName) (CommonName comName) (SpeciesCode spCode)
           Nothing Nothing (uFamily family)


-- | Given a 'Family' name, fetch a list of species belonging to that family. This is fetched from
-- our database, which contains the entire taxonomy for now
getSpecies :: MonadApp m => Family -> m [SpeciesCode]
getSpecies (Family family) = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  let q = "SELECT species_code FROM taxonomy WHERE family_common_name = ?"
  res <- liftIO $ PG.query conn q (PG.Only family)
  return $ (SpeciesCode . PG.fromOnly) <$> res

-- | Given a list of 'Bird's, get the final search result, by combining the common names and a list
-- of image URLs into a hashmap
getImages :: MonadApp m => [Bird] -> m SearchResult
getImages birds = do
  let commonNames = map bComName birds
  urls <- getImageUrls birds
  return $ Map.fromList $ zip commonNames urls

-- | Uses 'Async' to concurrently and asynchronously get images from 'searchPhotos' service
getImageUrls :: MonadApp m => [Bird] -> m [[ImgUrl]]
getImageUrls birds = do
  apiKey <- asks $ fcKey . acFlickr
  liftIO $ Async.forConcurrently birds $
    searchPhotos apiKey . (uCommonName . bComName)

------------ list of bird families of the world ------------------
type FamilyNames = Map.HashMap Text Text

getFamilyNamesQuery :: PG.Query
getFamilyNamesQuery =
  "SELECT DISTINCT family_scientific_name, family_common_name FROM taxonomy"

getFamilyNames :: MonadApp m => m FamilyNames
getFamilyNames = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  res <- liftIO $ PG.query_ conn getFamilyNamesQuery
  let familyCommonNames = Map.fromList $
                          filter (\(x,y) -> x /= "" && y /= "") $
                          map (\(x,y) -> (fromMaybe "" x, fromMaybe "" y)) res
  liftIO $ PG.close conn
  return familyCommonNames

----------------- list of regions ---------------
type RegionNames = Map.HashMap Text Text

getRegionNamesQuery :: PG.Query
getRegionNamesQuery =
  "SELECT DISTINCT region_code, region_name FROM region"

getRegionNames :: MonadApp m => m RegionNames
getRegionNames = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  res <- liftIO $ PG.query_ conn getRegionNamesQuery
  let regionNames = Map.fromList $
                    filter (\(x,y) -> x /= "" && y /= "") $
                    map (\(x,y) -> (fromMaybe "" x, fromMaybe "" y)) res
  liftIO $ PG.close conn
  return regionNames

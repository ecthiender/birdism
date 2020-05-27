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
  , getImageUrls
  , getFamilyNames
  , App
  ) where

import           Control.Monad.Except       (ExceptT, MonadError, throwError)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (MonadReader, ReaderT, asks)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)

import qualified Data.Aeson                 as J
import qualified Data.HashMap.Strict        as Map
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.SQLite.Simple     as Db

import           Config
import           Ebird.Region               (ChecklistObservation (..), SubRegion (..),
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

  where
    debug banner matter = do
      liftIO $ putStrLn "===============================>>>>>>>>"
      liftIO $ putStrLn "===============================>>>>>>>>"
      liftIO $ putStrLn (banner <> " ....")
      liftIO $ print matter
      liftIO $ putStrLn "<<<<<===============================>>>>>>>>"
      liftIO $ putStrLn "<<<<<===============================>>>>>>>>"


getRegionCode :: MonadApp m => Region -> m RegionCode
getRegionCode (Region region) = do
  subregions <- getSubRegions
  let isRegion x = T.toLower region == T.toLower (_srName x)
  let narrRegions = filter isRegion $ unSubRegions subregions
  --liftIO $ print narrRegions
  case narrRegions of
    []      -> throwError $ AESearchError $ "could not find region '" <> region <> "'"
    (reg:_) -> return $ _srCode reg

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


newtype DbRes = DbRes Text

instance Db.FromRow DbRes where
  fromRow = DbRes <$> Db.field

-- | Given a 'Family' name, fetch a list of species belonging to that family. This is fetched from
-- our database, which contains the entire taxonomy for now
getSpecies :: MonadApp m => Family -> m [SpeciesCode]
getSpecies (Family family) = do
  conn <- liftIO $ Db.open "../ws/bird.db"
  let q = "select sp_code from taxonomy_db where lower(family_common_name) "
          <> "like :family"
      _family = "%" <> family <> "%"
  res <- liftIO $ Db.queryNamed conn q [":family" Db.:= T.toLower _family]
  return $ map (\(DbRes r) -> SpeciesCode r) res

-- | Given a list of 'Bird's, get the final search result, by combining the common names and a list
-- of image URLs into a hashmap
getImages :: MonadApp m => [Bird] -> m SearchResult
getImages birds = do
  let commonNames = map bComName birds
  urls <- mapM (getImageUrls . bComName) birds
  return $ Map.fromList $ zip commonNames urls

-- | Given a 'CommonName' fetch a list of image urls from flickr
getImageUrls :: MonadApp m => CommonName -> m [ImgUrl]
getImageUrls (CommonName cn) = do
  apiKey <- asks $ fcKey . acFlickr
  liftIO $ searchPhotos apiKey cn

getFamilyNames :: MonadApp m => m [Text]
getFamilyNames = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  res <- liftIO $ PG.query_ conn "select distinct(family_common_name) from taxonomy"
  let familyCommonNames = fmap (fromMaybe "" . PG.fromOnly) res
  liftIO $ putStrLn $ ">>> Total no of families found: " <> show (length familyCommonNames)
  return familyCommonNames

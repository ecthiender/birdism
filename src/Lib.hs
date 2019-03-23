{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

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

import qualified Data.Aeson             as J
import qualified Data.HashMap.Strict    as Map
import qualified Data.Text              as T
import qualified Database.SQLite.Simple as Db

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
  | AEDbError Text
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

-- type alias our quite-used constriaints. we wan't IO capability and Reader of
-- our AppConfig
type Search m = (MonadIO m, MonadReader AppConfig m, MonadError AppError m)


getCorpus :: Search m => Region -> Family -> m Corpus
getCorpus region family = do
  allSpecies <- getSpecies family
  debug "ALL SPECIES" allSpecies
  liftIO $ print $ length allSpecies
  regcode    <- getRegionCode region
  debug "REGION CODE" regcode
  checklist  <- getChecklist regcode family
  debug "CHECKLIST" (map bSpCode $ cBirds checklist)
  liftIO $ print $ length (cBirds checklist)
  let matchedSpecies =
        filter (\s -> bSpCode s `elem` allSpecies) $ cBirds checklist
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


getRegionCode :: Search m => Region -> m RegionCode
getRegionCode (Region region) = do
  subregions <- getSubRegions
  let isRegion x = T.toLower region == T.toLower (_srName x)
  let narrRegions = filter isRegion $ unSubRegions subregions
  --liftIO $ print narrRegions
  when (null narrRegions) $
    throwError $ AESearchError $ "could not find region '" <> region <> "'"
  return $ _srCode $ head narrRegions

getChecklist :: Search m => RegionCode -> Family -> m Checklist
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

getSpecies :: Search m => Family -> m [SpeciesCode]
getSpecies (Family family) = do
  conn <- liftIO $ Db.open "../ws/bird.db"
  let q = "select sp_code from taxonomy_db where lower(family_common_name) "
          <> "like :family"
      _family = "%" <> family <> "%"
  res <- liftIO $ Db.queryNamed conn q [":family" Db.:= T.toLower _family]
  return $ map (\(DbRes r) -> SpeciesCode r) res

getImages :: Search m => [Bird] -> m Corpus
getImages birds = do
  let cns = map bComName birds
  urls <- mapM (getImageUrls . bComName) birds
  --let x = (head cns, head urls)
  --liftIO $ print x
  return $ Map.fromList $ zip cns urls

getImageUrls :: Search m => CommonName -> m [ImgUrl]
getImageUrls cn = do
  config <- ask
  let apiKey = fcKey $ acFlickr config
  liftIO $ searchPhotos apiKey (uCommonName cn)

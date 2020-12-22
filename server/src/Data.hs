{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Module to encapsulate functions that work with the database to fetch data

module Data where

import           Control.Lens
import qualified Database.PostgreSQL.Simple as PG

import           Common
import           Config
import           Types


------------ list of bird families of the world ------------------
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

-- | Given a 'Family' name, fetch a list of species belonging to that family. This is fetched from
-- our database, which contains the entire taxonomy for now
getSpecies
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     )
  => Family -> m [SpeciesCode]
getSpecies (Family _scName family) = do
  conn <- asks (^. dbConnection)
  let q = "SELECT species_code FROM taxonomy WHERE family_common_name = ?"
  res <- liftIO $ PG.query conn q (PG.Only $ uCommonName family)
  return $ SpeciesCode . PG.fromOnly <$> res

makeBirdFromCode
  :: (MonadIO m, MonadReader r m, HasDbConfig r)
  => SpeciesCode -> m (Maybe Bird)
makeBirdFromCode spCode = do
  conn <- asks (^. dbConnection)
  let q = "SELECT scientific_name, common_name, family_common_name, family_scientific_name FROM taxonomy WHERE species_code = ?"
  res <- liftIO $ PG.query conn q (PG.Only spCode)
  case res of
    []                                          -> return Nothing
    ((scName, commName, famCName, famScName):_) -> return $ Just $ makeBird spCode commName scName (Family famScName famCName)

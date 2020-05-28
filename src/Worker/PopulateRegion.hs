{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker.PopulateRegion where

import           Control.Concurrent         (threadDelay)
import           Control.Monad              (forM_)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Reader       (runReaderT)
import           Data.List.Split            (chunksOf)

import qualified Control.Concurrent.Async   as Async
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG

import           Config
import           Ebird.Region
import           Types


populateRegion :: AppConfig -> IO ()
populateRegion config = do
  conn <- liftIO $ PG.connectPostgreSQL "postgres://postgres:@localhost:5432/bih"
  truncateRegionTable conn
  countries <- runReaderT getCountries config
  asyncHandle <- Async.async $ getSubnats1Async conn countries
  insertRegions conn countries
  subnats1 <- Async.wait asyncHandle
  liftIO $ putStrLn "[DEBUG] inserted countries and subnational1s. cooling off for 10 secs before getting subnational2s"
  threadDelay (1000 * 1000 * 10)
  let batchedSubnats1 = chunksOf 10 subnats1
  forM_ batchedSubnats1 $ \subnats1Batch -> do
    liftIO $ putStrLn "[DEBUG] running one batch of subnational1s"
    Async.mapConcurrently_ (getSubnats2Async conn) subnats1Batch
    liftIO $ putStrLn "[DEBUG] inserted one subnational2s. cooling off for 10 secs.."
    threadDelay (1000 * 1000 * 10)
  where
    getSubnats1Async conn countries = do
      Async.forConcurrently countries $ \country -> do
        putStrLn $ "[DEBUG] getting subnational-1 region of: " <> T.unpack (uRegionCode $ _rCode country)
        subnats1 <- runReaderT (getSubnationa1Regions country) config
        insertRegions conn subnats1
        return subnats1

    getSubnats2Async conn subnats1 = do
      Async.forConcurrently subnats1 $ \subnat1 -> do
        subnats2 <- runReaderT (getSubnationa2Regions subnat1) config
        insertRegions conn subnats2

truncateRegionTable :: PG.Connection -> IO ()
truncateRegionTable conn = do
  let q = "DELETE FROM region"
  res <- PG.execute_ conn q
  liftIO $ putStrLn $ "TRUNCATED region TABLE. Deleted Rows: " <> show res

insertRegions :: PG.Connection -> [RRegion a] -> IO ()
insertRegions conn regions = do
  let q = "INSERT INTO region (region_code, region_name) VALUES (?, ?)"
  res <- PG.executeMany conn q $ map (\(RRegion code name) -> (uRegionCode code, name)) regions
  liftIO $ putStrLn $ "INSERTED. Affected Rows: " <> show res

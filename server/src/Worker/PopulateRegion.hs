{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker.PopulateRegion where

import           Common
import           Control.Concurrent         (threadDelay)
-- import           Control.Monad              (forM_)
-- import           Control.Monad.IO.Class     (liftIO)
-- import           Control.Monad.Reader       (runReaderT)
import           Data.List.Split            (chunksOf)

import qualified Control.Concurrent.Async   as Async
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG

import           Config
import           Ebird.Region
import           Init
import           Types


populateRegion :: AppConfig -> IO ()
populateRegion config = do
  ctx@AppCtx{..} <- initialiseAppCtx config
  truncateRegionTable (_dbConnection _axDbConn)
  countries <- run getCountries _axEbirdConf
  asyncHandle <- liftIO $ Async.async $ getSubnats1Async ctx countries
  insertRegions (_dbConnection $ _axDbConn) countries
  subnats1 <- liftIO $ Async.wait asyncHandle
  putStrLn "[DEBUG] inserted countries and subnational1s. cooling off for 10 secs before getting subnational2s"
  threadDelay (1000 * 1000 * 10)
  let batchedSubnats1 = chunksOf 10 subnats1
  forM_ batchedSubnats1 $ \subnats1Batch -> do
    putStrLn "[DEBUG] running one batch of subnational1s"
    Async.mapConcurrently_ (getSubnats2Async ctx) subnats1Batch
    putStrLn "[DEBUG] inserted one subnational2s. cooling off for 10 secs.."
    threadDelay (1000 * 1000 * 10)
  where
    getSubnats1Async ctx countries = do
      Async.forConcurrently countries $ \country -> do
        putStrLn $ "[DEBUG] getting subnational-1 region of: " <> T.unpack (uRegionCode $ _rCode country)
        subnats1 <- run (getSubnationa1Regions country) ctx
        insertRegions (_dbConnection $ _axDbConn ctx) subnats1
        return subnats1

    getSubnats2Async ctx subnats1 = do
      Async.forConcurrently subnats1 $ \subnat1 -> do
        subnats2 <- run (getSubnationa2Regions subnat1) ctx
        insertRegions (_dbConnection $ _axDbConn ctx) subnats2

truncateRegionTable :: PG.Connection -> IO ()
truncateRegionTable conn = do
  let q = "DELETE FROM region"
  res <- liftIO $ PG.execute_ conn q
  liftIO $ putStrLn $ "TRUNCATED region TABLE. Deleted Rows: " <> show res

insertRegions :: PG.Connection -> [RRegion a] -> IO ()
insertRegions conn regions = do
  let q = "INSERT INTO region (region_code, region_name) VALUES (?, ?)"
  res <- liftIO $ PG.executeMany conn q $ map (\(RRegion code name) -> (uRegionCode code, name)) regions
  liftIO $ putStrLn $ "INSERTED. Affected Rows: " <> show res

run :: ReaderT r (ExceptT AppError IO) a -> r -> IO a
run a cfg = do
  res <- runExceptT $ runReaderT a cfg
  case res of
    Left err -> error $ "AppError occurred: " <> show err
    Right v  -> return v

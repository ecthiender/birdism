{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           System.Exit                (exitFailure)

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Network.Wai.Handler.Warp   as Warp
import qualified System.Environment         as Sys

import           Birdism.Common
import           Birdism.Config
import           Birdism.Docs               (generateDocs)
import           Birdism.Init
import           Birdism.Server
import           Worker.PopulateRegion
import           Worker.PopulateTaxonomy

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    ["serve"]       -> runServer
    ["docs"]        -> generateDocs
    ["seed-taxa"]   -> runWorker populateTaxonomy
    ["seed-region"] -> runWorker populateRegion
    ["help"]        -> printUsage
    ["--help"]      -> printUsage
    _               -> putStrLn "Invalid command." >> printUsage >> exitFailure

runServer :: IO ()
runServer = do
  res <- runExceptT $ readConfig >>= initialiseAppCtx
  case res of
    Left e    -> exitWithError $ J.encode e
    Right ctx -> liftIO $ Warp.run (_axServerPort ctx) $ httpApp ctx

runWorker :: (AppConfig -> IO ()) -> IO ()
runWorker fn = do
  res <- runExceptT readConfig
  case res of
    Left e     -> exitWithError $ J.encode e
    Right conf -> fn conf

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "birdism serve|seed-taxa|seed-region"

exitWithError :: BL.ByteString -> IO ()
exitWithError msg = BL.putStrLn msg >> exitFailure

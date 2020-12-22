{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Common
import           System.Exit                (exitFailure)
import           Web.Spock.Core

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified System.Environment         as Sys

import           Config
import           Init
import           Server
import           Worker.PopulateRegion
import           Worker.PopulateTaxonomy

main :: IO ()
main = do
  args <- Sys.getArgs
  case args of
    ("serve":_)       -> runServer
    ("seed-taxa":_)   -> seed populateTaxonomy
    ("seed-region":_) -> seed populateRegion
    ("help":_)        -> printUsage
    ("--help":_)      -> printUsage
    _                 -> putStrLn "Invalid command." >> printUsage >> exitFailure

runServer :: IO ()
runServer = do
  res <- runExceptT $ readConfig >>= initialiseAppCtx
  case res of
    Left e    -> printExit $ J.encode e
    Right ctx -> liftIO $ runSpock (_axServerPort ctx) $ httpApp ctx

seed :: (AppConfig -> IO ()) -> IO ()
seed fn = do
  res <- runExceptT readConfig
  case res of
    Left e     -> printExit $ J.encode e
    Right conf -> fn conf

printUsage :: IO ()
printUsage = do
  putStrLn "Usage:"
  putStrLn "birdsim serve|seed-taxa|seed-region"

printExit :: BL.ByteString -> IO ()
printExit msg = BL.putStrLn msg >> exitFailure

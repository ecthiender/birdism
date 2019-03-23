{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Maybe         (fromMaybe)
import           Data.Text          (Text)
import           System.Environment (lookupEnv)
import           System.Exit        (exitFailure)
import           Web.Spock.Core

import qualified Data.Aeson         as J
import qualified Data.ByteString    as B
import qualified Data.Text          as T
import qualified Data.Text.IO       as T

import           Server


defaultConfigFileEnv :: String
defaultConfigFileEnv = "CORMORANT_CONFIG_FILE"

defaultConfigFilepath :: FilePath
defaultConfigFilepath = "./cormorant_conf.json"

failExit :: Text -> IO ()
failExit msg = T.putStrLn msg >> exitFailure

main :: IO ()
main = do
  env <- lookupEnv defaultConfigFileEnv
  let filepath = fromMaybe defaultConfigFilepath env
  configFile <- B.readFile filepath
  case J.eitherDecodeStrict configFile of
    Left e       -> failExit $ "error parsing config file: " <> T.pack e
    Right config -> runSpock 8888 $ httpApp config

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.Reader
import           Data.Maybe           (fromMaybe)
import           Data.Text            (Text)
import           System.Environment   (lookupEnv)
import           System.Exit          (exitFailure)
import           Web.Spock.Core

import qualified Data.Aeson           as J
import qualified Data.ByteString      as B
import qualified Data.Text            as T
import qualified Data.Text.IO         as T

import           Config
import           Server


defaultConfigFileEnv :: String
defaultConfigFileEnv = "BIRDISM_CONFIG_FILE"

defaultConfigFilepath :: FilePath
defaultConfigFilepath = "./birdism_conf.json"

printExit :: Text -> IO ()
printExit msg = T.putStrLn msg >> exitFailure

newtype AppM a
  = AppM { unAppM :: ReaderT AppConfig IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppConfig)

runAppM :: AppM a -> AppConfig -> IO a
runAppM app config = runReaderT (unAppM app) config

main :: IO ()
main = do
  env <- lookupEnv defaultConfigFileEnv
  let filepath = fromMaybe defaultConfigFilepath env
  configFile <- B.readFile filepath
  case J.eitherDecodeStrict configFile of
    Left e       -> printExit $ "FATAL ERROR: error parsing config file: " <> T.pack e
    Right config -> runSpock 8888 $ httpApp config

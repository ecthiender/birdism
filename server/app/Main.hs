{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Control.Monad.Reader
import           Data.Text            (Text)
import           System.Exit          (exitFailure)
import           Web.Spock.Core

import qualified Data.Text.IO         as T

import           Config
import           Server


printExit :: Text -> IO ()
printExit msg = T.putStrLn msg >> exitFailure

newtype AppM a
  = AppM { unAppM :: ReaderT AppCtx IO a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx)

runAppM :: AppM a -> AppCtx -> IO a
runAppM app config = runReaderT (unAppM app) config

main :: IO ()
main = do
  readConfig >>= \case
    Left e       -> printExit e
    Right config -> do
      appCtx <- initialiseAppCtx config
      runSpock 8888 $ httpApp appCtx

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Common
import           System.Exit    (exitFailure)
import           Web.Spock.Core

import qualified Data.Text.IO   as T

import           Config
import           Server

printExit :: Text -> IO ()
printExit msg = T.putStrLn msg >> exitFailure

main :: IO ()
main = do
  res <- runExceptT $ (readConfig >>= initialiseAppCtx)
  case res of
    Left e    -> err e
    Right ctx -> liftIO $ runSpock 8888 $ httpApp ctx

  where
    err = \case
      AESearchError e -> printExit e
      AEDbError e -> printExit e
      AEConfigError e -> printExit e

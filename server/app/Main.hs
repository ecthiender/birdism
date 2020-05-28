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
  res <- runExceptT $ readConfig >>= initialiseAppCtx
  case res of
    Left e    -> printExit $ getErrMsg e
    Right ctx -> liftIO $ runSpock (axServerPort ctx) $ httpApp ctx
  where
    getErrMsg err =
      let msg = case err of
            AESearchError e -> e
            AEDbError e     -> e
            AEConfigError e -> e
      in msg

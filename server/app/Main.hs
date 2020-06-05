{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Main where

import           Common
import           System.Exit                (exitFailure)
import           Web.Spock.Core

import qualified Data.Aeson                 as J
import qualified Data.ByteString.Lazy.Char8 as BL

import           Config
import           Init
import           Server

main :: IO ()
main = do
  res <- runExceptT $ readConfig >>= initialiseAppCtx
  case res of
    Left e    -> printExit $ J.encode e
    Right ctx -> liftIO $ runSpock (_axServerPort ctx) $ httpApp ctx

printExit :: BL.ByteString -> IO ()
printExit msg = BL.putStrLn msg >> exitFailure

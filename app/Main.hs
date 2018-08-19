{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Reader

import           Config
import           Lib

main :: IO ()
main = do
  let x = Region "abcd"
      y = "abcd"
  res <- runReaderT (getCorpus x y) (mkConf "a" "b" "c")
  print res

{-# LANGUAGE TemplateHaskell #-}

module Config
  ( AppConfig (..)
  , EBirdConf (..)
  , FlickrConf (..)
  , mkConf
  ) where

import           Data.Text         (Text)

import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J


newtype EBirdConf
  = EBirdConf { ebcToken :: Text }
  deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''EBirdConf)

data FlickrConf
  = FlickrConf
  { fcKey    :: !Text
  , fcSecret :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''FlickrConf)

data AppConfig
  = AppConfig
  { acEbird  :: !EBirdConf
  , acFlickr :: !FlickrConf
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''AppConfig)

mkConf :: Text -> Text -> Text -> AppConfig
mkConf ebToken fKey fSecret =
  AppConfig (EBirdConf ebToken) (FlickrConf fKey fSecret)

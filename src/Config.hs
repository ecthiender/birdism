module Config
  ( AppConfig (..)
  , EBirdConf (..)
  , FlickrConf (..)
  , mkConf
  ) where

import qualified Data.Text as T


newtype EBirdConf = EBirdConf { ebcToken :: T.Text }
  deriving (Show, Eq)

data FlickrConf
  = FlickrConf
  { fcKey    :: !T.Text
  , fcSecret :: !T.Text
  } deriving (Show, Eq)

data AppConfig =
  AppConfig
  { acEBird  :: !EBirdConf
  , acFlickr :: !FlickrConf
  } deriving (Show, Eq)

mkConf :: T.Text -> T.Text -> T.Text -> AppConfig
mkConf ebToken fKey fSecret = AppConfig (EBirdConf ebToken) (FlickrConf fKey fSecret)

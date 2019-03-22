{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}


module Ebird.Region where

import           Control.Lens
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Reader   (MonadReader, asks)
import           Data.Text              (Text)
import           GHC.Generics           (Generic)

import qualified Data.Aeson             as J
import qualified Data.Aeson.Casing      as J
import qualified Data.Aeson.TH          as J
import qualified Data.Text              as T
import qualified Data.Text.Encoding     as T
import qualified Network.Wreq           as W

import           Config
import           Types


data SubRegion
  = SubRegion
  { _srCode :: !RegionCode
  , _srName :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''SubRegion)

newtype SubRegions
  = SubRegions { unSubRegions :: [SubRegion] }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

subregionListUrl :: String
subregionListUrl = "https://ebird.org/ws2.0/ref/region/list/subnational2/IN.json"

getSubRegions :: (MonadReader AppConfig m, MonadIO m) => m SubRegions
getSubRegions = do
  token <- asks $ ebcToken . acEbird
  resp  <- liftIO $ W.getWith (opts token) subregionListUrl
  liftIO $ print (resp ^. W.responseBody)
  case J.eitherDecode (resp ^. W.responseBody) of
    Left e       -> do
      liftIO $ print $ "parsing failed: " <> e
      return $ SubRegions []
    Right subregions -> return subregions
  where
    opts key = W.defaults
               & W.header "X-eBirdApiToken" .~ [ T.encodeUtf8 key ]


data ChecklistObservation
  = ChecklistObservation
  { _coSpeciesCode :: !Text
  , _coComName     :: !Text
  , _coSciName     :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''ChecklistObservation)

searchCheckListUrl :: Text -> String
searchCheckListUrl reg = "https://ebird.org/ws2.0/data/obs/"
                         <> T.unpack reg <> "/recent?back=30"
searchCheckLists
  :: (MonadReader AppConfig m, MonadIO m)
  => RegionCode -> m [ChecklistObservation]
searchCheckLists region = do
  token <- asks $ ebcToken . acEbird
  resp  <- liftIO $ W.getWith (opts token) (searchCheckListUrl region)
  liftIO $ print (resp ^. W.responseBody)
  case J.eitherDecode (resp ^. W.responseBody) of
    Left e -> do
      liftIO $ print $ "parsing failed: " <> e
      return []
    Right list -> return list
  where
    opts key = W.defaults
               & W.header "X-eBirdApiToken" .~ [ T.encodeUtf8 key ]

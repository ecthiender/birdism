-- |
{-# LANGUAGE DeriveGeneric #-}

module Service.Ebird.V2.Data.Observation
  ( searchCheckLists
  , ChecklistObservation(..)
  ) where

import qualified Data.Aeson              as J
import qualified Data.Aeson.Casing       as J
import qualified Data.Text               as T

import           Common
import           Config
import           Service.Ebird.V2.Common
import           Types

searchCheckListUrl :: Text -> String
searchCheckListUrl reg = "https://ebird.org/ws2.0/data/obs/"
                         <> T.unpack reg <> "/recent?back=30"

data ChecklistObservation
  = ChecklistObservation
  { _coSpeciesCode :: !Text
  , _coComName     :: !Text
  , _coSciName     :: !Text
  } deriving (Show, Eq, Generic)

instance J.ToJSON ChecklistObservation where
  toJSON = J.genericToJSON (J.aesonDrop 3 J.snakeCase)

instance J.FromJSON ChecklistObservation where
  parseJSON = J.genericParseJSON (J.aesonDrop 3 J.snakeCase)

searchCheckLists
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RegionCode -> m [ChecklistObservation]
searchCheckLists = ebirdApiGetService . searchCheckListUrl . uRegionCode

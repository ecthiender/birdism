-- |
module Service.Ebird.V2.Data.Observation
  ( searchCheckLists
  , ChecklistObservation(..)
  ) where

import qualified Data.Aeson              as J
import qualified Data.Aeson.Casing       as J
import qualified Data.Text               as T

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types
import           Service.Ebird.V2.Common

searchCheckListUrl :: Text -> String
searchCheckListUrl reg = "https://ebird.org/ws2.0/data/obs/"
                         <> T.unpack reg <> "/recent?back=30"

data ChecklistObservation
  = ChecklistObservation
  { _coSpeciesCode :: !SpeciesCode
  , _coComName     :: !CommonName
  , _coSciName     :: !ScientificName
  } deriving (Show, Eq, Generic)

instance J.ToJSON ChecklistObservation where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

instance J.FromJSON ChecklistObservation where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

searchCheckLists
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RegionCode -> m [ChecklistObservation]
searchCheckLists = ebirdApiGetService . searchCheckListUrl . uRegionCode

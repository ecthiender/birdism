-- |
module Service.Ebird.V2.Data.Observation
  ( searchCheckLists
  , ChecklistObservation(..)
  , recentObservationsOfSpeciesInRegion
  ) where


import           Control.Lens ((.~))
import qualified Data.Aeson              as J
import qualified Data.Aeson.Casing       as J
import qualified Data.Text               as T
import qualified Network.Wreq           as W

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
searchCheckLists = flip ebirdApiGetService Nothing . searchCheckListUrl . uRegionCode

-- Recent observations of a species in a region -
-- https://documenter.getpostman.com/view/664302/S1ENwy59?version=latest#755ce9ab-dc27-4cfc-953f-c69fb0f282d9
recentObservationsOfSpeciesInRegionUrl :: Text -> Text -> String
recentObservationsOfSpeciesInRegionUrl species region =
  "https://api.ebird.org/v2/data/obs/" <> T.unpack region <> "/recent/" <> T.unpack species

recentObservationsOfSpeciesInRegion
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => SpeciesCode -> RegionCode -> m [RecentSightingLocation]
recentObservationsOfSpeciesInRegion (SpeciesCode speciesCode) (RegionCode regionCode) = do
  let url = recentObservationsOfSpeciesInRegionUrl speciesCode regionCode
  ebirdApiGetService url (Just opts)
  where
    opts = W.defaults
             & W.param "maxResults" .~ ["30"]
             & W.param "back" .~ ["15"]

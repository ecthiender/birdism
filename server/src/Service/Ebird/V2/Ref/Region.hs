module Service.Ebird.V2.Ref.Region
  ( getCountries
  , getSubRegions
  , getSubnationa1Regions
  , getSubnationa2Regions
  , RRegion(..)
  ) where

import qualified Data.Aeson              as J
import qualified Data.Aeson.Casing       as J
import qualified Data.Aeson.TH           as J
import qualified Data.Text               as T

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types
import           Service.Ebird.V2.Common

data Country = Country
  deriving (Generic, J.FromJSON, J.ToJSON)

data Subnational1 = Subnational1
  deriving (Generic, J.FromJSON, J.ToJSON)

data Subnational2 = Subnational2
  deriving (Generic, J.FromJSON, J.ToJSON)

data RRegion a
  = RRegion
  { _rCode :: !RegionCode
  , _rName :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''RRegion)

data SubRegion
  = SubRegion
  { _srCode :: !RegionCode
  , _srName :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''SubRegion)

newtype SubRegions
  = SubRegions { unSubRegions :: [RRegion Subnational2] }
  deriving (Show, Eq, Generic)
  deriving newtype (Semigroup, Monoid , J.FromJSON, J.ToJSON)

countriesListUrl :: String
countriesListUrl = "https://ebird.org/ws2.0/ref/region/list/country/world.json"

subnational1ListUrl :: Text -> String
subnational1ListUrl country =
  "https://ebird.org/ws2.0/ref/region/list/subnational1/" <> T.unpack country <> ".json"

subnational2ListUrl :: Text -> String
subnational2ListUrl subnat1 =
  "https://ebird.org/ws2.0/ref/region/list/subnational2/" <> T.unpack subnat1 <> ".json"


getCountries
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => m [RRegion Country]
getCountries = ebirdApiGetService countriesListUrl Nothing

getSubnationa1Regions
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RRegion Country -> m [RRegion Subnational1]
getSubnationa1Regions country =
  ebirdApiGetService (subnational1ListUrl $ uRegionCode $ _rCode country) Nothing

getSubnationa2Regions
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RRegion Subnational1 -> m [RRegion Subnational2]
getSubnationa2Regions subnat1 =
  ebirdApiGetService (subnational2ListUrl $ uRegionCode $ _rCode subnat1) Nothing

subregionListUrl :: String
subregionListUrl = "https://ebird.org/ws2.0/ref/region/list/subnational2/IN.json"

getSubRegions
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadIO m
     , MonadError e m
     , AsEbirdError e
     )
  => m SubRegions
getSubRegions = do
  ebirdApiGetService subregionListUrl Nothing

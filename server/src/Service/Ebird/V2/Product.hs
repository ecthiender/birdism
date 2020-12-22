-- |
module Service.Ebird.V2.Product
  ( getSpeciesListByRegion
  ) where

import qualified Data.Text               as T

import           Common
import           Config
import           Service.Ebird.V2.Common
import           Types

speciesListByRegionUrl :: Text -> String
speciesListByRegionUrl region = "https://api.ebird.org/v2/product/spplist/" <> T.unpack region

-- | Get Species List for a Region
getSpeciesListByRegion
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RegionCode -> m [SpeciesCode]
getSpeciesListByRegion = ebirdApiGetService . speciesListByRegionUrl . uRegionCode

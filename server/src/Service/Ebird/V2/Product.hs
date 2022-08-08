-- |
module Service.Ebird.V2.Product
  ( getSpeciesListByRegion
  ) where

import qualified Birdism.Cache           as Cache
import qualified Data.Text               as T

import           Birdism.Common
import           Birdism.Config
import           Birdism.Types
import           Control.Lens            ((^.))
import           Service.Ebird.V2.Common


speciesListByRegionUrl :: Text -> String
speciesListByRegionUrl region = "https://api.ebird.org/v2/product/spplist/" <> T.unpack region

-- | Get Species List for a Region
getSpeciesListByRegion
  :: ( MonadReader r m
     , HasEBirdConf r
     , HasBirdismCache r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RegionCode -> m [SpeciesCode]
getSpeciesListByRegion region = do
  cache <- asks (^. birdismCache)
  let reg = uRegionCode region
  Cache.lookupEbirdCache reg cache >>= \case
    Just spList -> pure spList
    Nothing -> do
      spList <- ebirdApiGetService $ speciesListByRegionUrl reg
      Cache.writeEbirdCache reg spList cache
      pure spList

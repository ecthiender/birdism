{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Hashable
import           Data.Text     (Text)
import           GHC.Generics  (Generic)

import qualified Data.Aeson    as J


newtype ScientificName
  = ScientificName { uScientificName :: Text }
  deriving (Show, Eq)

newtype CommonName
  = CommonName { uCommonName :: Text }
  deriving (Show, Eq, Generic, Hashable, J.FromJSONKey, J.ToJSONKey)

newtype SpeciesCode
  = SpeciesCode { uSpeciesCode :: Text }
  deriving (Show, Eq)

newtype Family
  = Family { uFamily :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

newtype RegionCode
  = RegionCode { uRegionCode :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

-- indicating a particular species; maybe in future change the name to `Species`
data Bird
  = Bird
  { bScName        :: !ScientificName
  , bComName       :: !CommonName
  , bSpCode        :: !SpeciesCode
  , bCategory      :: !(Maybe Text)
  , bOrder         :: !(Maybe Text)
  , bFamilyComName :: !Text
  } deriving (Show, Eq)

newtype Region
  = Region { uRegion :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

data Checklist
  = Checklist
  { cRegion :: RegionCode
  , cBirds  :: [Bird]
  } deriving (Show, Eq)

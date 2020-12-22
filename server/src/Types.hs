{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Hashable
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J


newtype ScientificName
  = ScientificName { uScientificName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (J.ToJSON, J.FromJSON)

newtype CommonName
  = CommonName { uCommonName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (Hashable, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON)

newtype SpeciesCode
  = SpeciesCode { uSpeciesCode :: Text }
  deriving stock (Show, Eq)
  deriving newtype (J.ToJSON, J.FromJSON)

data Family
  = Family
  { _fScientificName :: !ScientificName
  , _fCommonName     :: !CommonName
  } deriving (Show, Eq, Generic)

instance J.ToJSON Family where
  toJSON = J.genericToJSON (J.aesonDrop 2 J.snakeCase)

instance J.FromJSON Family where
  parseJSON = J.genericParseJSON (J.aesonDrop 2 J.snakeCase)

newtype RegionCode
  = RegionCode { uRegionCode :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (J.FromJSON, J.ToJSON) --, PG.FromRow)

newtype RegionName
  = RegionName { unRegionName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (J.FromJSON, J.ToJSON)

-- | A particular species; maybe in future change the name to `Species`
data Bird
  = Bird
  { bScName        :: !ScientificName
  , bComName       :: !CommonName
  , bSpCode        :: !SpeciesCode
  , bCategory      :: !(Maybe Text)
  , bOrder         :: !(Maybe Text)
  , bFamilyComName :: !Text
  } deriving (Show, Eq)

data Region
  = Region
  { _rRegionCode :: !RegionCode
  , _rRegionName :: !RegionName
  } deriving (Show, Eq, Generic)

instance J.ToJSON Region where
  toJSON = J.genericToJSON (J.aesonDrop 2 J.snakeCase)

instance J.FromJSON Region where
  parseJSON = J.genericParseJSON (J.aesonDrop 2 J.snakeCase)

data Checklist
  = Checklist
  { cRegion :: RegionCode
  , cBirds  :: [Bird]
  } deriving (Show, Eq)

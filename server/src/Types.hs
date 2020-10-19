{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}

module Types where

import           Data.Hashable
import           Data.Text         (Text)
import           GHC.Generics      (Generic)

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J


newtype ScientificName
  = ScientificName { uScientificName :: Text }
  deriving (Show, Eq, Generic, J.ToJSON, J.FromJSON)

newtype CommonName
  = CommonName { uCommonName :: Text }
  deriving (Show, Eq, Generic, Hashable, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON)

newtype SpeciesCode
  = SpeciesCode { uSpeciesCode :: Text }
  deriving (Show, Eq)

data Family
  = Family
  { _fScientificName :: !ScientificName
  , _fCommonName     :: !CommonName
  } deriving (Show, Eq, Generic)
$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''Family)

newtype RegionCode
  = RegionCode { uRegionCode :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON) --, PG.FromRow)

newtype RegionName
  = RegionName { unRegionName :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

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
  } deriving (Show, Eq) --, Generic, J.FromJSON, J.ToJSON)
$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''Region)

data Checklist
  = Checklist
  { cRegion :: RegionCode
  , cBirds  :: [Bird]
  } deriving (Show, Eq)

{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module Birdism.Types where

import           Data.Hashable
import           Data.Text                            (Text)
import           GHC.Generics                         (Generic)

import qualified Data.Aeson                           as J
import qualified Data.Aeson.Casing                    as J
import qualified Database.PostgreSQL.Simple.FromField as PG
import qualified Database.PostgreSQL.Simple.ToField   as PG


newtype ScientificName
  = ScientificName { uScientificName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (J.ToJSON, J.FromJSON, PG.ToField, PG.FromField)

newtype CommonName
  = CommonName { uCommonName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype ( Hashable, J.FromJSONKey, J.ToJSONKey, J.FromJSON, J.ToJSON, PG.ToField, PG.FromField)

newtype SpeciesCode
  = SpeciesCode { uSpeciesCode :: Text }
  deriving stock (Show, Eq)
  deriving newtype (J.ToJSON, J.FromJSON, PG.ToField, PG.FromField)

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
  deriving newtype (J.FromJSON, J.ToJSON, PG.FromField, PG.ToField)

newtype RegionName
  = RegionName { unRegionName :: Text }
  deriving stock (Show, Eq, Generic)
  deriving newtype (J.FromJSON, J.ToJSON, PG.FromField, PG.ToField)

-- | A particular species; maybe in future change the name to `Species`
data Bird
  = Bird
  { bScientificName   :: !ScientificName
  , bCommonName       :: !CommonName
  , bSpeciesCode      :: !SpeciesCode
  , bCategory         :: !(Maybe Text)
  , bOrder            :: !(Maybe Text)
  , bFamilyCommonName :: !CommonName
  } deriving (Show, Eq, Generic)

instance J.ToJSON Bird where
  toJSON = J.genericToJSON (J.aesonDrop 1 J.snakeCase)

instance J.FromJSON Bird where
  parseJSON = J.genericParseJSON (J.aesonDrop 1 J.snakeCase)

makeBird :: SpeciesCode -> CommonName -> ScientificName -> Family -> Bird
makeBird spCode comName sciName family =
  Bird sciName comName spCode Nothing Nothing (_fCommonName family)


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


type FamilyNames = [Family]
type RegionNames = [Region]

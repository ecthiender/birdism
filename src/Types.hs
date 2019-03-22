{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import           Data.Text    (Text)
import           GHC.Generics (Generic)

import qualified Data.Aeson   as J


type ScientificName = Text
type CommonName     = Text
type SpeciesCode    = Text

type Family         = Text
type RegionCode     = Text

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
  = Region { getRegion :: Text }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

data Checklist
  = Checklist
  { cRegion :: RegionCode
  , cBirds  :: [Bird]
  } deriving (Show, Eq)

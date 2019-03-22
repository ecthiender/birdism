{-# LANGUAGE TemplateHaskell #-}

module Api where

import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J

import           Lib


data SearchRequest
  = SearchRequest
  { _srqRegion :: !Region
  , _srqFamily :: !Family
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''SearchRequest)

newtype SearchResponse
  = SearchResponse { _srpResult :: Corpus }
  deriving (Show, Eq)
$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''SearchResponse)

processSearch :: SearchRequest -> App SearchResponse
processSearch (SearchRequest region family) =
  SearchResponse <$> getCorpus region family

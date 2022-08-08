-- | A common module to hold all Flickr related common types. Required by
-- multiple modules

module Service.Flickr.Context where

import           Birdism.Common    (Generic, Text)
import           Control.Lens      (makeClassy, makeClassyPrisms)

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J


data FlickrConf
  = FlickrConf
  { _fcKey    :: !Text
  , _fcSecret :: !Text
  } deriving (Show, Generic)

instance J.ToJSON FlickrConf where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

instance J.FromJSON FlickrConf where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

makeClassy ''FlickrConf

data FlickrError
  = FlickrErrorSearch !Text
  | FlickrErrorParseResponse !Text
  | FlickrErrorUnexpected !Text
  deriving (Show)

makeClassyPrisms ''FlickrError

$(J.deriveToJSON
  J.defaultOptions { J.constructorTagModifier = J.snakeCase
                   , J.sumEncoding = J.TaggedObject "code" "error"
                   } ''FlickrError)

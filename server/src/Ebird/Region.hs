{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleContexts  #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}


module Ebird.Region where

import           Common
import           Control.Lens

import qualified Data.Aeson         as J
import qualified Data.Aeson.Casing  as J
import qualified Data.Aeson.TH      as J
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Network.Wreq       as W

import           Config
import           Types


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

$(J.deriveJSON (J.aesonDrop 2 J.snakeCase) ''RRegion)

data SubRegion
  = SubRegion
  { _srCode :: !RegionCode
  , _srName :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.snakeCase) ''SubRegion)

newtype SubRegions
  = SubRegions { unSubRegions :: [RRegion Subnational2] }
  deriving (Show, Eq, Generic, J.FromJSON, J.ToJSON)

instance Semigroup SubRegions where
  (SubRegions x) <> (SubRegions y) = SubRegions $ x <> y

instance Monoid SubRegions where
  mempty = SubRegions []

data Countries

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
getCountries = ebirdApiGetService countriesListUrl

getSubnationa1Regions
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RRegion Country -> m [RRegion Subnational1]
getSubnationa1Regions country =
  ebirdApiGetService (subnational1ListUrl $ uRegionCode $ _rCode country)

getSubnationa2Regions
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RRegion Subnational1 -> m [RRegion Subnational2]
getSubnationa2Regions subnat1 =
  ebirdApiGetService (subnational2ListUrl $ uRegionCode $ _rCode subnat1)

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
  ebirdApiGetService subregionListUrl

ebirdApiGetService
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     , J.FromJSON a
     )
  => String
  -- ^ the complete URL
  -> m a
ebirdApiGetService url = do
  r <- ask
  let token = r ^. ebcToken
  resp  <- liftIO $ W.getWith (opts token) url
  case J.eitherDecode (resp ^. W.responseBody) of
    Left e       -> do
      throwError $ (_EbirdErrorParseResponse #) (T.pack e)
    Right res -> return res
  where
    opts key = W.defaults
               & W.header "X-eBirdApiToken" .~ [ T.encodeUtf8 key ]


data ChecklistObservation
  = ChecklistObservation
  { _coSpeciesCode :: !Text
  , _coComName     :: !Text
  , _coSciName     :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 3 J.camelCase) ''ChecklistObservation)

searchCheckListUrl :: Text -> String
searchCheckListUrl reg = "https://ebird.org/ws2.0/data/obs/"
                         <> T.unpack reg <> "/recent?back=30"
searchCheckLists
  :: ( MonadReader r m
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     , MonadIO m
     )
  => RegionCode -> m [ChecklistObservation]
searchCheckLists (RegionCode region) = do
  ebirdApiGetService (searchCheckListUrl region)
  -- token <- asks $ _ebcToken . _axEbirdConf
  -- resp  <- liftIO $ W.getWith (opts token) (searchCheckListUrl region)
  -- case J.eitherDecode (resp ^. W.responseBody) of
  --   Left e -> do
  --     liftIO $ print $ "parsing failed: " <> e
  --     return []
  --   Right list -> return list
  -- where
  --   opts key = W.defaults
  --              & W.header "X-eBirdApiToken" .~ [ T.encodeUtf8 key ]

module Birdism.Api where

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Text         as T

import           Birdism.Common
import           Birdism.Config
import           Birdism.Lib
import           Birdism.Types
import           Control.Lens
import           GHC.TypeLits      (Nat)
import           Servant


type BirdismHttpAPIF f
  =    GetRedirect 302 RedirectLocation
  :<|> "api" :> "ping" :> Get '[PlainText] Pong
  -- the v1 API
  :<|> "api" :> "v1" :> "regions" :> Get '[JSON] RegionNames

  :<|> "api" :> "v1" :> "families" :> Get '[JSON] FamilyNames
  :<|> "api" :> "v1" :> "family" :> "scientific-name"
        :> ReqBody '[JSON] FamilyScientificNameRequest
        :> Post '[JSON] (f FamilyScientificNameResponse)

  :<|> "api" :> "v1" :> "search" :> ReqBody '[JSON] SearchRequest :> Post '[JSON] (f SearchResult)
  :<|> "api" :> "v1" :> "search" :> "species" :> ReqBody '[JSON] SearchRequest :> Post '[JSON] (f [Bird])
  :<|> "api" :> "v1" :> "search" :> "images" :> ReqBody '[JSON] [CommonName] :> Post '[JSON] (f SearchResult)


type BirdismHttpAPI = BirdismHttpAPIF ApiResponse

-- | The HTTP server implementing the above API
birdismApiServer
  :: (MonadError AppError m, MonadReader AppCtx m, MonadIO m)
  => ServerT BirdismHttpAPI m
birdismApiServer
     = redirect (RedirectLocation "/index.html")
  :<|> pingApiHandler
  :<|> getRegions
  :<|> getFamilies
  :<|> withApiResponse getFamilyScientificName
  :<|> withApiResponse processSearch
  :<|> withApiResponse processSpeciesSearch
  :<|> withApiResponse processImageSearch
  where
    withApiResponse handler = fmap ApiResponse . handler

serverProxy :: Proxy BirdismHttpAPI
serverProxy = Proxy

data Pong = Pong

instance Show Pong where
  show _ = "pong"

instance J.ToJSON Pong where
  toJSON _ = "pong"

instance MimeRender PlainText Pong where
  mimeRender _ _ = "pong"

pingApiHandler :: Monad m => m Pong
pingApiHandler = pure Pong

newtype ApiResponse a
  = ApiResponse { _arpResult :: a }
  deriving (Show, Eq, Generic)

instance J.ToJSON a => J.ToJSON (ApiResponse a) where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

data SearchRequest
  = SearchRequest
  { _srqRegion :: !RegionCode
  , _srqFamily :: !ScientificName
  -- ^ 'ScientificName' of the family
  } deriving (Show, Eq, Generic)

instance J.FromJSON SearchRequest where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

instance J.ToJSON SearchRequest where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

processSearch
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasAllRegions r
     , HasAllFamilies r
     , HasBirdismCache r
     , HasFlickrConf r
     , HasEBirdConf r
     , MonadError e m
     , AsEbirdError e
     )
  => SearchRequest -> m SearchResult
processSearch (SearchRequest regionCode familySciName) = do
  region <- validateRegion regionCode
  family <- validateFamily familySciName
  getCorpus region family

processSpeciesSearch
  :: ( MonadIO m
     , MonadReader r m
     , HasDbConfig r
     , HasAllRegions r
     , HasAllFamilies r
     , HasEBirdConf r
     , HasBirdismCache r
     , MonadError e m
     , AsEbirdError e
     )
  => SearchRequest -> m [Bird]
processSpeciesSearch (SearchRequest regionCode familySciName) = do
  region <- validateRegion regionCode
  family <- validateFamily familySciName
  getSpeciesByRegionFamily region family

processImageSearch
  :: ( MonadReader r m
     , HasFlickrConf r
     , HasBirdismCache r
     , MonadIO m
     )
  => [CommonName] -> m SearchResult
processImageSearch = getImagesBySpecies

newtype FamilyScientificNameRequest
  = FamilyScientificNameRequest { _gfsnrName :: CommonName }
  deriving (Show, Eq, Generic)

instance J.FromJSON FamilyScientificNameRequest where
  parseJSON = J.genericParseJSON (J.aesonPrefix J.snakeCase)

instance J.ToJSON FamilyScientificNameRequest where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

newtype FamilyScientificNameResponse
  = FamilyScientificNameResponse { _gfsnrFamilies :: [Family] }
  deriving (Show, Eq, Generic)

instance J.ToJSON FamilyScientificNameResponse where
  toJSON = J.genericToJSON (J.aesonPrefix J.snakeCase)

getFamilyScientificName
  :: ( MonadIO m
     , MonadReader r m
     , HasAllFamilies r
     )
  => FamilyScientificNameRequest -> m FamilyScientificNameResponse
getFamilyScientificName (FamilyScientificNameRequest commonName) = do
  families <- asks (^. allFamilies)
  let isSubStr v1 v2 = T.isInfixOf (T.toLower $ uCommonName v1) (T.toLower $ uCommonName v2)
      found = filter (isSubStr commonName . _fCommonName) (unAllFamilies families)
  pure $ FamilyScientificNameResponse found

validateRegion
  :: (MonadReader r m, HasAllRegions r, MonadError e m, AsEbirdError e)
  => RegionCode -> m Region
validateRegion region = do
  regions <- unAllRegions <$> asks (^. allRegions)
  let findRegion x = region == _rRegionCode x
  validate findRegion regions (_EbirdErrorSearch # "Invalid region")

validateFamily
  :: (MonadReader r m, HasAllFamilies r, MonadError e m, AsEbirdError e)
  => ScientificName -> m Family
validateFamily family = do
  families <- unAllFamilies <$> asks (^. allFamilies)
  let findScName x = family == _fScientificName x
  validate findScName families (_EbirdErrorSearch # "Invalid family")

-- | Function that generalizes some validation and parsing of string-ly values
-- into proper domain-types. E.g. - a string based region code can be converted
-- to a 'Region' type etc.
validate :: (MonadError e m) => (a -> Bool) -> [a] -> e -> m a
validate check cache e = maybe (throwError e) pure $ find check cache

getFamilies
  :: ( MonadIO m
     , MonadReader r m
     , HasAllFamilies r
     )
  => m FamilyNames
getFamilies =
  unAllFamilies <$> asks (^. allFamilies)

getRegions
  :: ( MonadIO m
     , MonadReader r m
     , HasAllRegions r
     )
  => m RegionNames
getRegions = do
  unAllRegions <$> asks (^. allRegions)

-- * Internal Servant extensions

type GetRedirect (code :: Nat) loc
  = Verb 'GET code '[PlainText, JSON] (Headers '[Header "Location" loc] NoContent)

redirect
  :: (Monad m, ToHttpApiData loc)
  => loc -- ^ what to put in the 'Location' header
  -> m (Headers '[Header "Location" loc] NoContent)
redirect a = return (addHeader a NoContent)

newtype RedirectLocation = RedirectLocation Text
  deriving stock (Show)
  deriving newtype (ToHttpApiData)

-- | A service to search media in Macualay Library
-- https://www.macaulaylibrary.org

module Service.MacaulayLibrary.V1.Search where

import           Control.Lens      ((.~), (^.))

import qualified Data.Aeson        as J
import qualified Data.Aeson.Casing as J
import qualified Data.Aeson.TH     as J
import qualified Network.Wreq      as W

import           Birdism.Common
import           Birdism.Types

searchMedia :: SpeciesCode -> IO [Text]
searchMedia spCode = do
  -- the API to be called:
  -- 'https://search.macaulaylibrary.org/catalog.json?searchField=species&q=Asian+Barred+Owlet&taxonCode=asbowl1'
  resp <- liftIO $ W.getWith opts "https://search.macaulaylibrary.org/catalog.json"
  let res = resp ^. W.responseBody
  -- FIXME: check headers and handle error cases
  case J.eitherDecode res of
    Right searchRes -> do
      let mediaItems = _srContent $ _srResults searchRes
      -- FIXME: push the limit to the API request
      return $ take 3 $ map _miPreviewUrl mediaItems
    Left e       -> do
      -- FIXME: proper error handling and logging here
      liftIO $ print $ "[Service::MacualayLibrary.searchMedia] parsing failed: " <> e
      return []
  where
    -- TODO: sort by rating/best photo?
    opts = W.defaults
         & W.param "taxonCode" .~ [uSpeciesCode spCode]
         & W.param "searchField" .~ ["species"]
         & W.param "_mediaType" .~ ["on"]
         & W.param "mediaType" .~ ["photo"]

data SearchResponse
  = SearchResponse
  { _srResults   :: SearchResult
  , _srUserId    :: Maybe Text
  , _srSourceUrl :: Maybe Text
  } deriving stock (Show)


data SearchResult
  = SearchResult
  { _srCount            :: Maybe Int
  , _srNextCursorMark   :: Maybe Text
  , _srContent          :: [MediaItem]
  , _srSearchTimeMillis :: Maybe Int
  , _srFacetInfo        :: Maybe FacetInfo
  } deriving stock (Show)


-- | A media item response from Macaulay Library is kind of overloaded. But good
-- that its a flat structure of various information.
data MediaItem
  = MediaItem
  -- Metadata about the media item
  { _miCatalogId         :: Maybe Text
  , _miAssetId           :: Maybe Text
  , _miLicenseType       :: Maybe Text
  , _miRating            :: Maybe Text
  , _miSource            :: Maybe Text

  -- location info about the obs
  , _miLocation          :: Maybe Text
  , _miLocationLine1     :: Maybe Text
  , _miLocationLine2     :: Maybe Text
  , _miLongitude         :: Maybe Double
  , _miLatitude          :: Maybe Double

  -- the actual media URLs and media related metadata
  , _miMediaType         :: Text
  , _miPreviewUrl        :: Text
  , _miLargeUrl          :: Text
  , _miMediaUrl          :: Text
  , _miWidth             :: Maybe Int
  , _miHeight            :: Maybe Int
  -- , _miExifData          :: Maybe ExifData

  -- data about the species
  , _miAge               :: Maybe Text
  , _miSex               :: Maybe Text
  , _miBehaviours        :: Maybe Text
  , _miSpeciesCode       :: Maybe Text
  , _miCommonName        :: Maybe Text
  , _miSciName           :: Maybe Text
  , _miEbirdSpeciesUrl   :: Maybe Text
  , _miEBirdChecklistId  :: Maybe Text
  , _miEBirdChecklistUrl :: Maybe Text
  , _miSpecimenUrl       :: Maybe Text
  , _miObsDttm           :: Maybe Text

  -- info about the author of the media
  , _miUserId            :: Maybe Text
  , _miUserDisplayName   :: Maybe Text
  , _miUserProfileUrl    :: Maybe Text
  } deriving stock (Show)

data ExifData = ExifData
  deriving stock (Show)

newtype FacetInfo = FacetInfo { _fiAssetFormatCode :: AssetFormatCode }
  deriving stock (Show)

data AssetFormatCode
  = AssetFormatCode
  { _afcPhoto :: Text
  , _afcVideo :: Text
  , _afcAudio :: Text
  } deriving stock (Show)


$(J.deriveJSON (J.aesonPrefix J.camelCase) 'MediaItem)
$(J.deriveJSON (J.aesonPrefix J.camelCase) 'SearchResult)
$(J.deriveJSON (J.aesonPrefix J.camelCase) 'ExifData)
$(J.deriveJSON (J.aesonPrefix J.camelCase) 'FacetInfo)
$(J.deriveJSON (J.aesonPrefix J.camelCase) 'AssetFormatCode)
$(J.deriveJSON (J.aesonPrefix J.camelCase) 'SearchResponse)

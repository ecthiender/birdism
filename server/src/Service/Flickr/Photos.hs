module Service.Flickr.Photos where

import           Birdism.Common
import           Control.Lens
import           Service.Flickr.Context

import qualified Data.Aeson             as J
import qualified Data.Aeson.Casing      as J
import qualified Data.Aeson.TH          as J
import qualified Data.Text              as T
import qualified HTTP
import qualified Network.Wreq           as W


data FlickrPhotoResponse
  = FlickrPhotoResponse
  { _fprUrlM   :: !Text
  , _fprWidthM :: !Int
  , _fprOwner  :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonPrefix J.snakeCase) ''FlickrPhotoResponse)

newtype FlickrResponse
  = FlickrResponse { unFlickrResponse :: [FlickrPhotoResponse] }
  deriving stock (Show, Eq)

instance J.FromJSON FlickrResponse where
  parseJSON (J.Object r) = do
    a <- r J..: "photos"
    ph <- a J..: "photo"
    return $ FlickrResponse ph
  parseJSON _ = fail "unexpected JSON response from Flickr"

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

-- | Main API to search photos in Flickr
searchPhotos
  :: ( MonadError e m
     , AsFlickrError e
     , MonadReader r m
     , HasFlickrContext r
     , MonadIO m
     )
  => Text -> m [Text]
searchPhotos term = do
  r <- ask
  let apiKey = r ^. fcxConf . fcKey
      cache = r ^. fcxCache

  -- liftIO $ putStrLn "Flickr Photo Search :: Looking up flickr cache"
  lookupFlickrCache term cache >>= \case
    Just urls -> pure urls
    Nothing -> do
      -- liftIO (putStrLn "Flickr Photo Search :: cache miss")
      -- the API to be called:
      -- "https://api.flickr.com/services/rest/?method=flickr.photos.search&format=json&api_key=<apiKey>&text=<name>"
      resp <- runExceptT $ HTTP.httpGetJSON "https://api.flickr.com/services/rest/" (Just $ opts apiKey) []
      case resp of
        Right photos -> do
          let urls = map _fprUrlM $ unFlickrResponse photos
          writeFlickrCache term urls cache
          return urls
        Left e       -> do
          -- TODO: add proper logging
          liftIO $ print $ "HTTP request error: " <> show e
          throwError $ httpErrToFlickrError e
  where
    opts key = W.defaults
               & W.param "method" .~ ["flickr.photos.search"]
               & W.param "format" .~ ["json"]
               & W.param "nojsoncallback" .~ ["?"]
               & W.param "api_key" .~ [key]
               & W.param "text" .~ [term]
               & W.param "tags" .~ ["bird,wildlife"]
               & W.param "sort" .~ ["relevance"]
               & W.param "per_page" .~ ["3"]
               -- & W.param "extras" .~ ["url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o"]
               & W.param "extras" .~ ["url_m"]

httpErrToFlickrError :: (AsFlickrError e) => HTTP.HttpRequestError -> e
httpErrToFlickrError = \case
  HTTP.HREConnectionError e       -> _FlickrErrorUnexpected # T.pack (show e)
  HTTP.HREUnexpectedRedirect _ r  -> _FlickrErrorUnexpected # r
  HTTP.HREClientError _ e         -> _FlickrErrorSearch # e
  HTTP.HREServerError _ e         -> _FlickrErrorSearch # e
  HTTP.HREParseResponseFailed r e -> _FlickrErrorParseResponse # (T.pack e <> " : " <> r)

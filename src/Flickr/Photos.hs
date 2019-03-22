{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Flickr.Photos where

import           Control.Lens
import           Control.Monad.IO.Class (liftIO)
import           Data.Text              (Text)

import qualified Data.Aeson             as J
import qualified Data.Aeson.Casing      as J
import qualified Data.Aeson.TH          as J
import qualified Network.Wreq           as W

data FlickrPhotoResponse
  = FlickrPhotoResponse
  { _fprUrlM   :: !Text
  , _fprWidthM :: !Text
  , _fprOwner  :: !Text
  } deriving (Show, Eq)

$(J.deriveJSON (J.aesonDrop 4 J.snakeCase) ''FlickrPhotoResponse)

newtype FlickrResponse
  = FlickrResponse { unFlickrResponse :: [FlickrPhotoResponse] }
  deriving (Show, Eq)

instance J.FromJSON FlickrResponse where
  parseJSON (J.Object r) = do
    a <- r J..: "photos"
    ph <- a J..: "photo"
    return $ FlickrResponse ph
  parseJSON _            = fail "unexpected JSON response from Flickr"


searchPhotos :: Text -> Text -> IO [Text]
searchPhotos apiKey term = do
  --liftIO $ print $ "https://api.flickr.com/services/rest/" <> "?method=flickr.photos.search&format=json&api_key=" <> apiKey <> "&text=" <> cn
  resp <- liftIO $ W.getWith (opts apiKey) "https://api.flickr.com/services/rest/"
  let res = resp ^. W.responseBody
  --liftIO $ print $ resp ^. W.responseBody
  --liftIO $ writeFile "request.log" (BL.unpack res)
  let photos = J.eitherDecode res
  case photos of
    Left e       -> do
      liftIO $ print $ "parsing failed: " <> e
      return []
    Right photos -> return $ map _fprUrlM $ unFlickrResponse photos
  where
    opts key = W.defaults
               & W.param "method" .~ ["flickr.photos.search"]
               & W.param "format" .~ ["json"]
               & W.param "nojsoncallback" .~ ["?"]
               & W.param "api_key" .~ [key]
               & W.param "text" .~ [term]
               & W.param "per_page" .~ ["5"]
               & W.param "extras" .~ ["url_sq,url_t,url_s,url_q,url_m,url_n,url_z,url_c,url_l,url_o"]

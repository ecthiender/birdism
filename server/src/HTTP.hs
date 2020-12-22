{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
module HTTP where

import           Birdism.Common
import           Control.Exception    (try)
import           Control.Lens

import qualified Data.Aeson           as J
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text.Encoding   as T
import qualified Network.HTTP.Client  as HTTP
import qualified Network.HTTP.Types   as HTTP
import qualified Network.Wreq         as W

data HttpRequestError
  = HREConnectionError !HTTP.HttpException
  -- ^ HTTP exception
  | HREUnexpectedRedirect !Int !Text
  -- ^ The HTTP status and the response body; when 3xx
  | HREClientError !Int !Text
  -- ^ The HTTP status and the response body; when 4xx
  | HREServerError !Int !Text
  -- ^ The HTTP status and the response body; when 5xx
  | HREParseResponseFailed !Text !String
  -- ^ The original response and the aeson error message.
  deriving (Show)

httpGetJSON
  :: ( MonadError HttpRequestError m
     , MonadIO m
     , J.FromJSON a
     )
  => String
  -- ^ the complete URL
  -> [HTTP.Header]
  -- ^ optional request headers
  -> m a
  -- ^ return parsed type from expected JSON response
httpGetJSON url headers = do
  httpRes <- liftIO (try $ W.getWith opts url)

  -- throw error if any connection error occurs
  resp <- either (throwError . HREConnectionError) return httpRes

  -- extract relevant info
  let status = resp ^. W.responseStatus
      respBody = resp ^. W.responseBody
      respBodyTxt = T.decodeUtf8 $ BL.toStrict respBody
      statusCode = status ^. W.statusCode
  -- throw errors if not 200
  when (statusCode >= 300 && statusCode <= 400) $
    throwError $ HREUnexpectedRedirect statusCode respBodyTxt
  when (statusCode >= 400 && statusCode <= 500) $
    throwError $ HREClientError statusCode respBodyTxt
  when (statusCode >= 500) $
    throwError $ HREServerError statusCode respBodyTxt

  -- throw error, if response parsing fails
  case J.eitherDecode respBody of
    Left e    -> throwError $ HREParseResponseFailed respBodyTxt e
    Right res -> return res
  where
    opts = W.defaults
           & W.header "User-Agent" .~ ["birdism/v0.1"]
           & W.headers .~ headers
           & W.checkResponse ?~ (\_ _ -> return ())
           -- & W.manager .~ Right manager


httpGet
  :: ( MonadError HttpRequestError m
     , MonadIO m
     )
  => String
  -- ^ the complete URL
  -> [HTTP.Header]
  -- ^ optional request headers
  -> m BL.ByteString
  -- ^ return the raw response as 'BL.ByteString'
httpGet url headers = do
  httpRes <- liftIO (try $ W.getWith opts url)

  -- throw error if any connection error occurs
  resp <- either (throwError . HREConnectionError) return httpRes

  -- extract relevant info
  let status = resp ^. W.responseStatus
      respBody = resp ^. W.responseBody
      respBodyTxt = T.decodeUtf8 $ BL.toStrict respBody
      statusCode = status ^. W.statusCode
  -- throw errors if not 200
  when (statusCode >= 300 && statusCode <= 400) $
    throwError $ HREUnexpectedRedirect statusCode respBodyTxt
  when (statusCode >= 400 && statusCode <= 500) $
    throwError $ HREClientError statusCode respBodyTxt
  when (statusCode >= 500) $
    throwError $ HREServerError statusCode respBodyTxt

  return respBody
  where
    opts = W.defaults
           & W.header "User-Agent" .~ ["birdism/v0.1"]
           & W.headers .~ headers
           & W.checkResponse ?~ (\_ _ -> return ())
           -- & W.manager .~ Right manager

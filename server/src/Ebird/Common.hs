{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
module Ebird.Common where

import           Common
import           Control.Lens

import qualified Data.Aeson         as J
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T

import           Config
import           HTTP

-- | Helper function to call the EBird API
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
  -- ^ return parsed type from expected JSON response
ebirdApiGetService url = do
  r <- ask
  let token = r ^. ebcToken
  resp <- runExceptT $ httpGetJSON url [("X-eBirdApiToken", T.encodeUtf8 token)]
  case resp of
    Left e    -> throwError $ httpErrToEbirdError e
    Right res -> return res
  where
    httpErrToEbirdError = \case
      HREConnectionError e -> (_EbirdErrorUnexpected #) (T.pack $ show e)
      HREUnexpectedRedirect _ r -> (_EbirdErrorUnexpected #) r
      HREClientError _ e -> (_EbirdErrorUnexpected #) e
      HREServerError _ e -> (_EbirdErrorUnexpected #) e
      HREParseResponseFailed r e -> (_EbirdErrorParseResponse #) (T.pack e <> " : " <> r)

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static
import           Web.Spock.Core

import qualified Data.Aeson                           as J
import qualified Network.HTTP.Types.Status            as HTTP
import qualified Network.Wai                          as Wai

import           Api
import           Config
import           Lib                                  (App)


httpApp :: AppConfig -> IO Wai.Middleware
httpApp config = spockT id $ do
  middleware $ logStdoutDev
  middleware $ staticPolicy (addBase "../app/")

  -- simple ping API for status check
  get "api/ping" $ text "pong"

  -- API which returns all bird families of the world (according to ebird taxonomy)
  get "api/v1/families" $ httpGetHandler config getFamilies

  -- API which handles the family/region search
  post "api/v1/search" $ httpPostHandler config processSearch


httpGetHandler :: J.ToJSON a => AppConfig -> App a -> ActionCtxT () IO ()
httpGetHandler config service = do
  handleResult =<< liftIO (runExceptT $ runReaderT service config)

httpPostHandler
  :: (J.FromJSON a, J.ToJSON b)
  => AppConfig -> (a -> App b) -> ActionCtxT () IO ()
httpPostHandler config service = do
  req <- jsonBody'
  handleResult =<< liftIO (runExceptT $ runReaderT (service req) config)

handleResult :: (J.ToJSON e, J.ToJSON a) => Either e a -> ActionCtxT ctx IO b
handleResult = \case
  Left e -> do
    setStatus HTTP.status400
    setJsonHeader
    lazyBytes $ J.encode e
  Right r -> do
    setJsonHeader
    lazyBytes $ J.encode r
  where
    jsonHeader = ("Content-Type", "application/json")
    setJsonHeader = uncurry setHeader jsonHeader

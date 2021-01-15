{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Birdism.Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.Wai.Middleware.RequestLogger
import           Network.Wai.Middleware.Static

import qualified Data.Aeson                           as J
import qualified Network.HTTP.Types.Status            as HTTP
import qualified Network.Wai                          as Wai
import qualified Web.Spock.Core                       as Spock

import           Birdism.Api
import           Birdism.Config

httpApp :: MonadIO m => AppCtx -> m Wai.Middleware
httpApp config = liftIO $ Spock.spockT id $ do
  Spock.middleware $ logStdoutDev
  Spock.middleware $ staticPolicy (addBase "../app/")

  -- redirect / to /index.html
  Spock.get Spock.root $ Spock.redirect "index.html"

  -- simple ping API for status check
  Spock.get "api/ping" $ Spock.text "pong"

  -- API which returns all bird families of the world (according to ebird taxonomy)
  Spock.get "api/v1/families" $ httpGetHandler config getFamilies

  -- API which returns all available regions
  Spock.get "api/v1/regions" $ httpGetHandler config getRegions

  -- API which handles the family/region search
  Spock.post "api/v1/search" $ httpPostHandler config processSearch

  Spock.post "api/v1/search/species" $ httpPostHandler config processSpeciesSearch
  Spock.post "api/v1/search/images" $ httpPostHandler config processImageSearch

  Spock.post "api/v1/family/scientific-name" $ httpPostHandler config getFamilyScientificName


newtype AppM a
  = AppM { unAppM :: ReaderT AppCtx (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadError AppError, MonadIO)

runAppM :: AppM a -> AppCtx -> IO (Either AppError a)
runAppM app = runExceptT . runReaderT (unAppM app)

httpGetHandler :: J.ToJSON a => AppCtx -> AppM a -> Spock.ActionCtxT () IO ()
httpGetHandler config (AppM service) = do
  handleResult =<< liftIO (runExceptT $ runReaderT service config)

httpPostHandler
  :: (J.FromJSON a, J.ToJSON b)
  => AppCtx -> (a -> AppM b) -> Spock.ActionCtxT () IO ()
httpPostHandler config service = do
  req <- Spock.jsonBody'
  handleResult =<< liftIO (runExceptT $ runReaderT (unAppM $ service req) config)

handleResult :: (J.ToJSON e, J.ToJSON a) => Either e a -> Spock.ActionCtxT ctx IO b
handleResult = \case
  Left e -> do
    Spock.setStatus HTTP.status400
    setJsonHeader
    Spock.lazyBytes $ J.encode e
  Right r -> do
    setJsonHeader
    Spock.lazyBytes $ J.encode r
  where
    jsonHeader = ("Content-Type", "application/json")
    setJsonHeader = uncurry Spock.setHeader jsonHeader

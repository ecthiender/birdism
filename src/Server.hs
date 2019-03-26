{-# LANGUAGE OverloadedStrings #-}

module Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Text                     (Text)
import           Web.Spock.Core

import qualified Data.Aeson                    as J
import qualified Network.HTTP.Types.Status     as HTTP
import qualified Network.Wai                   as Wai

import           Network.Wai.Middleware.Static

import           Api
import           Config


jsonHeader :: (Text, Text)
jsonHeader = ("Content-Type", "application/json")

httpApp :: AppConfig -> IO Wai.Middleware
httpApp config = spockT id $ do
  middleware $  staticPolicy (addBase "../app/")
  get "api/ping" $ text "pong"

  post "api/v1/search" $ do
    req <- jsonBody'
    res <- liftIO $ runExceptT $ runReaderT (processSearch req) config
    case res of
      Left e -> do
        setStatus HTTP.status400
        setJsonHeader
        lazyBytes $ J.encode e
      Right r -> do
        setJsonHeader
        lazyBytes $ J.encode r

  where
    setJsonHeader = uncurry setHeader jsonHeader

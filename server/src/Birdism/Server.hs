{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module Birdism.Server where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)

import qualified Data.Aeson                           as J
import qualified Network.Wai                          as Wai

import           Birdism.Api
import           Birdism.Config

import           Servant

newtype AppM a
  = AppM { unAppM :: ReaderT AppCtx (ExceptT AppError IO) a }
  deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadError AppError, MonadIO)

runAppM :: AppM a -> AppCtx -> IO (Either AppError a)
runAppM app = runExceptT . runReaderT (unAppM app)

httpApp :: AppCtx -> Wai.Application
httpApp ctx =
  logStdoutDev
  $ staticPolicy (addBase "../app/")
  $ serve serverProxy $ hoistServer serverProxy (appMToServantHandler ctx) birdismApiServer

appMToServantHandler :: AppCtx -> AppM a -> Handler a
appMToServantHandler ctx service =
  liftIO (runAppM service ctx) >>= either (throwError . toServerError) return
  where
    toServerError e =
      let resp = toHttpResp e
      in resp { errBody = J.encode e, errHeaders = [jsonHeader] }
    toHttpResp = \case
      AEEbirdError ebErr -> case ebErr of
        EbirdErrorSearch _        -> err400
        EbirdErrorParseResponse _ -> err400
        EbirdErrorUnexpected _    -> err500
      AEDbError _     -> err500
      AEConfigError _ -> err400
    jsonHeader = ("Content-Type", "application/json;charset=utf-8")

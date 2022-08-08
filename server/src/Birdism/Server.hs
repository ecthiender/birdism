module Birdism.Server
  ( httpApp
  ) where

import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Network.Wai.Middleware.Static        (addBase, staticPolicy)
import           Servant                              (ServerError (..), err400, err500,
                                                       hoistServer, serve)

import qualified Data.Aeson                           as J
import qualified Network.Wai                          as Wai
import qualified Servant

import           Birdism.Api                          (birdismApiServer, serverProxy)
import           Birdism.Common
import           Birdism.Config
import           Service.Flickr.Context               (FlickrError (..))


newtype AppM a
  = AppM { unAppM :: ReaderT AppCtx (ExceptT AppError IO) a }
  deriving newtype ( Functor, Applicative, Monad
                   , MonadReader AppCtx
                   , MonadError AppError
                   , MonadIO
                   )

runAppM :: AppM a -> AppCtx -> IO (Either AppError a)
runAppM app = runExceptT . runReaderT (unAppM app)

httpApp :: AppCtx -> Wai.Application
httpApp ctx =
  logStdoutDev
  $ staticPolicy (addBase "../app/")
  $ serve serverProxy $ hoistServer serverProxy (appMToServantHandler ctx) birdismApiServer

appMToServantHandler :: AppCtx -> AppM a -> Servant.Handler a
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
      AEFlickrError flErr -> case flErr of
        FlickrErrorSearch _        -> err400
        FlickrErrorParseResponse _ -> err400
        FlickrErrorUnexpected _    -> err500
      AEDbError _     -> err500
      AEConfigError _ -> err400
    jsonHeader = ("Content-Type", "application/json;charset=utf-8")

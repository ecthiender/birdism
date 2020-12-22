{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Worker.PopulateTaxonomy where

import           Common
import           Control.Lens
import           Config
import           HTTP
import           Init

import qualified Data.ByteString.Lazy            as BL
import qualified Data.Text.Encoding              as T
import qualified Database.PostgreSQL.Simple      as PG
import qualified Database.PostgreSQL.Simple.Copy as PG
import qualified System.Exit                     as Sys

taxonomyUrl :: String
taxonomyUrl = "https://api.ebird.org/v2/ref/taxonomy/ebird"

populateTaxonomy :: AppConfig -> IO ()
populateTaxonomy config = do
  ctx@AppCtx{..} <- initialiseAppCtx config
  truncateTaxonomyTable (ctx ^. dbConnection)
  resp <- runExceptT $ httpGet taxonomyUrl [("X-eBirdApiToken", T.encodeUtf8 $ (ctx ^. ebcToken))]
  case resp of
    Left e -> putStrLn ("FATAL ERROR: could not receive taxonomy from ebird: " <> show e) >> Sys.exitFailure
    Right res -> do
      PG.copy_ (ctx ^. dbConnection) copyQ
      PG.putCopyData (ctx ^. dbConnection) $ BL.toStrict res
      reply <- PG.putCopyEnd (ctx ^. dbConnection)
      print reply

copyQ :: PG.Query
copyQ = "COPY taxonomy(scientific_name, common_name, species_code, category, taxonomy_order, common_name_code, scientific_name_code, banding_codes, order_name, family_common_name, family_scientific_name, report_as, extinct, extinct_year) FROM STDIN DELIMITER ',' CSV HEADER;"

truncateTaxonomyTable :: PG.Connection -> IO ()
truncateTaxonomyTable conn = do
  let q = "DELETE FROM taxonomy"
  res <- liftIO $ PG.execute_ conn q
  liftIO $ putStrLn $ "TRUNCATED taxonomy TABLE. Deleted Rows: " <> show res

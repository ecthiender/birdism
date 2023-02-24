module Worker.PopulateRegion where

import           Control.Lens

import qualified Control.Retry              as Retry
import qualified Data.Text                  as T
import qualified Database.PostgreSQL.Simple as PG
import qualified Say

import           Birdism.Common
import           Birdism.Config
import           Birdism.Init
import           Birdism.Types
import           Service.Ebird


populateRegion :: AppConfig -> IO ()
populateRegion config = do
  -- Step 1: initialise 'AppCtx'
  ctx@AppCtx{..} <- initialiseAppCtx config

  -- Step 2: truncate existing table
  sayDebug "Truncating the existing region table.."
  truncateRegionTable (_dbConnection _axDbConn)

  -- Step 3: get the countries first
  countries <- runWithRetry getCountries _axEbirdConf
  sayDebug $ "Retrieved all countries. " <> tshow (length countries) <> " countries..."

  -- Step 4.1: insert the countries into db
  insertRegions (_dbConnection _axDbConn) countries
  sayDebug "countries written to DB"

  -- ~~Step 4.1: concurrently, get subnationals-1 of each country~~
  -- Step 5: get subnationals1 syncly
  subnats1 <- liftIO $ getSubnats1 ctx countries

  sayDebug $ "Total subnationals-1 is: " <> tshow (length subnats1)

  sayDebug $ "Filtering subnats-1 which we know don't have subnats-2: " <> tshow (length noSubNats2)
  let filteredSubNats1 = filter checkPrefix subnats1
      checkPrefix sns = case sns of
        []     -> False
        (sn:_) -> any (\countryCode -> not $ countryCode `T.isPrefixOf` uRegionCode (_rCode sn)) noSubNats2

  sayDebug $ "Filtered subnationals-1 is: " <> tshow (length filteredSubNats1)

  forM_ filteredSubNats1 $ \subnat1 -> do
    sayDebug $ "running one subnational1: " <> tshow subnat1
    subnats2 <- getSubnats2 ctx subnat1
    sayDebug $ "got " <> tshow (length subnats2) <> " subnational-2 regions"
    sayDebug "inserted subnational2s. cooling off for 10 secs.."
    sleep 10
  where
    getSubnats1 ctx countries = do
      forM countries $ \country -> do
        let regCode = uRegionCode $ _rCode country
            regName = _rName country
        sayDebug $ "getting subnational-1 region of country: " <> regName <> " : " <> regCode
        subnats1 <- runWithRetry (getSubnationa1Regions country) ctx
        sayDebug $ "got " <> tshow (length subnats1) <> " subnational-1 regions"
        -- sayDebug $ "subnats1: " <> tshow subnats1
        insertRegions (ctx ^. dbConnection) subnats1
        sayDebug $ "inserted subnats1 for country: " <> regCode <> " cooling off for 3 secs"
        sleep 3
        return subnats1

    -- getSubnats1Async ctx countries = do
    --   Async.forConcurrently countries $ \country -> do
    --     sayDebug $ "getting subnational-1 region of country: " <> (uRegionCode $ _rCode country)
    --     subnats1 <- runWithRetry (getSubnationa1Regions country) ctx
    --     insertRegions (_dbConnection $ _axDbConn ctx) subnats1
    --     return subnats1

    getSubnats2 ctx subnats1 = do
      forM subnats1 $ \subnat1 -> do
        sayDebug $ "getting subnational-2 region of subnational-1: " <> uRegionCode (_rCode subnat1)
        subnats2 <- runWithRetry (getSubnationa2Regions subnat1) ctx
        insertRegions (_dbConnection $ _axDbConn ctx) subnats2
        sleep 3

    -- getSubnats2Async ctx subnats1 = do
    --   Async.forConcurrently subnats1 $ \subnat1 -> do
    --     sayDebug $ "getting subnational-2 region of subnational-1: " <> (uRegionCode $ _rCode subnat1)
    --     subnats2 <- runWithRetry (getSubnationa2Regions subnat1) ctx
    --     insertRegions (_dbConnection $ _axDbConn ctx) subnats2

insertRegions :: PG.Connection -> [RRegion a] -> IO ()
insertRegions conn regions = do
  let q = "INSERT INTO region (region_code, region_name) VALUES (?, ?)"
  res <- liftIO $ PG.executeMany conn q $ map (\(RRegion code name) -> (uRegionCode code, name)) regions
  Say.sayString $ "INSERTED. Affected Rows: " <> show res

truncateRegionTable :: PG.Connection -> IO ()
truncateRegionTable conn = do
  let q = "DELETE FROM region"
  res <- liftIO $ PG.execute_ conn q
  Say.sayString $ "TRUNCATED region TABLE. Deleted Rows: " <> show res

runWithRetry :: ReaderT r (ExceptT AppError IO) a -> r -> IO a
runWithRetry trans cfg = do
  let retryPolicy = Retry.exponentialBackoff (2 * 1000 * 1000) <> Retry.limitRetries 10
  res <- Retry.retrying retryPolicy shouldRetry (const $ runExceptT $ runReaderT trans cfg)
  case res of
    Left err -> error $ "AppError occurred: " <> show err
    Right v  -> return v
  where
    shouldRetry status res = case res of
      Left err -> do
        sayDebug $ "[RUNWITHRETRY]: Encountered error: " <> tshow err
        sayDebug $ "[RUNWITHRETRY]: Retrying. Status: " <> tshow status
        return True
      Right _v -> do
        sayDebug "[RUNWITHRETRY]: Success."
        return False

run :: ReaderT r (ExceptT AppError IO) a -> r -> IO a
run trans cfg = do
  res <- runExceptT $ runReaderT trans cfg
  case res of
    Left err -> error $ "AppError occurred: " <> show err
    Right v  -> return v

sayDebug :: (MonadIO m) => Text -> m ()
sayDebug = Say.say . ("[DEBUG] " <>)

noSubNats2 :: [Text]
noSubNats2 = ["BF","BG","BR","BW","BO","BT","BJ","BZ","BE","BY","BB","BD","BH","BS","AZ","AT","AM","AG","AO","AD","DZ","AL","AF","BI","KH","CM","CV","BQ","KY","CF","TD","CN","CO","KM","CG","CR","CI","HR","CU","CY","CZ","DK","DJ","DM","DO","CD","EC","EG","SV","ER","EE","SZ","ET","FJ","FI","PF","GA","GM","GE","GH","GR","GD","GT","GN","GW","GY","HT","HN","HU","IS","IR","IQ","IL","IT","JM","JP","JO","KZ","KE","KI","KW","KG","LA","LV","LB","LS","LR","LY","LI","LT","LU","MG","MW","MY","MV","ML","MT","MH","MR","MU","FM","MD","MN","ME","MA","MZ","MM","NA","NR","NL","NI","NE","NG","MP","KP","MK","NO","OM","PK","PW","PA","PG","PY","PE","PH","PL","PR","QA","RO","RU","RW","SH","KN","LC","VC","WS","SM","ST","SA","SN","RS","SC","SL","SK","SI","SB","SO","ZA","KR","SS","SD","SR","SE","CH","SY","TW","TJ","TZ","TH","TL","TG","TO","TT","TN","TR","TM","TV","UG","UA","AE","UM","UY","UZ","VU","VE","VN","VI","YE","ZM","ZW"]

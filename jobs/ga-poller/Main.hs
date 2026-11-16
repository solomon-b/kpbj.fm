module Main (main) where

--------------------------------------------------------------------------------

import Control.Exception (SomeException, try)
import Control.Monad (forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Crypto.JOSE qualified as JOSE
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT qualified as JWT
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseEither)
import Data.Bifunctor (first)
import Data.ByteString.Char8 qualified as BS8
import Data.ByteString.Lazy qualified as LBS
import Data.Either (lefts)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time
  ( Day,
    NominalDiffTime,
    UTCTime (..),
    addDays,
    addUTCTime,
    defaultTimeLocale,
    formatTime,
    getCurrentTime,
    utctDay,
  )
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 qualified as X509
import Data.X509.Memory qualified as X509
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.GaSnapshots qualified as GaSnapshots
import Hasql.Interpolate (OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Job (JobM, exitWithError, requireEnv, runJob)
import Log qualified
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import System.Exit (exitFailure)

--------------------------------------------------------------------------------

main :: IO ()
main = do
  propertyId <- requireEnv "GA_PROPERTY_ID"
  keyPath <- requireEnv "GA_SERVICE_ACCOUNT_KEY_PATH"
  saKey <- loadServiceAccountKey keyPath
  runJob "ga-poller" (pollAndRecord (Text.pack propertyId) saKey)

--------------------------------------------------------------------------------
-- Top-level workflow

-- | Decide between backfill and steady-state, fetch a token, and run the
-- appropriate batch of GA reports.
pollAndRecord :: Text -> ServiceAccountKey -> JobM ()
pollAndRecord propertyId saKey = do
  Log.logInfo_ "Acquiring GA access token"
  tokenResult <- liftIO $ acquireAccessToken saKey
  token <- case tokenResult of
    Left err -> do
      Log.logAttention "Failed to acquire access token" $ Aeson.object ["error" .= err]
      liftIO exitFailure
    Right t -> pure t

  Log.logInfo_ "Checking ga_snapshots for existing rows"
  hasRows <-
    execQuery snapshotsTableHasRows >>= \case
      Left err -> do
        Log.logAttention "Existence check session error" $ Aeson.object ["error" .= show err]
        liftIO exitFailure
      Right b -> pure b

  now <- liftIO getCurrentTime
  if hasRows
    then do
      Log.logInfo_ "Steady-state mode: polling previous full UTC day"
      let !day = addDays (-1) (utctDay now)
          !windowStart = UTCTime day 0
          !windowEnd = UTCTime (addDays 1 day) 0
      runDailyReports propertyId token day windowStart windowEnd
    else do
      Log.logInfo_ "Backfill mode: polling past 90 days"
      let !today = utctDay now
          !days = [addDays (-d) today | d <- [90, 89 .. 1]]
      forM_ days $ \day -> do
        let !windowStart = UTCTime day 0
            !windowEnd = UTCTime (addDays 1 day) 0
        runDailyReports propertyId token day windowStart windowEnd

-- | Run all three dimension reports for a single calendar day and persist
-- the rows to Postgres.
runDailyReports ::
  Text ->
  AccessToken ->
  Day ->
  UTCTime ->
  UTCTime ->
  JobM ()
runDailyReports propertyId token day windowStart windowEnd = do
  let dimensions =
        [ (GaSnapshots.Source, "sessionSource"),
          (GaSnapshots.Country, "country"),
          (GaSnapshots.City, "city")
        ]
  forM_ dimensions $ \(dimType, gaName) -> do
    Log.logInfo "Running GA report" $
      Aeson.object
        [ "day" .= formatDay day,
          "dimension" .= gaName
        ]
    rowsResult <- liftIO $ runReport propertyId token gaName day
    case rowsResult of
      Left err ->
        Log.logAttention "GA report failed" $
          Aeson.object
            [ "day" .= formatDay day,
              "dimension" .= gaName,
              "error" .= err
            ]
      Right rows -> do
        let !filtered = filter (validDimensionValue . fst) rows
            !inserts =
              [ GaSnapshots.Insert
                  { dimensionType = dimType,
                    dimensionValue = value,
                    metricValue = metric,
                    windowStart = windowStart,
                    windowEnd = windowEnd
                  }
              | (value, metric) <- filtered
              ]
        Log.logInfo "Parsed GA rows" $
          Aeson.object
            [ "day" .= formatDay day,
              "dimension" .= gaName,
              "rows" .= length inserts
            ]
        -- A row that fails does not stop the rows after it. The failures of
        -- one report go in one log line, so a lost connection during the
        -- backfill does not write a line for each row.
        errors <- fmap lefts $ forM inserts $ \ins ->
          execQuery (GaSnapshots.insertSnapshot ins)
        case errors of
          [] -> Log.logInfo_ "Inserted rows"
          (firstErr : _) ->
            Log.logAttention "Insert session error" $
              Aeson.object
                [ "day" .= formatDay day,
                  "dimension" .= gaName,
                  "failed" .= length errors,
                  "rows" .= length inserts,
                  "error" .= show firstErr
                ]

-- | Filter junk dimension values that GA returns for unattributed sessions.
validDimensionValue :: Text -> Bool
validDimensionValue v = not (Text.null v) && v /= "(not set)"

-- | Format a 'Day' as @YYYY-MM-DD@.
formatDay :: Day -> String
formatDay = formatTime defaultTimeLocale "%Y-%m-%d"

--------------------------------------------------------------------------------
-- Database helpers

-- | Returns 'True' if at least one row exists in @ga_snapshots@.
--
-- A @SELECT EXISTS@ always returns one row, so the 'False' is never used.
snapshotsTableHasRows :: Hasql.Statement () Bool
snapshotsTableHasRows =
  maybe False getOneColumn
    <$> interp
      True
      [sql|
      SELECT EXISTS (SELECT 1 FROM ga_snapshots LIMIT 1)
    |]

--------------------------------------------------------------------------------
-- Service account key

-- | A service account key loaded from a JSON file on disk. Holds only the
-- bits we need — caller email, signing key, and token endpoint.
data ServiceAccountKey = ServiceAccountKey
  { sakClientEmail :: Text,
    sakPrivateKey :: JWK.JWK,
    sakTokenUri :: Text
  }

-- | Load and parse a Google service account JSON key file. Fails loudly on
-- I/O or schema errors.
loadServiceAccountKey :: FilePath -> IO ServiceAccountKey
loadServiceAccountKey path = do
  bytes <-
    try @SomeException (LBS.readFile path) >>= \case
      Left e -> exitWithError [i|Failed to read service account key #{path}: #{show e}|]
      Right b -> pure b
  raw <- case Aeson.eitherDecode bytes of
    Left err -> exitWithError [i|Service account key #{path} is not valid JSON: #{err}|]
    Right v -> pure v
  case parseEither parseSaKeyFields raw of
    Left err -> exitWithError [i|Service account key #{path} is missing required fields: #{err}|]
    Right (clientEmail, privateKeyPem, tokenUri) ->
      case parseRsaPrivateKey privateKeyPem of
        Left err -> exitWithError [i|Service account private key in #{path} could not be parsed: #{Text.unpack err}|]
        Right jwk ->
          pure
            ServiceAccountKey
              { sakClientEmail = clientEmail,
                sakPrivateKey = jwk,
                sakTokenUri = tokenUri
              }

-- | Pull @client_email@, @private_key@, and (optionally) @token_uri@ from
-- the service account JSON.
parseSaKeyFields :: Aeson.Value -> Parser (Text, Text, Text)
parseSaKeyFields = Aeson.withObject "ServiceAccountKey" $ \o -> do
  clientEmail <- o .: "client_email"
  privateKey <- o .: "private_key"
  tokenUri <-
    o .:? "token_uri" >>= \case
      Just t -> pure t
      Nothing -> pure "https://oauth2.googleapis.com/token"
  pure (clientEmail, privateKey, tokenUri)

-- | Parse a PEM-encoded PKCS#8 RSA private key into a JOSE 'JWK'.
parseRsaPrivateKey :: Text -> Either Text JWK.JWK
parseRsaPrivateKey pemText =
  case X509.readKeyFileFromMemory (Text.encodeUtf8 pemText) of
    (X509.PrivKeyRSA rsaKey : _) -> Right (JWK.fromRSA rsaKey)
    (_ : _) -> Left "Service account key is not an RSA key"
    [] -> Left "No private key found in PEM data"

--------------------------------------------------------------------------------
-- OAuth2 access token

-- | An opaque OAuth2 bearer token.
newtype AccessToken = AccessToken Text

-- | Acquire a short-lived OAuth2 access token by signing a JWT with the
-- service account key and exchanging it at the token endpoint.
acquireAccessToken :: ServiceAccountKey -> IO (Either String AccessToken)
acquireAccessToken ServiceAccountKey {..} = runExceptT $ do
  now <- liftIOE getCurrentTime
  claims <-
    except $
      first ("Failed to build JWT claims: " <>) $
        makeClaims sakClientEmail sakTokenUri now
  let header = JWS.newJWSHeader ((), JWS.RS256)
  assertion <-
    withExceptT (\(e :: JOSE.Error) -> "JWT signing failed: " <> show e) $
      ExceptT $
        JOSE.runJOSE @JOSE.Error $ do
          signed <- JWT.signClaims sakPrivateKey header claims
          pure (JOSE.encodeCompact signed)
  token <- exchangeAssertion sakTokenUri assertion
  pure (AccessToken token)
  where
    liftIOE :: IO a -> ExceptT String IO a
    liftIOE = ExceptT . fmap Right

-- | Build the OAuth2 JWT bearer claim set required by Google's token
-- endpoint. The token is valid for one hour.
makeClaims :: Text -> Text -> UTCTime -> Either String JWT.ClaimsSet
makeClaims iss aud now = do
  let !nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      !expEpoch = floor (utcTimeToPOSIXSeconds (addUTCTime hour now)) :: Int
      claimsJson =
        Aeson.object
          [ "iss" .= iss,
            "scope" .= ("https://www.googleapis.com/auth/analytics.readonly" :: Text),
            "aud" .= aud,
            "iat" .= nowEpoch,
            "exp" .= expEpoch
          ]
  case Aeson.fromJSON claimsJson of
    Aeson.Success cs -> Right cs
    Aeson.Error err -> Left err
  where
    hour :: NominalDiffTime
    hour = 3600

-- | POST a signed assertion to the token endpoint and pull @access_token@
-- out of the response.
exchangeAssertion :: Text -> LBS.ByteString -> ExceptT String IO Text
exchangeAssertion tokenUri assertion = do
  let assertionBS = LBS.toStrict assertion
  responseE <-
    liftIO' $ try @HTTP.HttpException $ do
      request <- HTTP.parseRequest [i|POST #{tokenUri}|]
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro thirtySeconds) $
              HTTP.setRequestBodyURLEncoded
                [ ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
                  ("assertion", assertionBS)
                ]
                request
      HTTP.httpJSON req
  response <- case responseE of
    Left e -> throwE [i|Token exchange HTTP error: #{show e}|]
    Right r -> pure r
  let !status = HTTP.getResponseStatus response
      !body = HTTP.getResponseBody response :: Aeson.Value
  if not (HTTP.statusIsSuccessful status)
    then
      throwE
        [i|Token exchange returned HTTP #{HTTP.statusCode status}: #{showBody body}|]
    else case parseEither (Aeson.withObject "token" (.: "access_token")) body of
      Right t -> pure t
      Left err -> throwE [i|Token exchange response missing access_token: #{err}|]
  where
    liftIO' = ExceptT . fmap Right
    showBody = Text.unpack . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

--------------------------------------------------------------------------------
-- runReport call

-- | A single (dimension value, metric value) pair from a GA report.
type GaRow = (Text, Int64)

-- | Call @properties/<id>:runReport@ for a single dimension and one calendar
-- day, returning all (dimension value, sessions) rows.
runReport :: Text -> AccessToken -> Text -> Day -> IO (Either String [GaRow])
runReport propertyId (AccessToken token) gaDimension day = runExceptT $ do
  let url =
        [i|POST https://analyticsdata.googleapis.com/v1beta/properties/#{propertyId}:runReport|] ::
          String
      dayText = Text.pack (formatDay day)
      body =
        Aeson.object
          [ "dateRanges"
              .= [ Aeson.object
                     [ "startDate" .= dayText,
                       "endDate" .= dayText
                     ]
                 ],
            "dimensions" .= [Aeson.object ["name" .= gaDimension]],
            "metrics" .= [Aeson.object ["name" .= ("sessions" :: Text)]],
            "orderBys"
              .= [ Aeson.object
                     [ "metric" .= Aeson.object ["metricName" .= ("sessions" :: Text)],
                       "desc" .= True
                     ]
                 ],
            "limit" .= (50 :: Int)
          ]
  responseE <-
    liftIO' $ try @HTTP.HttpException $ do
      request <- HTTP.parseRequest url
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro thirtySeconds) $
              HTTP.addRequestHeader "Authorization" (BS8.pack [i|Bearer #{token}|]) $
                HTTP.setRequestBodyJSON body request
      HTTP.httpJSON req
  response <- case responseE of
    Left e -> throwE [i|GA HTTP error: #{show e}|]
    Right r -> pure r
  let !status = HTTP.getResponseStatus response
      !respBody = HTTP.getResponseBody response :: Aeson.Value
  if not (HTTP.statusIsSuccessful status)
    then
      throwE
        [i|GA returned HTTP #{HTTP.statusCode status}: #{showBody respBody}|]
    else case parseEither parseRunReport respBody of
      Right rows -> pure rows
      Left err -> throwE [i|GA response parse failed: #{err}|]
  where
    liftIO' = ExceptT . fmap Right
    showBody = Text.unpack . Text.decodeUtf8 . LBS.toStrict . Aeson.encode

-- | Parse the @rows@ array of a runReport response into (dimension value,
-- metric value) pairs.
parseRunReport :: Aeson.Value -> Parser [GaRow]
parseRunReport = Aeson.withObject "RunReportResponse" $ \o ->
  o .:? "rows" >>= \case
    Nothing -> pure []
    Just arr -> mapM parseRow arr
  where
    parseRow = Aeson.withObject "ReportRow" $ \o -> do
      dimensionValues <- o .: "dimensionValues"
      metricValues <- o .: "metricValues"
      dimText <- firstValue "dimensionValues" dimensionValues
      metricText <- firstValue "metricValues" metricValues
      case reads (Text.unpack metricText) of
        [(n :: Int64, "")] -> pure (dimText, n)
        _ -> fail [i|Expected integer metric value, got #{Text.unpack metricText}|]

    firstValue :: String -> [Aeson.Value] -> Parser Text
    firstValue field arr = case arr of
      [] -> fail [i|Empty #{field}|]
      (v : _) -> Aeson.withObject field (.: "value") v

--------------------------------------------------------------------------------
-- Misc

-- | 30 second HTTP timeout in microseconds.
thirtySeconds :: Int
thirtySeconds = 30_000_000

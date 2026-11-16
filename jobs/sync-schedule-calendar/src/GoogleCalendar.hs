-- | A small client for the events of one Google Calendar.
--
-- The job logs in as a service account that has edit rights on the calendar.
-- It does not act as a Workspace user, so the token request has no @sub@ claim.
--
-- See <https://developers.google.com/workspace/calendar/api/v3/reference/events>.
module GoogleCalendar
  ( -- * Types
    CalendarConfig (..),
    CalendarError (..),
    displayError,

    -- * Session
    AccessToken,
    acquireToken,

    -- * API
    listEvents,
    insertEvent,
    updateEvent,
    patchEvent,
    deleteEvent,

    -- * Response parser
    parseListPage,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (try)
import Control.Monad (unless)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, throwE, withExceptT)
import Crypto.JOSE qualified as JOSE
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT qualified as JWT
import Data.Aeson ((.!=), (.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseEither, parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.Maybe (maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, addUTCTime, getCurrentTime)
import Data.Time.Clock.POSIX (utcTimeToPOSIXSeconds)
import Data.X509 qualified as X509
import Data.X509.Memory qualified as X509
import Network.HTTP.Client qualified as HTTPClient
import Network.HTTP.Simple qualified as HTTP
import Network.HTTP.Types.Status qualified as HTTP
import Network.HTTP.Types.URI (urlEncode)

--------------------------------------------------------------------------------
-- Types

-- | The service account and the calendar that it writes to.
data CalendarConfig = CalendarConfig
  { ccSaEmail :: Text,
    ccSaKey :: Text,
    ccCalendarId :: Text
  }

-- | An opaque OAuth2 access token.
newtype AccessToken = AccessToken Text

-- | An error from the token flow or from the Calendar API.
data CalendarError
  = KeyParsingFailed Text
  | ClaimsCreationFailed Text
  | JWTSigningFailed JOSE.Error
  | TokenExchangeHttpError HTTP.HttpException
  | TokenExchangeUnexpectedResponse Text
  | ListHttpError HTTP.HttpException
  | -- | The status code and the start of the response body.
    ListHttpStatus Int Text
  | ListParseFailed Text
  | -- | The operation, then the exception.
    WriteHttpError Text HTTP.HttpException
  | -- | The operation, the status code, and the start of the response body.
    WriteHttpStatus Text Int Text
  deriving stock (Show)

-- | Render an error as text for the log.
displayError :: CalendarError -> Text
displayError = \case
  KeyParsingFailed err -> [i|Key parsing failed: #{err}|]
  ClaimsCreationFailed err -> [i|Claims creation failed: #{err}|]
  JWTSigningFailed _err -> "JWT signing failed."
  TokenExchangeHttpError err -> [i|Token exchange HTTP error: #{httpCause err}|]
  TokenExchangeUnexpectedResponse body -> [i|Token exchange: unexpected response body: #{body}|]
  ListHttpError err -> [i|Events list HTTP error: #{httpCause err}|]
  ListHttpStatus code body -> [i|Events list error: HTTP #{code}: #{body}|]
  ListParseFailed err -> [i|Events list: unexpected response body: #{err}|]
  WriteHttpError operation err -> [i|#{operation}: HTTP error: #{httpCause err}|]
  WriteHttpStatus operation code body -> [i|#{operation}: HTTP #{code}: #{body}|]

-- | The cause of an HTTP exception, without the request. The request holds the
-- access token, so it must stay out of the log.
httpCause :: HTTP.HttpException -> Text
httpCause = \case
  HTTP.HttpExceptionRequest _request content -> Text.pack (show content)
  HTTP.InvalidUrlException _url reason -> Text.pack reason

-- | The start of a response body, as text for an error.
bodyExcerpt :: LBS.ByteString -> Text
bodyExcerpt = Text.take 500 . Text.decodeUtf8Lenient . LBS.toStrict

-- | Catch 'HTTP.HttpException' and wrap it with the given constructor.
tryHttp :: (HTTP.HttpException -> CalendarError) -> IO a -> ExceptT CalendarError IO a
tryHttp wrapErr action = ExceptT $ first wrapErr <$> try @HTTP.HttpException action

--------------------------------------------------------------------------------
-- Public API

-- | Acquire an OAuth2 access token for the Calendar API.
acquireToken :: CalendarConfig -> IO (Either CalendarError AccessToken)
acquireToken CalendarConfig {..} = runExceptT $ AccessToken <$> getAccessToken ccSaEmail ccSaKey

-- | List every event of the calendar as raw JSON.
--
-- The list holds single events, the parents of recurring events, and the
-- exceptions to recurring events. It does not expand a recurring event into its
-- airings.
listEvents :: AccessToken -> Text -> IO (Either CalendarError [Aeson.Value])
listEvents token calendarId = runExceptT (go Nothing [])
  where
    go :: Maybe Text -> [Aeson.Value] -> ExceptT CalendarError IO [Aeson.Value]
    go mPageToken acc = do
      (items, mNextToken) <- fetchEventsPage token calendarId mPageToken
      let allItems = acc <> items
      case mNextToken of
        Nothing -> pure allItems
        Just nextToken -> go (Just nextToken) allItems

-- | Create an event from an @events.insert@ body.
insertEvent :: AccessToken -> Text -> Aeson.Value -> IO (Either CalendarError ())
insertEvent token calendarId body =
  write "Insert event" "POST" token (eventsUrl calendarId) (Just body)

-- | Replace an event with an @events.update@ body.
updateEvent :: AccessToken -> Text -> Text -> Aeson.Value -> IO (Either CalendarError ())
updateEvent token calendarId eventId body =
  write [i|Update event #{eventId}|] "PUT" token (eventUrl calendarId eventId) (Just body)

-- | Change only the fields that the body names.
patchEvent :: AccessToken -> Text -> Text -> Aeson.Value -> IO (Either CalendarError ())
patchEvent token calendarId eventId body =
  write [i|Patch event #{eventId}|] "PATCH" token (eventUrl calendarId eventId) (Just body)

-- | Delete an event.
deleteEvent :: AccessToken -> Text -> Text -> IO (Either CalendarError ())
deleteEvent token calendarId eventId =
  write [i|Delete event #{eventId}|] "DELETE" token (eventUrl calendarId eventId) Nothing

--------------------------------------------------------------------------------
-- Calendar API

-- | The events collection of a calendar. A calendar id contains @\@@, so the
-- URL holds it in encoded form.
eventsUrl :: Text -> String
eventsUrl calendarId =
  [i|https://www.googleapis.com/calendar/v3/calendars/#{encodeSegment calendarId}/events|]

eventUrl :: Text -> Text -> String
eventUrl calendarId eventId = [i|#{eventsUrl calendarId}/#{encodeSegment eventId}|]

encodeSegment :: Text -> Text
encodeSegment = Text.decodeUtf8 . urlEncode True . Text.encodeUtf8

-- | Send one write request and check the status.
write :: Text -> ByteString -> AccessToken -> String -> Maybe Aeson.Value -> IO (Either CalendarError ())
write operation method (AccessToken token) url mBody = runExceptT $ do
  response <-
    tryHttp (WriteHttpError operation) $ do
      request <- HTTP.parseRequest url
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod method $
                HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|] $
                  maybe id HTTP.setRequestBodyJSON mBody request
      HTTP.httpLBS req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (WriteHttpStatus operation (HTTP.statusCode status) (bodyExcerpt (HTTP.getResponseBody response)))

-- | Fetch a single page of events.
fetchEventsPage :: AccessToken -> Text -> Maybe Text -> ExceptT CalendarError IO ([Aeson.Value], Maybe Text)
fetchEventsPage (AccessToken token) calendarId mPageToken = do
  let queryParams =
        [ ("singleEvents", Just "false"),
          ("showDeleted", Just "false"),
          ("maxResults", Just "2500")
        ]
          <> maybeToList (fmap (\pt -> ("pageToken", Just (Text.encodeUtf8 pt))) mPageToken)
  response <-
    tryHttp ListHttpError $ do
      request <- HTTP.parseRequest (eventsUrl calendarId)
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|] $
                HTTP.setRequestQueryString queryParams request
      HTTP.httpLBS req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (ListHttpStatus (HTTP.statusCode status) (bodyExcerpt (HTTP.getResponseBody response)))
  value <-
    except $
      first (ListParseFailed . Text.pack) (Aeson.eitherDecode (HTTP.getResponseBody response))
  except $ first ListParseFailed (parseListPage value)

-- | Read the items and the token of the next page from an @events.list@
-- response. An empty calendar gives a response with no @items@ field.
parseListPage :: Aeson.Value -> Either Text ([Aeson.Value], Maybe Text)
parseListPage =
  first Text.pack . parseEither (Aeson.withObject "EventsResponse" $ \o -> (,) <$> o .:? "items" .!= [] <*> o .:? "nextPageToken")

--------------------------------------------------------------------------------
-- Google OAuth2 JWT Authentication
--
-- This is the token flow of jobs/sync-host-emails/src/GoogleGroups.hs, without
-- the sub claim.

-- | The scope that permits changes to the events of a shared calendar.
calendarScope :: Text
calendarScope = "https://www.googleapis.com/auth/calendar.events"

-- | Get an OAuth2 access token using service account credentials.
getAccessToken :: Text -> Text -> ExceptT CalendarError IO Text
getAccessToken saEmail saKey = do
  now <- liftIO getCurrentTime
  jwk <- except $ first KeyParsingFailed $ parseServiceAccountKey saKey
  claimsSet <- except $ first ClaimsCreationFailed $ makeClaimsSet saEmail calendarScope now
  let header = JWS.newJWSHeader ((), JWS.RS256)
  assertion <-
    withExceptT JWTSigningFailed $ ExceptT $ JOSE.runJOSE @JOSE.Error $ do
      signed <- JWT.signClaims jwk header claimsSet
      pure $ JOSE.encodeCompact signed
  exchangeToken assertion
  where
    liftIO = ExceptT . fmap Right

-- | Construct a JWT ClaimsSet for Google's OAuth2 token endpoint.
makeClaimsSet :: Text -> Text -> UTCTime -> Either Text JWT.ClaimsSet
makeClaimsSet iss scope now = do
  let nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      expEpoch = floor (utcTimeToPOSIXSeconds (addUTCTime 3600 now)) :: Int
      claimsJson =
        Aeson.object
          [ "iss" .= iss,
            "aud" .= ("https://oauth2.googleapis.com/token" :: Text),
            "iat" .= nowEpoch,
            "exp" .= expEpoch,
            "scope" .= scope
          ]
  case Aeson.fromJSON claimsJson of
    Aeson.Success cs -> Right cs
    Aeson.Error err -> Left (Text.pack err)

-- | Exchange a signed JWT assertion for an access token.
exchangeToken :: LBS.ByteString -> ExceptT CalendarError IO Text
exchangeToken assertion = do
  let assertionBS = LBS.toStrict assertion
  response <-
    tryHttp TokenExchangeHttpError $ do
      request <- HTTP.parseRequest "https://oauth2.googleapis.com/token"
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod "POST" $
                HTTP.setRequestBodyURLEncoded
                  [ ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
                    ("assertion", assertionBS)
                  ]
                  request
      HTTP.httpJSON req
  let body = HTTP.getResponseBody response :: Aeson.Value
  case parseMaybe (Aeson.withObject "token" (.: "access_token")) body of
    Just token -> pure token
    Nothing ->
      let bodyText = Text.decodeUtf8 $ LBS.toStrict $ Aeson.encode body
       in throwE (TokenExchangeUnexpectedResponse bodyText)

--------------------------------------------------------------------------------
-- PEM / PKCS#8 Key Parsing

-- | Parse a PEM-encoded PKCS#8 RSA private key into a JWK.
parseServiceAccountKey :: Text -> Either Text JWK.JWK
parseServiceAccountKey pemText =
  case X509.readKeyFileFromMemory (Text.encodeUtf8 pemText) of
    (X509.PrivKeyRSA rsaKey : _) -> Right (JWK.fromRSA rsaKey)
    (_ : _) -> Left "Service account key is not an RSA key"
    [] -> Left "No private key found in PEM data"

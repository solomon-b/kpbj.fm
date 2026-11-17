-- | The OAuth2 token flow of a Google service account.
--
-- The caller signs a JWT with the private key of the account and exchanges it
-- for an access token. An account with domain-wide delegation can act as a
-- Workspace user. The @sub@ claim names that user.
--
-- See <https://developers.google.com/identity/protocols/oauth2/service-account#httprest>.
module Google.ServiceAccount
  ( -- * Credentials
    ServiceAccount (..),

    -- * Token
    AccessToken,
    acquireToken,
    authorize,

    -- * Errors
    TokenError (..),
    displayTokenError,
    httpCause,

    -- * Parts of the flow
    makeClaimsSet,
    parseTokenResponse,
    parseServiceAccountKey,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (try)
import Control.Monad.Trans.Except (ExceptT (..), except, runExceptT, withExceptT)
import Crypto.JOSE qualified as JOSE
import Crypto.JOSE.JWK qualified as JWK
import Crypto.JOSE.JWS qualified as JWS
import Crypto.JWT qualified as JWT
import Data.Aeson ((.:), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe)
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as LBS
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

--------------------------------------------------------------------------------
-- Credentials

-- | The identity of a service account.
data ServiceAccount = ServiceAccount
  { saEmail :: Text,
    -- | The RSA private key in PEM format.
    saPrivateKey :: Text
  }

--------------------------------------------------------------------------------
-- Token

-- | An opaque OAuth2 access token. It has no 'Show' instance, so it stays out
-- of the log.
newtype AccessToken = AccessToken Text

-- | Acquire an access token.
acquireToken ::
  ServiceAccount ->
  -- | The scope of the token.
  Text ->
  -- | The Workspace user to act as. 'Nothing' means the service account acts
  -- as itself.
  Maybe Text ->
  IO (Either TokenError AccessToken)
acquireToken ServiceAccount {..} scope subject = runExceptT $ do
  now <- ExceptT (Right <$> getCurrentTime)
  jwk <- except $ first KeyParsingFailed $ parseServiceAccountKey saPrivateKey
  claimsSet <- except $ first ClaimsCreationFailed $ makeClaimsSet saEmail subject scope now
  let header = JWS.newJWSHeader ((), JWS.RS256)
  assertion <-
    withExceptT JWTSigningFailed $ ExceptT $ JOSE.runJOSE @JOSE.Error $ do
      signed <- JWT.signClaims jwk header claimsSet
      pure $ JOSE.encodeCompact signed
  exchangeToken assertion

-- | Add the token to a request as a bearer token.
authorize :: AccessToken -> HTTP.Request -> HTTP.Request
authorize (AccessToken token) = HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|]

--------------------------------------------------------------------------------
-- Errors

-- | An error from the token flow.
data TokenError
  = KeyParsingFailed Text
  | ClaimsCreationFailed Text
  | JWTSigningFailed JOSE.Error
  | TokenExchangeHttpError HTTP.HttpException
  | TokenExchangeUnexpectedResponse Text
  deriving stock (Show)

-- | Render an error as text for the log.
displayTokenError :: TokenError -> Text
displayTokenError = \case
  KeyParsingFailed err -> [i|Key parsing failed: #{err}|]
  ClaimsCreationFailed err -> [i|Claims creation failed: #{err}|]
  JWTSigningFailed _err -> "JWT signing failed."
  TokenExchangeHttpError err -> [i|Token exchange HTTP error: #{httpCause err}|]
  TokenExchangeUnexpectedResponse body -> [i|Token exchange: unexpected response body: #{body}|]

-- | The cause of an HTTP exception, without the request. The request of an
-- authorized call holds the access token, so it must stay out of the log.
httpCause :: HTTP.HttpException -> Text
httpCause = \case
  HTTP.HttpExceptionRequest _request content -> Text.pack (show content)
  HTTP.InvalidUrlException _url reason -> Text.pack reason

--------------------------------------------------------------------------------
-- Parts of the flow

-- | Construct the claims of the JWT for the token endpoint.
makeClaimsSet ::
  -- | The email of the service account.
  Text ->
  -- | The Workspace user to act as.
  Maybe Text ->
  -- | The scope of the token.
  Text ->
  -- | The current time. The claims expire one hour later.
  UTCTime ->
  Either Text JWT.ClaimsSet
makeClaimsSet iss subject scope now = do
  let nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      expEpoch = floor (utcTimeToPOSIXSeconds (addUTCTime 3600 now)) :: Int
      claimsJson =
        Aeson.object $
          [ "iss" .= iss,
            "aud" .= ("https://oauth2.googleapis.com/token" :: Text),
            "iat" .= nowEpoch,
            "exp" .= expEpoch,
            "scope" .= scope
          ]
            <> ["sub" .= sub | Just sub <- [subject]]
  case Aeson.fromJSON claimsJson of
    Aeson.Success cs -> Right cs
    Aeson.Error err -> Left (Text.pack err)

-- | Exchange a signed JWT assertion for an access token.
exchangeToken :: LBS.ByteString -> ExceptT TokenError IO AccessToken
exchangeToken assertion = do
  let assertionBS = LBS.toStrict assertion
  response <-
    ExceptT . fmap (first TokenExchangeHttpError) . try @HTTP.HttpException $ do
      request <- HTTP.parseRequest "https://oauth2.googleapis.com/token"
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod "POST" $
                HTTP.setRequestBodyURLEncoded
                  [ ("grant_type", "urn:ietf:params:oauth:grant-type:jwt-bearer"),
                    ("assertion", assertionBS)
                  ]
                  request
      HTTP.httpLBS req
  except $ parseTokenResponse (HTTP.getResponseBody response)

-- | Read the access token from the response of the token endpoint.
--
-- A refused request has a JSON body with no token. A body that is not JSON
-- can come from a proxy. The error carries the start of each such body.
parseTokenResponse :: LBS.ByteString -> Either TokenError AccessToken
parseTokenResponse body =
  maybe (Left (TokenExchangeUnexpectedResponse excerpt)) (Right . AccessToken) $
    Aeson.decode body >>= parseMaybe (Aeson.withObject "token" (.: "access_token"))
  where
    excerpt :: Text
    excerpt = Text.take 500 (Text.decodeUtf8Lenient (LBS.toStrict body))

-- | Parse a PEM-encoded PKCS#8 RSA private key into a JWK.
parseServiceAccountKey :: Text -> Either Text JWK.JWK
parseServiceAccountKey pemText =
  case X509.readKeyFileFromMemory (Text.encodeUtf8 pemText) of
    (X509.PrivKeyRSA rsaKey : _) -> Right (JWK.fromRSA rsaKey)
    (_ : _) -> Left "Service account key is not an RSA key"
    [] -> Left "No private key found in PEM data"

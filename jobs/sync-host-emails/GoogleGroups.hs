module GoogleGroups
  ( -- * Types
    GoogleGroupsConfig (..),
    GroupMember (..),
    GroupMemberRole (..),
    GoogleGroupsError (..),
    displayError,

    -- * Session
    AccessToken,
    acquireToken,

    -- * API
    listGroupMembers,
    addGroupMember,
    removeGroupMember,
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
import Data.Aeson ((.=), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Bifunctor (first)
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

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Configuration for Google Groups API access.
data GoogleGroupsConfig = GoogleGroupsConfig
  { ggSaEmail :: Text,
    ggSaKey :: Text,
    ggDelegatedUser :: Text,
    ggGroupEmail :: Text
  }

-- | An opaque OAuth2 access token.
newtype AccessToken = AccessToken Text

-- | Role of a member within a Google Group.
data GroupMemberRole
  = RoleOwner
  | RoleManager
  | RoleMember
  | RoleUnknown Text
  deriving stock (Show, Eq)

-- | A member of a Google Group.
data GroupMember = GroupMember
  { gmEmail :: Text,
    gmRole :: GroupMemberRole,
    gmType :: Text,
    gmStatus :: Text
  }
  deriving stock (Show, Eq)

-- | Error that can occur while interacting with Google Groups.
data GoogleGroupsError
  = KeyParsingFailed Text
  | ClaimsCreationFailed Text
  | JWTSigningFailed JOSE.Error
  | TokenExchangeHttpError HTTP.HttpException
  | TokenExchangeUnexpectedResponse Text
  | MembersApiHttpError HTTP.HttpException
  | MembersApiHttpStatus Int
  | MembersApiParseFailed
  | AddMemberHttpError HTTP.HttpException
  | AddMemberHttpStatus Int Text
  | RemoveMemberHttpError HTTP.HttpException
  | RemoveMemberHttpStatus Int Text
  deriving stock (Show)

-- | Render an error as user-facing text.
displayError :: GoogleGroupsError -> Text
displayError = \case
  KeyParsingFailed err -> [i|Key parsing failed: #{err}|]
  ClaimsCreationFailed err -> [i|Claims creation failed: #{err}|]
  JWTSigningFailed _err -> "JWT signing failed."
  TokenExchangeHttpError _err -> "Token exchange HTTP error."
  TokenExchangeUnexpectedResponse body -> [i|Token exchange: unexpected response body: #{body}|]
  MembersApiHttpError _err -> "Members API HTTP error."
  MembersApiHttpStatus code -> [i|Members API error: HTTP #{code}.|]
  MembersApiParseFailed -> "Failed to parse members response."
  AddMemberHttpError _err -> "Add member HTTP error."
  AddMemberHttpStatus code email -> [i|Add member #{email}: HTTP #{code}.|]
  RemoveMemberHttpError _err -> "Remove member HTTP error."
  RemoveMemberHttpStatus code email -> [i|Remove member #{email}: HTTP #{code}.|]

-- | Catch 'HTTP.HttpException' and wrap it with the given constructor.
tryHttp :: (HTTP.HttpException -> GoogleGroupsError) -> IO a -> ExceptT GoogleGroupsError IO a
tryHttp wrapErr action = ExceptT $ first wrapErr <$> try @HTTP.HttpException action

--------------------------------------------------------------------------------
-- Public API
--------------------------------------------------------------------------------

-- | Acquire an OAuth2 access token for Google Admin SDK.
acquireToken :: GoogleGroupsConfig -> IO (Either GoogleGroupsError AccessToken)
acquireToken GoogleGroupsConfig {..} = runExceptT $ do
  token <- getAccessToken ggSaEmail ggSaKey ggDelegatedUser
  pure (AccessToken token)

-- | List all members of the configured Google Group.
listGroupMembers :: AccessToken -> Text -> IO (Either GoogleGroupsError [GroupMember])
listGroupMembers (AccessToken token) groupEmail = runExceptT $
  fetchAllMembers token groupEmail

-- | Add an email address to a Google Group as a MEMBER.
addGroupMember :: AccessToken -> Text -> Text -> IO (Either GoogleGroupsError ())
addGroupMember (AccessToken token) groupEmail memberEmail = runExceptT $ do
  let url = [i|https://admin.googleapis.com/admin/directory/v1/groups/#{groupEmail}/members|] :: String
      body =
        Aeson.object
          [ "email" .= memberEmail,
            "role" .= ("MEMBER" :: Text)
          ]
  response <-
    tryHttp AddMemberHttpError $ do
      request <- HTTP.parseRequest url
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod "POST" $
                HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|] $
                  HTTP.addRequestHeader "Content-Type" "application/json" $
                    HTTP.setRequestBodyJSON body request
      HTTP.httpNoBody req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (AddMemberHttpStatus (HTTP.statusCode status) memberEmail)

-- | Remove an email address from a Google Group.
removeGroupMember :: AccessToken -> Text -> Text -> IO (Either GoogleGroupsError ())
removeGroupMember (AccessToken token) groupEmail memberEmail = runExceptT $ do
  let url = [i|https://admin.googleapis.com/admin/directory/v1/groups/#{groupEmail}/members/#{memberEmail}|] :: String
  response <-
    tryHttp RemoveMemberHttpError $ do
      request <- HTTP.parseRequest url
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod "DELETE" $
                HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|] request
      HTTP.httpNoBody req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (RemoveMemberHttpStatus (HTTP.statusCode status) memberEmail)

--------------------------------------------------------------------------------
-- Google OAuth2 JWT Authentication
--------------------------------------------------------------------------------

-- | Google Admin SDK scope for group member management.
directoryScope :: Text
directoryScope = "https://www.googleapis.com/auth/admin.directory.group.member"

-- | Get an OAuth2 access token using service account credentials.
getAccessToken :: Text -> Text -> Text -> ExceptT GoogleGroupsError IO Text
getAccessToken saEmail saKey delegatedUser = do
  now <- liftIO getCurrentTime
  jwk <- except $ first KeyParsingFailed $ parseServiceAccountKey saKey
  claimsSet <- except $ first ClaimsCreationFailed $ makeClaimsSet saEmail delegatedUser directoryScope now
  let header = JWS.newJWSHeader ((), JWS.RS256)
  assertion <-
    withExceptT JWTSigningFailed $ ExceptT $ JOSE.runJOSE @JOSE.Error $ do
      signed <- JWT.signClaims jwk header claimsSet
      pure $ JOSE.encodeCompact signed
  exchangeToken assertion
  where
    liftIO = ExceptT . fmap Right

-- | Construct a JWT ClaimsSet for Google's OAuth2 token endpoint.
makeClaimsSet :: Text -> Text -> Text -> UTCTime -> Either Text JWT.ClaimsSet
makeClaimsSet iss sub scope now = do
  let nowEpoch = floor (utcTimeToPOSIXSeconds now) :: Int
      expEpoch = floor (utcTimeToPOSIXSeconds (addUTCTime 3600 now)) :: Int
      claimsJson =
        Aeson.object
          [ "iss" .= iss,
            "sub" .= sub,
            "aud" .= ("https://oauth2.googleapis.com/token" :: Text),
            "iat" .= nowEpoch,
            "exp" .= expEpoch,
            "scope" .= scope
          ]
  case Aeson.fromJSON claimsJson of
    Aeson.Success cs -> Right cs
    Aeson.Error err -> Left (Text.pack err)

-- | Exchange a signed JWT assertion for an access token.
exchangeToken :: LBS.ByteString -> ExceptT GoogleGroupsError IO Text
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
-- Google Directory API
--------------------------------------------------------------------------------

-- | Fetch all members of a group, following pagination.
fetchAllMembers :: Text -> Text -> ExceptT GoogleGroupsError IO [GroupMember]
fetchAllMembers token groupEmail = go Nothing []
  where
    go mPageToken acc = do
      (members, mNextToken) <- fetchMembersPage token groupEmail mPageToken
      let allMembers = acc <> members
      case mNextToken of
        Nothing -> pure allMembers
        Just nextToken -> go (Just nextToken) allMembers

-- | Fetch a single page of group members.
fetchMembersPage :: Text -> Text -> Maybe Text -> ExceptT GoogleGroupsError IO ([GroupMember], Maybe Text)
fetchMembersPage token groupEmail mPageToken = do
  let baseUrl = [i|https://admin.googleapis.com/admin/directory/v1/groups/#{groupEmail}/members|] :: String
      queryParams =
        ("maxResults", Just "200") : maybeToList (fmap (\pt -> ("pageToken", Just (Text.encodeUtf8 pt))) mPageToken)
  response <-
    tryHttp MembersApiHttpError $ do
      request <- HTTP.parseRequest baseUrl
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.addRequestHeader "Authorization" [i|Bearer #{token}|] $
                HTTP.setRequestQueryString queryParams request
      HTTP.httpJSON req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (MembersApiHttpStatus $ HTTP.statusCode status)
  case parseMaybe (parseMembersResponse . HTTP.getResponseBody) response of
    Nothing -> throwE MembersApiParseFailed
    Just result -> pure result

-- | Parse the members list response from Google Directory API.
parseMembersResponse :: Aeson.Value -> Parser ([GroupMember], Maybe Text)
parseMembersResponse = Aeson.withObject "MembersResponse" $ \o -> do
  members <-
    o .:? "members" >>= \case
      Nothing -> pure []
      Just arr -> mapM parseMember arr
  nextPageToken <- o .:? "nextPageToken"
  pure (members, nextPageToken)

-- | Parse a single group member from the API response.
parseMember :: Aeson.Value -> Parser GroupMember
parseMember = Aeson.withObject "Member" $ \o -> do
  email <- o .: "email"
  role <- o .: "role" >>= parseRole
  memberType <- o .: "type"
  status <-
    o .:? "status" >>= \case
      Nothing -> pure "UNKNOWN"
      Just s -> pure s
  pure
    GroupMember
      { gmEmail = email,
        gmRole = role,
        gmType = memberType,
        gmStatus = status
      }

-- | Parse a member role string.
parseRole :: Text -> Parser GroupMemberRole
parseRole = \case
  "OWNER" -> pure RoleOwner
  "MANAGER" -> pure RoleManager
  "MEMBER" -> pure RoleMember
  other -> pure (RoleUnknown other)

--------------------------------------------------------------------------------
-- PEM / PKCS#8 Key Parsing
--------------------------------------------------------------------------------

-- | Parse a PEM-encoded PKCS#8 RSA private key into a JWK.
parseServiceAccountKey :: Text -> Either Text JWK.JWK
parseServiceAccountKey pemText =
  case X509.readKeyFileFromMemory (Text.encodeUtf8 pemText) of
    (X509.PrivKeyRSA rsaKey : _) -> Right (JWK.fromRSA rsaKey)
    (_ : _) -> Left "Service account key is not an RSA key"
    [] -> Left "No private key found in PEM data"

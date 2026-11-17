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
import Control.Monad.Trans.Except (ExceptT (..), runExceptT, throwE)
import Data.Aeson ((.:), (.:?), (.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Bifunctor (first)
import Data.Maybe (maybeToList)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Google.ServiceAccount (AccessToken, ServiceAccount (..), TokenError, authorize, displayTokenError)
import Google.ServiceAccount qualified as ServiceAccount
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
  = TokenFailed TokenError
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
  TokenFailed err -> displayTokenError err
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
acquireToken GoogleGroupsConfig {..} =
  first TokenFailed <$> ServiceAccount.acquireToken (ServiceAccount ggSaEmail ggSaKey) directoryScope (Just ggDelegatedUser)

-- | Google Admin SDK scope for group member management.
directoryScope :: Text
directoryScope = "https://www.googleapis.com/auth/admin.directory.group.member"

-- | List all members of the configured Google Group.
listGroupMembers :: AccessToken -> Text -> IO (Either GoogleGroupsError [GroupMember])
listGroupMembers token groupEmail =
  runExceptT $
    fetchAllMembers token groupEmail

-- | Add an email address to a Google Group as a MEMBER.
addGroupMember :: AccessToken -> Text -> Text -> IO (Either GoogleGroupsError ())
addGroupMember token groupEmail memberEmail = runExceptT $ do
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
                authorize token $
                  HTTP.addRequestHeader "Content-Type" "application/json" $
                    HTTP.setRequestBodyJSON body request
      HTTP.httpNoBody req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (AddMemberHttpStatus (HTTP.statusCode status) memberEmail)

-- | Remove an email address from a Google Group.
removeGroupMember :: AccessToken -> Text -> Text -> IO (Either GoogleGroupsError ())
removeGroupMember token groupEmail memberEmail = runExceptT $ do
  let url = [i|https://admin.googleapis.com/admin/directory/v1/groups/#{groupEmail}/members/#{memberEmail}|] :: String
  response <-
    tryHttp RemoveMemberHttpError $ do
      request <- HTTP.parseRequest url
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              HTTP.setRequestMethod "DELETE" $
                authorize token request
      HTTP.httpNoBody req
  let status = HTTP.getResponseStatus response
  unless (HTTP.statusIsSuccessful status) $
    throwE (RemoveMemberHttpStatus (HTTP.statusCode status) memberEmail)

--------------------------------------------------------------------------------
-- Google Directory API
--------------------------------------------------------------------------------

-- | Fetch all members of a group, following pagination.
fetchAllMembers :: AccessToken -> Text -> ExceptT GoogleGroupsError IO [GroupMember]
fetchAllMembers token groupEmail = go Nothing []
  where
    go mPageToken acc = do
      (members, mNextToken) <- fetchMembersPage token groupEmail mPageToken
      let allMembers = acc <> members
      case mNextToken of
        Nothing -> pure allMembers
        Just nextToken -> go (Just nextToken) allMembers

-- | Fetch a single page of group members.
fetchMembersPage :: AccessToken -> Text -> Maybe Text -> ExceptT GoogleGroupsError IO ([GroupMember], Maybe Text)
fetchMembersPage token groupEmail mPageToken = do
  let baseUrl = [i|https://admin.googleapis.com/admin/directory/v1/groups/#{groupEmail}/members|] :: String
      queryParams =
        ("maxResults", Just "200") : maybeToList (fmap (\pt -> ("pageToken", Just (Text.encodeUtf8 pt))) mPageToken)
  response <-
    tryHttp MembersApiHttpError $ do
      request <- HTTP.parseRequest baseUrl
      let req =
            HTTP.setRequestResponseTimeout (HTTPClient.responseTimeoutMicro 10_000_000) $
              authorize token $
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

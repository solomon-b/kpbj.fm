-- | Mailchimp Marketing API client functions.
--
-- 'mkClient' bundles a 'Manager', an API key, and an audience id into a
-- record of three pre-bound IO operations. The base URL is derived from
-- the API key's data center suffix and the auth header is computed once,
-- so callers never need to repeat that plumbing.
module Mailchimp.Client
  ( -- * Error Type
    MailchimpError (..),

    -- * Client Bundle
    MailchimpClient (..),
    mkClient,
  )
where

--------------------------------------------------------------------------------

import Data.Bifunctor (first)
import Data.ByteString.Base64 qualified as Base64
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.EmailAddress (EmailAddress)
import Mailchimp.API (MailchimpAPI)
import Mailchimp.Config
  ( MailchimpApiKey (..),
    MailchimpAudienceId (..),
    parseDataCenter,
  )
import Mailchimp.Types
  ( ListMembersResponse,
    Member,
    MemberStatus (..),
    UpsertMemberBody (..),
    subscriberHash,
  )
import Network.HTTP.Client (Manager)
import Servant.API (NoContent (..), (:<|>) (..))
import Servant.Client qualified as Client

--------------------------------------------------------------------------------
-- Error Type

-- | An error from a Mailchimp API call, wrapping the underlying
-- 'Client.ClientError'.
newtype MailchimpError = MailchimpError {unMailchimpError :: Client.ClientError}
  deriving stock (Show)

--------------------------------------------------------------------------------
-- Client Bundle

-- | A bundle of pre-bound Mailchimp operations.
--
-- 'mkClient' partially applies the audience id and Authorization header so
-- callers only supply the operation-specific arguments.
data MailchimpClient = MailchimpClient
  { mcUpsertMember :: EmailAddress -> IO (Either MailchimpError Member),
    mcArchiveMember :: EmailAddress -> IO (Either MailchimpError ()),
    mcListMembers :: Int -> Int -> IO (Either MailchimpError ListMembersResponse)
  }

-- | Construct a 'MailchimpClient' from the shared HTTP manager and config.
--
-- Returns 'Left' if the API key does not contain a recognizable data
-- center suffix.
mkClient ::
  Manager ->
  MailchimpApiKey ->
  MailchimpAudienceId ->
  Either Text MailchimpClient
mkClient manager apiKey (MailchimpAudienceId listId) = do
  dc <- parseDataCenter apiKey
  let baseUrl = mkBaseUrl dc
      env = Client.mkClientEnv manager baseUrl
      authHeader = mkAuthHeader apiKey
      upsertMemberClient :<|> archiveMemberClient :<|> listMembersClient =
        Client.client (Proxy :: Proxy MailchimpAPI)
      run :: Client.ClientM a -> IO (Either MailchimpError a)
      run action = first MailchimpError <$> Client.runClientM action env
  pure $
    MailchimpClient
      { mcUpsertMember = \email ->
          run $
            upsertMemberClient
              listId
              (subscriberHash email)
              authHeader
              (UpsertMemberBody email Subscribed),
        mcArchiveMember = \email ->
          fmap (fmap (\NoContent -> ())) $
            run $
              archiveMemberClient
                listId
                (subscriberHash email)
                authHeader,
        mcListMembers = \count offset ->
          run $
            listMembersClient
              listId
              (Just count)
              (Just offset)
              (Just defaultFields)
              authHeader
      }

--------------------------------------------------------------------------------
-- Internal helpers

-- | Build the @https://<dc>.api.mailchimp.com@ base URL.
mkBaseUrl :: Text -> Client.BaseUrl
mkBaseUrl dc =
  Client.BaseUrl
    { Client.baseUrlScheme = Client.Https,
      Client.baseUrlHost = Text.unpack dc <> ".api.mailchimp.com",
      Client.baseUrlPort = 443,
      Client.baseUrlPath = "/3.0"
    }

-- | Format an 'MailchimpApiKey' as an HTTP Basic @Authorization@ header.
--
-- Mailchimp accepts any non-empty username paired with the API key as the
-- password — we use the literal @anystring@ to match the public docs.
mkAuthHeader :: MailchimpApiKey -> Text
mkAuthHeader (MailchimpApiKey key) =
  "Basic " <> Text.Encoding.decodeUtf8 (Base64.encode ("anystring:" <> key))

-- | Comma-separated list of fields the reconcile job needs back.
--
-- Trimming the response cuts payload size and avoids decoding fields we
-- never use.
defaultFields :: Text
defaultFields =
  "members.id,members.email_address,members.status,members.last_changed,total_items"

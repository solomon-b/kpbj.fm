-- | Core Mailchimp Marketing API domain types.
--
-- Types in this module model Mailchimp API objects with snake_case JSON
-- encoding. Only the fields used by this client are decoded; Mailchimp's
-- responses include many more.
module Mailchimp.Types
  ( -- * Member Status
    MemberStatus (..),
    memberStatusTag,

    -- * Members
    UpsertMemberBody (..),
    Member (..),
    ListMembersResponse (..),

    -- * Hashing
    subscriberHash,
  )
where

--------------------------------------------------------------------------------

import Crypto.Hash (Digest, MD5, hash)
import Data.Aeson (FromJSON (..), ToJSON (..))
import Data.Aeson qualified as Aeson
import Data.ByteArray.Encoding (Base (..), convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime)
import Domain.Types.EmailAddress (EmailAddress)
import GHC.Generics (Generic)

--------------------------------------------------------------------------------
-- Member Status

-- | The full set of statuses Mailchimp can report for an audience member.
--
-- Mailchimp encodes each value as a lowercase tag.
data MemberStatus
  = Subscribed
  | Unsubscribed
  | Cleaned
  | Pending
  | Transactional
  | Archived
  deriving stock (Show, Eq, Generic)

instance ToJSON MemberStatus where
  toJSON = Aeson.String . memberStatusTag

-- | The lowercase string tag for a 'MemberStatus'.
--
-- Used both as the JSON encoding (Mailchimp's API) and as the value
-- stored in the @newsletter_subscribers.mailchimp_status@ column. Keeping
-- one mapping prevents the two consumers from drifting out of sync.
memberStatusTag :: MemberStatus -> Text
memberStatusTag = \case
  Subscribed -> "subscribed"
  Unsubscribed -> "unsubscribed"
  Cleaned -> "cleaned"
  Pending -> "pending"
  Transactional -> "transactional"
  Archived -> "archived"

instance FromJSON MemberStatus where
  parseJSON = Aeson.withText "MemberStatus" $ \case
    "subscribed" -> pure Subscribed
    "unsubscribed" -> pure Unsubscribed
    "cleaned" -> pure Cleaned
    "pending" -> pure Pending
    "transactional" -> pure Transactional
    "archived" -> pure Archived
    other -> fail $ "Unknown Mailchimp member status: " <> Text.unpack other

--------------------------------------------------------------------------------
-- Members

-- | Body for @PUT /lists/{list_id}/members/{subscriber_hash}@.
--
-- Uses @status_if_new@ rather than @status@: if the member already exists
-- and is not currently @Subscribed@, Mailchimp will reject @status:
-- "subscribed"@ with a 400. @status_if_new@ is the documented upsert-safe
-- field — it only takes effect when creating a new member.
data UpsertMemberBody = UpsertMemberBody
  { umbEmailAddress :: EmailAddress,
    umbStatusIfNew :: MemberStatus
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON UpsertMemberBody where
  toJSON umb =
    Aeson.object
      [ "email_address" Aeson..= umb.umbEmailAddress,
        "status_if_new" Aeson..= umb.umbStatusIfNew
      ]

-- | A Mailchimp audience member.
--
-- @mId@ is the @subscriber_hash@ — Mailchimp's own primary key for the row.
-- We persist it locally so reconcile and webhook handlers can match
-- against the same identifier Mailchimp uses.
data Member = Member
  { mId :: Text,
    mEmailAddress :: EmailAddress,
    mStatus :: MemberStatus,
    mLastChanged :: UTCTime
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Member where
  parseJSON = Aeson.withObject "Member" $ \o ->
    Member
      <$> o Aeson..: "id"
      <*> o Aeson..: "email_address"
      <*> o Aeson..: "status"
      <*> o Aeson..: "last_changed"

instance ToJSON Member where
  toJSON m =
    Aeson.object
      [ "id" Aeson..= m.mId,
        "email_address" Aeson..= m.mEmailAddress,
        "status" Aeson..= m.mStatus,
        "last_changed" Aeson..= m.mLastChanged
      ]

-- | Paginated response for @GET /lists/{list_id}/members@.
--
-- Mailchimp returns @total_items@ alongside the page so the caller can
-- drive pagination.
data ListMembersResponse = ListMembersResponse
  { lmrMembers :: [Member],
    lmrTotalItems :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ListMembersResponse where
  parseJSON = Aeson.withObject "ListMembersResponse" $ \o ->
    ListMembersResponse
      <$> o Aeson..: "members"
      <*> o Aeson..: "total_items"

instance ToJSON ListMembersResponse where
  toJSON r =
    Aeson.object
      [ "members" Aeson..= r.lmrMembers,
        "total_items" Aeson..= r.lmrTotalItems
      ]

--------------------------------------------------------------------------------
-- Hashing

-- | MD5 digest of the lowercase email address, hex-encoded.
--
-- Mailchimp calls this the "subscriber hash" and uses it as the path
-- parameter on every member-scoped endpoint.
subscriberHash :: EmailAddress -> Text
subscriberHash email =
  let lower = Text.toLower (display email)
      bytes = Text.Encoding.encodeUtf8 lower
      digest = hash bytes :: Digest MD5
      hexBytes = convertToBase Base16 digest :: ByteString
   in Text.Encoding.decodeUtf8 hexBytes

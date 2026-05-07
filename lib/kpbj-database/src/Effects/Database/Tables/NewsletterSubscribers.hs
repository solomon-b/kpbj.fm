{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @newsletter_subscribers@.
--
-- Backs the homepage newsletter signup form. Duplicate emails are silently
-- swallowed via @ON CONFLICT (email) DO NOTHING@.
--
-- The @mailchimp_*@ columns track sync state with Mailchimp's audience.
-- Postgres is the post-bootstrap source of truth: outbound mutations write
-- here first, then fire async Mailchimp updates that flip the status from
-- @\'pending\'@ to @\'subscribed\'@ (or @\'error\'@) once they complete.
module Effects.Database.Tables.NewsletterSubscribers
  ( -- * Id Type
    Id (..),

    -- * Model
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Queries
    insert,
    countByEmail,
    getPaginated,
    countAll,
    getById,
    getByEmail,
    deleteById,
    updateEmail,
    markSynced,
    markError,
    markCleaned,
    streamAll,
    upsertFromMailchimp,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.EmailAddress (EmailAddress)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue, EncodeValue, OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for newsletter subscriber primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype
    ( Show,
      Eq,
      Ord,
      Num,
      Servant.FromHttpApiData,
      Servant.ToHttpApiData,
      ToJSON,
      FromJSON,
      Display,
      DecodeValue,
      EncodeValue
    )

--------------------------------------------------------------------------------
-- Model

-- | Database Model for a row in @newsletter_subscribers@.
data Model = Model
  { mId :: Id,
    mEmail :: EmailAddress,
    mCreatedAt :: UTCTime,
    mMailchimpMemberId :: Maybe Text,
    mMailchimpStatus :: Maybe Text,
    mMailchimpSyncedAt :: Maybe UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

instance Display Model where
  displayBuilder m = displayBuilder (mId m) <> " - " <> displayBuilder (mEmail m)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert payload for adding a newsletter subscriber.
newtype Insert = Insert
  { nsiEmail :: EmailAddress
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Insert a subscriber with @mailchimp_status = \'pending\'@.
--
-- Returns @Just id@ on a fresh insert, @Nothing@ when the email already
-- exists (silently swallowed via @ON CONFLICT DO NOTHING@). Every
-- application-side insert path means "we just got a new subscriber", so the
-- pending status is hardcoded here rather than threaded through the 'Insert'
-- payload.
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  fmap listToMaybe $
    interp
      False
      [sql|
      INSERT INTO newsletter_subscribers (email, mailchimp_status)
      VALUES (#{nsiEmail}, 'pending')
      ON CONFLICT (email) DO NOTHING
      RETURNING id
    |]

-- | Count subscribers with the given email.
--
-- Used by tests to assert the duplicate-insert behavior.
countByEmail :: EmailAddress -> Hasql.Statement () Int64
countByEmail email =
  let query =
        interp
          False
          [sql|
          SELECT COUNT(*)::INT8
          FROM newsletter_subscribers
          WHERE email = #{email}
        |]
   in maybe 0 getOneColumn <$> query

-- | Fetch a page of subscribers, optionally filtered by an email substring.
--
-- Search uses a case-insensitive @LIKE@ on the email; pass 'Nothing' to skip
-- the filter. Rows are ordered most-recently-subscribed first.
getPaginated :: Maybe Text -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPaginated mSearch limit offset =
  case mSearch of
    Nothing ->
      interp
        False
        [sql|
        SELECT id, email, created_at, mailchimp_member_id, mailchimp_status, mailchimp_synced_at
        FROM newsletter_subscribers
        ORDER BY created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    Just search ->
      interp
        False
        [sql|
        SELECT id, email, created_at, mailchimp_member_id, mailchimp_status, mailchimp_synced_at
        FROM newsletter_subscribers
        WHERE email ILIKE '%' || #{search} || '%'
        ORDER BY created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]

-- | Count all subscribers, optionally filtered by an email substring.
--
-- Mirrors the filter used by 'getPaginated' so callers can compute total
-- result sets and pagination state.
countAll :: Maybe Text -> Hasql.Statement () Int64
countAll mSearch =
  let query = case mSearch of
        Nothing ->
          interp
            False
            [sql|
            SELECT COUNT(*)::INT8
            FROM newsletter_subscribers
          |]
        Just search ->
          interp
            False
            [sql|
            SELECT COUNT(*)::INT8
            FROM newsletter_subscribers
            WHERE email ILIKE '%' || #{search} || '%'
          |]
   in maybe 0 getOneColumn <$> query

-- | Look up a single subscriber by primary key.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById subId =
  fmap listToMaybe $
    interp
      False
      [sql|
      SELECT id, email, created_at, mailchimp_member_id, mailchimp_status, mailchimp_synced_at
      FROM newsletter_subscribers
      WHERE id = #{subId}
    |]

-- | Look up a single subscriber by email address (case-insensitive).
--
-- The webhook receiver uses this to translate Mailchimp's @data[email]@
-- payload back to a Postgres row. Mailchimp may normalize the casing
-- between when a user submits the form (e.g. @Foo\@Bar.com@) and when
-- webhooks are delivered (often lowercased), so the lookup folds case to
-- match either way.
--
-- The @email@ column does not yet have a functional unique index on
-- @LOWER(email)@, so this query is a sequential scan. With the small list
-- size (low thousands) the cost is negligible, but a follow-up migration
-- could replace the plain @UNIQUE(email)@ constraint with
-- @UNIQUE(LOWER(email))@ to make the lookup index-backed and the
-- uniqueness check case-folded as well.
getByEmail :: EmailAddress -> Hasql.Statement () (Maybe Model)
getByEmail email =
  fmap listToMaybe $
    interp
      False
      [sql|
      SELECT id, email, created_at, mailchimp_member_id, mailchimp_status, mailchimp_synced_at
      FROM newsletter_subscribers
      WHERE LOWER(email) = LOWER(#{email})
    |]

-- | Delete a subscriber by primary key, returning the deleted row's id and email.
--
-- The email is needed by the outbound Mailchimp archive call: by the time
-- the archive is dispatched the row is gone, so we capture the address at
-- delete time. Returns @Nothing@ when nothing matched.
deleteById :: Id -> Hasql.Statement () (Maybe (Id, EmailAddress))
deleteById subId =
  fmap listToMaybe $
    interp
      False
      [sql|
      DELETE FROM newsletter_subscribers
      WHERE id = #{subId}
      RETURNING id, email
    |]

-- | Replace the email address on an existing row.
--
-- The Mailchimp @upemail@ webhook fires when a subscriber updates their
-- address inside Mailchimp; we mirror the change locally and then dispatch
-- a fresh upsert against the new subscriber hash. Resets sync metadata so
-- the next push re-stamps the row from scratch.
updateEmail :: Id -> EmailAddress -> Hasql.Statement () ()
updateEmail subId email =
  interp
    False
    [sql|
      UPDATE newsletter_subscribers
      SET email = #{email},
          mailchimp_member_id = NULL,
          mailchimp_status = 'pending',
          mailchimp_synced_at = NULL
      WHERE id = #{subId}
    |]


-- | Stamp a row with the latest Mailchimp identity and status.
--
-- Called by the outbound dispatch worker after a successful upsert and by
-- the reconcile job after a successful pull. @synced_at@ is set to @NOW()@.
markSynced :: Id -> Text -> Text -> Hasql.Statement () ()
markSynced subId memberId status =
  interp
    False
    [sql|
      UPDATE newsletter_subscribers
      SET mailchimp_member_id = #{memberId},
          mailchimp_status = #{status},
          mailchimp_synced_at = NOW()
      WHERE id = #{subId}
    |]

-- | Mark a row as having failed its last Mailchimp call.
--
-- Leaves @synced_at@ untouched so the reconcile job can spot the row by its
-- stale timestamp. The daily reconcile retries the operation.
markError :: Id -> Hasql.Statement () ()
markError subId =
  interp
    False
    [sql|
      UPDATE newsletter_subscribers
      SET mailchimp_status = 'error'
      WHERE id = #{subId}
    |]

-- | Mark a row as cleaned (a hard bounce or persistent delivery failure on
-- Mailchimp's side).
--
-- Unlike unsubscribes, cleaned addresses cannot be re-subscribed via the
-- Mailchimp API — Mailchimp considers them undeliverable. We tombstone the
-- row instead of deleting it so a subsequent homepage signup hits the
-- existing row (no new outbound dispatch fires) and the admin dashboard can
-- surface the cleaned status. The row is never reused for outbound; only
-- admin-initiated deletion clears it.
markCleaned :: Id -> Hasql.Statement () ()
markCleaned subId =
  interp
    False
    [sql|
      UPDATE newsletter_subscribers
      SET mailchimp_status = 'cleaned',
          mailchimp_synced_at = NOW()
      WHERE id = #{subId}
    |]

-- | Stream every subscriber row in id order.
--
-- Used by the reconcile job to diff Postgres against Mailchimp. The list
-- size is small enough (low thousands at most) to comfortably fit in memory.
streamAll :: Hasql.Statement () [Model]
streamAll =
  interp
    False
    [sql|
      SELECT id, email, created_at, mailchimp_member_id, mailchimp_status, mailchimp_synced_at
      FROM newsletter_subscribers
      ORDER BY id ASC
    |]

-- | Insert a row sourced from a Mailchimp pull, populating sync columns.
--
-- Used only by the bootstrap mode of the reconcile job. Sets @member_id@
-- and @status@ from the Mailchimp side and stamps @synced_at@. On conflict
-- with an existing email, leaves the existing row alone — the local row is
-- already authoritative for that subscriber.
upsertFromMailchimp ::
  EmailAddress ->
  Text ->
  Text ->
  Hasql.Statement () (Maybe Id)
upsertFromMailchimp email memberId status =
  fmap listToMaybe $
    interp
      False
      [sql|
      INSERT INTO newsletter_subscribers
        (email, mailchimp_member_id, mailchimp_status, mailchimp_synced_at)
      VALUES
        (#{email}, #{memberId}, #{status}, NOW())
      ON CONFLICT (email) DO NOTHING
      RETURNING id
    |]

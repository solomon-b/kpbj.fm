{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @newsletter_subscribers@.
--
-- Backs the homepage newsletter signup form. Duplicate emails are silently
-- swallowed via @ON CONFLICT (email) DO NOTHING@.
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
    deleteById,
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
    mCreatedAt :: UTCTime
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

-- | Insert a subscriber.
--
-- Returns @Just id@ on a fresh insert, @Nothing@ when the email already
-- exists (silently swallowed via @ON CONFLICT DO NOTHING@).
insert :: Insert -> Hasql.Statement () (Maybe Id)
insert Insert {..} =
  fmap listToMaybe $
    interp
      False
      [sql|
      INSERT INTO newsletter_subscribers (email)
      VALUES (#{nsiEmail})
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
        SELECT id, email, created_at
        FROM newsletter_subscribers
        ORDER BY created_at DESC
        LIMIT #{limit} OFFSET #{offset}
      |]
    Just search ->
      interp
        False
        [sql|
        SELECT id, email, created_at
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
      SELECT id, email, created_at
      FROM newsletter_subscribers
      WHERE id = #{subId}
    |]

-- | Delete a subscriber by primary key.
--
-- Returns @Just id@ when a row was deleted, @Nothing@ if nothing matched.
deleteById :: Id -> Hasql.Statement () (Maybe Id)
deleteById subId =
  fmap listToMaybe $
    interp
      False
      [sql|
      DELETE FROM newsletter_subscribers
      WHERE id = #{subId}
      RETURNING id
    |]

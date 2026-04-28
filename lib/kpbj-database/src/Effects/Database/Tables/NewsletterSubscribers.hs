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
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
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

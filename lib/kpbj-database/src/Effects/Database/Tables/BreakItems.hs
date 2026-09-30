{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @break_items@.
--
-- A break item is a PSA or an underwriting announcement. Both fill breaks,
-- which are the two minutes before a half-hour boundary. KPBJ holds a
-- noncommercial license, so paid spots are underwriting, not advertising.
--
-- One table holds both categories. The dashboard splits them into two sections
-- so each can carry its own permission gate. The daily planner draws from both.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.BreakItems
  ( -- * Id Type
    Id (..),

    -- * Category
    Category (..),

    -- * Table Definition
    BreakItem (..),
    breakItemSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getByCategory,
    getById,
    getByIdsIncludingDeleted,
    getActiveOnDay,
    getActiveInMonth,
    insertBreakItem,
    updateBreakItem,
    softDeleteBreakItem,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (Day, UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Enum, Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for break item primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Category

-- | Which dashboard section owns a break item.
--
-- A PSA fills unsold time. An underwriting announcement is sold per month, and
-- the planner spreads its airings across the month's show breaks.
data Category
  = Psa
  | Underwriting
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

instance DBType Category where
  typeInformation =
    parseTypeInformation
      ( \case
          "psa" -> Right Psa
          "underwriting" -> Right Underwriting
          other -> Left $ "Invalid Category: " <> Text.unpack other
      )
      ( \case
          Psa -> "psa"
          Underwriting -> "underwriting"
      )
      typeInformation

instance DBEq Category

instance DecodeValue Category where
  decodeValue = Decoders.enum $ \case
    "psa" -> Just Psa
    "underwriting" -> Just Underwriting
    _ -> Nothing

instance EncodeValue Category where
  encodeValue = Encoders.enum $ \case
    Psa -> "psa"
    Underwriting -> "underwriting"

instance Display Category where
  displayBuilder = \case
    Psa -> "PSA"
    Underwriting -> "Underwriting"

--------------------------------------------------------------------------------
-- Table Definition

-- | The @break_items@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data BreakItem f = BreakItem
  { bimId :: Column f Id,
    bimTitle :: Column f Text,
    bimCategory :: Column f Category,
    -- | The underwriter who pays for this announcement. Nothing for a PSA.
    bimUnderwriterId :: Column f (Maybe Underwriters.Id),
    -- | Airings sold per Pacific calendar month. A partial month owes its
    -- share, rounded up. Nothing for a PSA.
    bimSpotsPerMonth :: Column f (Maybe Int64),
    bimAudioFilePath :: Column f Text,
    bimMimeType :: Column f Text,
    bimFileSize :: Column f Int64,
    bimDurationSeconds :: Column f Int64,
    bimStartsOn :: Column f Day,
    bimEndsOn :: Column f (Maybe Day),
    bimCreatorId :: Column f User.Id,
    bimCreatedAt :: Column f UTCTime,
    bimUpdatedAt :: Column f UTCTime,
    bimDeletedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (BreakItem f)

deriving stock instance (f ~ Result) => Eq (BreakItem f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (BreakItem Result)

-- | Display instance for BreakItem Result.
instance Display (BreakItem Result) where
  displayBuilder m = displayBuilder (bimId m) <> " - " <> displayBuilder (bimTitle m)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @BreakItem Result@.
type Model = BreakItem Result

-- | Table schema connecting the Haskell type to the database table.
breakItemSchema :: TableSchema (BreakItem Name)
breakItemSchema =
  TableSchema
    { name = "break_items",
      columns =
        BreakItem
          { bimId = "id",
            bimTitle = "title",
            bimCategory = "category",
            bimUnderwriterId = "underwriter_id",
            bimSpotsPerMonth = "spots_per_month",
            bimAudioFilePath = "audio_file_path",
            bimMimeType = "mime_type",
            bimFileSize = "file_size",
            bimDurationSeconds = "duration_seconds",
            bimStartsOn = "starts_on",
            bimEndsOn = "ends_on",
            bimCreatorId = "creator_id",
            bimCreatedAt = "created_at",
            bimUpdatedAt = "updated_at",
            bimDeletedAt = "deleted_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new break items.
data Insert = Insert
  { biiTitle :: Text,
    biiCategory :: Category,
    biiUnderwriterId :: Maybe Underwriters.Id,
    biiSpotsPerMonth :: Maybe Int64,
    biiAudioFilePath :: Text,
    biiMimeType :: Text,
    biiFileSize :: Int64,
    biiDurationSeconds :: Int64,
    biiStartsOn :: Day,
    biiEndsOn :: Maybe Day,
    biiCreatorId :: User.Id
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- Every raw query below selects the columns in BreakItem field order. The
-- generic DecodeRow instance reads them positionally, so a reordered SELECT
-- decodes into the wrong fields rather than failing.

-- | Get one page of break items in a category, newest first.
--
-- Skips soft-deleted rows.
getByCategory :: Category -> Limit -> Offset -> Hasql.Statement () [Model]
getByCategory category (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, title, category, underwriter_id, spots_per_month, audio_file_path, mime_type,
           file_size, duration_seconds, starts_on, ends_on, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE deleted_at IS NULL
      AND category = #{category}
    ORDER BY created_at DESC, id DESC
    LIMIT #{lim}
    OFFSET #{off}
  |]

-- | Get a break item by its ID.
--
-- Returns soft-deleted rows as Nothing. A caller that holds an ID from a list
-- page cannot then act on a row another staff member deleted meanwhile.
getById :: Id -> Hasql.Statement () (Maybe Model)
getById breakItemId =
  listToMaybe
    <$> interp
      False
      [sql|
      SELECT id, title, category, underwriter_id, spots_per_month, audio_file_path, mime_type,
             file_size, duration_seconds, starts_on, ends_on, creator_id, created_at,
             updated_at, deleted_at
      FROM break_items
      WHERE id = #{breakItemId}
        AND deleted_at IS NULL
    |]

-- | Get break items by ID, deleted ones included.
--
-- The delivery report needs the title of every item that aired, and an item
-- that aired can be deleted later.
getByIdsIncludingDeleted :: [Id] -> Hasql.Statement () [Model]
getByIdsIncludingDeleted breakItemIds =
  interp
    False
    [sql|
    SELECT id, title, category, underwriter_id, spots_per_month, audio_file_path, mime_type,
           file_size, duration_seconds, starts_on, ends_on, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE id = ANY (#{breakItemIds})
    ORDER BY id
  |]

-- | Live break items whose air dates cover this Pacific day.
getActiveOnDay :: Day -> Hasql.Statement () [Model]
getActiveOnDay day =
  interp
    False
    [sql|
    SELECT id, title, category, underwriter_id, spots_per_month, audio_file_path, mime_type,
           file_size, duration_seconds, starts_on, ends_on, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE deleted_at IS NULL
      AND starts_on <= #{day}
      AND (ends_on IS NULL OR ends_on >= #{day})
    ORDER BY id
  |]

-- | Live break items active on at least one day in the inclusive range.
getActiveInMonth :: Day -> Day -> Hasql.Statement () [Model]
getActiveInMonth firstDay lastDay =
  interp
    False
    [sql|
    SELECT id, title, category, underwriter_id, spots_per_month, audio_file_path, mime_type,
           file_size, duration_seconds, starts_on, ends_on, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE deleted_at IS NULL
      AND starts_on <= #{lastDay}
      AND (ends_on IS NULL OR ends_on >= #{firstDay})
    ORDER BY id
  |]

-- | Insert a new break item and return its ID.
insertBreakItem :: Insert -> Hasql.Statement () (Maybe Id)
insertBreakItem Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = breakItemSchema,
            rows =
              values
                [ BreakItem
                    { bimId = nextId "break_items_id_seq",
                      bimTitle = lit biiTitle,
                      bimCategory = lit biiCategory,
                      bimUnderwriterId = lit biiUnderwriterId,
                      bimSpotsPerMonth = lit biiSpotsPerMonth,
                      bimAudioFilePath = lit biiAudioFilePath,
                      bimMimeType = lit biiMimeType,
                      bimFileSize = lit biiFileSize,
                      bimDurationSeconds = lit biiDurationSeconds,
                      bimStartsOn = lit biiStartsOn,
                      bimEndsOn = lit biiEndsOn,
                      bimCreatorId = lit biiCreatorId,
                      bimCreatedAt = now,
                      bimUpdatedAt = now,
                      bimDeletedAt = lit Nothing
                    }
                ],
            onConflict = Abort,
            returning = Returning bimId
          }

-- | Update the editable fields of a break item.
--
-- The category is not editable. An item moves between the two dashboard
-- sections only by being deleted and uploaded again, so a stale link cannot
-- silently change which permission gate guards a row.
--
-- The audio is not editable either, so the columns describing it are absent. A
-- new recording is a new row.
--
-- Returns the updated row, or Nothing when the ID names no live row.
updateBreakItem ::
  Id ->
  -- | Title
  Text ->
  -- | First air date, inclusive
  Day ->
  -- | Last air date, inclusive. Nothing runs open ended
  Maybe Day ->
  -- | Underwriter. Nothing for a PSA
  Maybe Underwriters.Id ->
  -- | Spots per month. Nothing for a PSA
  Maybe Int64 ->
  Hasql.Statement () (Maybe Model)
updateBreakItem breakItemId newTitle newStartsOn newEndsOn newUnderwriterId newSpots =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE break_items
      SET title = #{newTitle},
          starts_on = #{newStartsOn},
          ends_on = #{newEndsOn},
          underwriter_id = #{newUnderwriterId},
          spots_per_month = #{newSpots},
          updated_at = NOW()
      WHERE id = #{breakItemId}
        AND deleted_at IS NULL
      RETURNING id, title, category, underwriter_id, spots_per_month, audio_file_path,
                mime_type, file_size, duration_seconds, starts_on, ends_on, creator_id,
                created_at, updated_at, deleted_at
    |]

-- | Soft delete a break item.
--
-- The row stops airing at once, because the break endpoint skips deleted items
-- in the stored plan. It keeps its playback history, so the delivery report
-- over past dates stays complete.
--
-- Returns the ID if a live row was deleted, Nothing otherwise.
softDeleteBreakItem :: Id -> Hasql.Statement () (Maybe Id)
softDeleteBreakItem breakItemId =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE break_items
      SET deleted_at = NOW(), updated_at = NOW()
      WHERE id = #{breakItemId}
        AND deleted_at IS NULL
      RETURNING id
    |]

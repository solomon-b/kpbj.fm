{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @break_items@.
--
-- A break item is a PSA or an advertisement spot. Both fill the break window,
-- which is the last two minutes of a scheduled show slot and the last two
-- minutes of an hour that no slot spans.
--
-- One table holds both categories. The dashboard splits them into two sections
-- so each can carry its own permission gate, but the playout rotation draws
-- from both at once through 'getEligibleForBreak'.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.BreakItems
  ( -- * Id Type
    Id (..),

    -- * Category
    Category (..),
    categoryToText,
    parseCategory,

    -- * Table Definition
    BreakItem (..),
    breakItemSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getByCategory,
    countByCategory,
    getById,
    getEligibleForBreak,
    insertBreakItem,
    updateBreakItem,
    softDeleteBreakItem,
    markPlayed,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import Data.Time (Day, UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
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
-- The two categories share one table, one rotation, and one break window. They
-- differ only in which dashboard route reaches them and which permission gate
-- that route applies.
data Category
  = Psa
  | Advertisement
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

instance DBType Category where
  typeInformation =
    parseTypeInformation
      ( \case
          "psa" -> Right Psa
          "advertisement" -> Right Advertisement
          other -> Left $ "Invalid Category: " <> Text.unpack other
      )
      ( \case
          Psa -> "psa"
          Advertisement -> "advertisement"
      )
      typeInformation

instance DBEq Category

instance DecodeValue Category where
  decodeValue = Decoders.enum $ \case
    "psa" -> Just Psa
    "advertisement" -> Just Advertisement
    _ -> Nothing

instance EncodeValue Category where
  encodeValue = Encoders.enum $ \case
    Psa -> "psa"
    Advertisement -> "advertisement"

instance Display Category where
  displayBuilder = \case
    Psa -> "PSA"
    Advertisement -> "Advertisement"

-- | Convert a category to its human-readable label.
categoryToText :: Category -> Text
categoryToText = \case
  Psa -> "PSA"
  Advertisement -> "Advertisement"

-- | Parse a human-readable label into a category.
parseCategory :: Text -> Maybe Category
parseCategory = \case
  "PSA" -> Just Psa
  "Advertisement" -> Just Advertisement
  _ -> Nothing

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
    bimAudioFilePath :: Column f Text,
    bimMimeType :: Column f Text,
    bimFileSize :: Column f Int64,
    bimDurationSeconds :: Column f Int64,
    bimStartsOn :: Column f Day,
    bimEndsOn :: Column f (Maybe Day),
    bimPriority :: Column f Int64,
    bimLastPlayedAt :: Column f (Maybe UTCTime),
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
            bimAudioFilePath = "audio_file_path",
            bimMimeType = "mime_type",
            bimFileSize = "file_size",
            bimDurationSeconds = "duration_seconds",
            bimStartsOn = "starts_on",
            bimEndsOn = "ends_on",
            bimPriority = "priority",
            bimLastPlayedAt = "last_played_at",
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
    biiAudioFilePath :: Text,
    biiMimeType :: Text,
    biiFileSize :: Int64,
    biiDurationSeconds :: Int64,
    biiStartsOn :: Day,
    biiEndsOn :: Maybe Day,
    biiPriority :: Int64,
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
    SELECT id, title, category, audio_file_path, mime_type, file_size, duration_seconds,
           starts_on, ends_on, priority, last_played_at, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE deleted_at IS NULL
      AND category = #{category}
    ORDER BY created_at DESC
    LIMIT #{lim}
    OFFSET #{off}
  |]

-- | Count the live break items in a category.
countByCategory :: Category -> Hasql.Statement () Int64
countByCategory category =
  let query =
        interp
          False
          [sql|
          SELECT COUNT(*)::INT8
          FROM break_items
          WHERE deleted_at IS NULL
            AND category = #{category}
        |]
   in maybe 0 getOneColumn <$> query

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
      SELECT id, title, category, audio_file_path, mime_type, file_size, duration_seconds,
             starts_on, ends_on, priority, last_played_at, creator_id, created_at,
             updated_at, deleted_at
      FROM break_items
      WHERE id = #{breakItemId}
        AND deleted_at IS NULL
    |]

-- | Candidates for one break window, already filtered to what could fit.
--
-- Takes the Pacific date the break airs on and the number of seconds left in
-- the window after the station ID. Returns both categories together, ordered so
-- the caller can fill the window by walking the list from the front:
--
-- * higher @priority@ first, so a paid spot outranks a filler PSA
-- * then least recently played, so the rotation advances
-- * then @id@, so the order is stable when two rows tie on both
--
-- An item longer than the whole budget is dropped here rather than in the
-- caller, so a single long item cannot hide every item behind it.
--
-- The caller still has to walk the result, because an item that fits the budget
-- may not fit the remainder once earlier items have taken their share.
getEligibleForBreak :: Day -> Int64 -> Hasql.Statement () [Model]
getEligibleForBreak airDate budgetSeconds =
  interp
    False
    [sql|
    SELECT id, title, category, audio_file_path, mime_type, file_size, duration_seconds,
           starts_on, ends_on, priority, last_played_at, creator_id, created_at,
           updated_at, deleted_at
    FROM break_items
    WHERE deleted_at IS NULL
      AND starts_on <= #{airDate}
      AND (ends_on IS NULL OR ends_on >= #{airDate})
      AND duration_seconds <= #{budgetSeconds}
    ORDER BY priority DESC, last_played_at ASC NULLS FIRST, id ASC
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
                      bimAudioFilePath = lit biiAudioFilePath,
                      bimMimeType = lit biiMimeType,
                      bimFileSize = lit biiFileSize,
                      bimDurationSeconds = lit biiDurationSeconds,
                      bimStartsOn = lit biiStartsOn,
                      bimEndsOn = lit biiEndsOn,
                      bimPriority = lit biiPriority,
                      bimLastPlayedAt = lit Nothing,
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
-- Returns the updated row, or Nothing when the ID names no live row.
updateBreakItem ::
  Id ->
  -- | Title
  Text ->
  -- | Audio file path
  Text ->
  -- | MIME type
  Text ->
  -- | File size in bytes
  Int64 ->
  -- | Duration in seconds
  Int64 ->
  -- | First air date, inclusive
  Day ->
  -- | Last air date, inclusive. Nothing runs open ended
  Maybe Day ->
  -- | Priority
  Int64 ->
  Hasql.Statement () (Maybe Model)
updateBreakItem breakItemId newTitle newAudioFilePath newMimeType newFileSize newDuration newStartsOn newEndsOn newPriority =
  listToMaybe
    <$> interp
      False
      [sql|
      UPDATE break_items
      SET title = #{newTitle},
          audio_file_path = #{newAudioFilePath},
          mime_type = #{newMimeType},
          file_size = #{newFileSize},
          duration_seconds = #{newDuration},
          starts_on = #{newStartsOn},
          ends_on = #{newEndsOn},
          priority = #{newPriority},
          updated_at = NOW()
      WHERE id = #{breakItemId}
        AND deleted_at IS NULL
      RETURNING id, title, category, audio_file_path, mime_type, file_size, duration_seconds,
                starts_on, ends_on, priority, last_played_at, creator_id, created_at,
                updated_at, deleted_at
    |]

-- | Soft delete a break item.
--
-- The row leaves the rotation and both dashboard lists but keeps its playback
-- history, so an airplay report over past dates stays complete.
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

-- | Record that these items were handed to the playout for one break window.
--
-- This stamps the moment the API chose the items, not the moment they aired.
-- The two differ by at most the length of one window, and using the choice
-- keeps the rotation advancing even when the @\/played@ callback lags or never
-- arrives.
markPlayed :: NonEmpty Id -> Hasql.Statement () ()
markPlayed itemIds =
  interp
    False
    [sql|
    UPDATE break_items
    SET last_played_at = NOW()
    WHERE id = ANY (#{NonEmpty.toList itemIds})
  |]

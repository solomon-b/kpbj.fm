{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @site_page_revisions@.
--
-- Stores complete content snapshots for site page revision history.
-- Each revision captures the full content at a point in time, enabling
-- easy diff computation and rollback functionality.
--
-- Uses rel8 for type-safe database queries where possible.
module Effects.Database.Tables.SitePageRevisions
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    SitePageRevision (..),
    sitePageRevisionSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Result Types
    RevisionWithEditor (..),

    -- * Queries
    getRevisionsForPage,
    getRevisionById,
    insertRevision,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import OrphanInstances.UTCTime ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for site page revision primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, Display, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @site_page_revisions@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data SitePageRevision f = SitePageRevision
  { sprId :: Column f Id,
    sprPageId :: Column f SitePages.Id,
    sprContent :: Column f Text,
    sprEditSummary :: Column f (Maybe Text),
    sprCreatedAt :: Column f UTCTime,
    sprCreatedBy :: Column f User.Id
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (SitePageRevision f)

deriving stock instance (f ~ Result) => Eq (SitePageRevision f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (SitePageRevision Result)

-- | Display instance for SitePageRevision Result.
deriving via (RecordInstance (SitePageRevision Result)) instance Display (SitePageRevision Result)

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @SitePageRevision Result@.
type Model = SitePageRevision Result

-- | Table schema connecting the Haskell type to the database table.
sitePageRevisionSchema :: TableSchema (SitePageRevision Name)
sitePageRevisionSchema =
  TableSchema
    { name = "site_page_revisions",
      columns =
        SitePageRevision
          { sprId = "id",
            sprPageId = "page_id",
            sprContent = "content",
            sprEditSummary = "edit_summary",
            sprCreatedAt = "created_at",
            sprCreatedBy = "created_by"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new revisions.
data Insert = Insert
  { spriPageId :: SitePages.Id,
    spriContent :: Text,
    spriEditSummary :: Maybe Text,
    spriCreatedBy :: User.Id
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Result Types

-- | Revision with editor display name for history display.
data RevisionWithEditor = RevisionWithEditor
  { rweRevision :: Model,
    rweEditorName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance RevisionWithEditor)

-- | Raw row type for decoding revision with editor from database.
data RevisionWithEditorRow = RevisionWithEditorRow
  { rwerId :: Id,
    rwerPageId :: SitePages.Id,
    rwerContent :: Text,
    rwerEditSummary :: Maybe Text,
    rwerCreatedAt :: UTCTime,
    rwerCreatedBy :: User.Id,
    rwerEditorName :: Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)

-- | Convert raw row to structured type.
rowToRevisionWithEditor :: RevisionWithEditorRow -> RevisionWithEditor
rowToRevisionWithEditor row =
  RevisionWithEditor
    { rweRevision =
        SitePageRevision
          { sprId = row.rwerId,
            sprPageId = row.rwerPageId,
            sprContent = row.rwerContent,
            sprEditSummary = row.rwerEditSummary,
            sprCreatedAt = row.rwerCreatedAt,
            sprCreatedBy = row.rwerCreatedBy
          },
      rweEditorName = row.rwerEditorName
    }

--------------------------------------------------------------------------------
-- Queries

-- | Get all revisions for a page, ordered by creation date (newest first).
--
-- Includes the editor's display name for each revision.
-- Orders by created_at DESC, then by id DESC for deterministic ordering.
getRevisionsForPage :: SitePages.Id -> Hasql.Statement () [RevisionWithEditor]
getRevisionsForPage pageId =
  map rowToRevisionWithEditor
    <$> interp
      False
      [sql|
    SELECT
      r.id, r.page_id, r.content, r.edit_summary, r.created_at, r.created_by,
      COALESCE(um.display_name, 'Unknown')
    FROM site_page_revisions r
    LEFT JOIN user_metadata um ON r.created_by = um.user_id
    WHERE r.page_id = #{pageId}
    ORDER BY r.created_at DESC, r.id DESC
  |]

-- | Get a specific revision by ID.
getRevisionById :: Id -> Hasql.Statement () (Maybe Model)
getRevisionById revisionId = fmap listToMaybe $ run $ select do
  row <- each sitePageRevisionSchema
  where_ $ sprId row ==. lit revisionId
  pure row

-- | Insert a new revision.
insertRevision :: Insert -> Hasql.Statement () (Maybe Id)
insertRevision Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = sitePageRevisionSchema,
            rows =
              values
                [ SitePageRevision
                    { sprId = nextId "site_page_revisions_id_seq",
                      sprPageId = lit spriPageId,
                      sprContent = lit spriContent,
                      sprEditSummary = lit spriEditSummary,
                      sprCreatedAt = now,
                      sprCreatedBy = lit spriCreatedBy
                    }
                ],
            onConflict = Abort,
            returning = Returning sprId
          }

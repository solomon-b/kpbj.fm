{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @site_page_revisions@.
--
-- Stores complete content snapshots for site page revision history.
-- Each revision captures the full content at a point in time, enabling
-- easy diff computation and rollback functionality.
module Effects.Database.Tables.SitePageRevisions
  ( -- * Id Type
    Id (..),

    -- * Model
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Result Types
    RevisionWithEditor (..),

    -- * Queries
    getRevisionsForPage,
    getRevisionById,
    getLatestRevision,
    insertRevision,
    countRevisionsForPage,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.SitePages qualified as SitePages
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for site page revision primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, Display)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

--------------------------------------------------------------------------------
-- Model

-- | Site page revision model representing a row from @site_page_revisions@.
data Model = Model
  { sprId :: Id,
    sprPageId :: SitePages.Id,
    sprContent :: Text,
    sprEditSummary :: Maybe Text,
    sprCreatedAt :: UTCTime,
    sprCreatedBy :: User.Id
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

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
        Model
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
getRevisionById revisionId =
  interp
    False
    [sql|
    SELECT id, page_id, content, edit_summary, created_at, created_by
    FROM site_page_revisions
    WHERE id = #{revisionId}
  |]

-- | Get the latest revision for a page.
--
-- Orders by created_at DESC, then by id DESC to handle cases where
-- multiple revisions have the same timestamp (e.g., within a transaction).
getLatestRevision :: SitePages.Id -> Hasql.Statement () (Maybe Model)
getLatestRevision pageId =
  interp
    False
    [sql|
    SELECT id, page_id, content, edit_summary, created_at, created_by
    FROM site_page_revisions
    WHERE page_id = #{pageId}
    ORDER BY created_at DESC, id DESC
    LIMIT 1
  |]

-- | Insert a new revision.
insertRevision :: Insert -> Hasql.Statement () (Maybe Id)
insertRevision Insert {..} =
  interp
    False
    [sql|
    INSERT INTO site_page_revisions (page_id, content, edit_summary, created_by)
    VALUES (#{spriPageId}, #{spriContent}, #{spriEditSummary}, #{spriCreatedBy})
    RETURNING id
  |]

-- | Count total revisions for a page.
countRevisionsForPage :: SitePages.Id -> Hasql.Statement () Int64
countRevisionsForPage pageId =
  let query =
        interp
          False
          [sql|
    SELECT COUNT(*)::bigint
    FROM site_page_revisions
    WHERE page_id = #{pageId}
  |]
   in maybe 0 getOneColumn <$> query

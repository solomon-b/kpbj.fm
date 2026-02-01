{-# LANGUAGE QuasiQuotes #-}

-- | Database table definition and queries for @site_pages@.
--
-- Stores dynamic site pages (About, Privacy Policy, Terms of Service) with
-- markdown content that can be edited by staff users through the dashboard.
module Effects.Database.Tables.SitePages
  ( -- * Id Type
    Id (..),

    -- * Model
    Model (..),

    -- * Insert Type
    Insert (..),

    -- * Update Type
    Update (..),

    -- * Queries
    getAllPages,
    getPageBySlug,
    updatePage,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for site page primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, Display)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

--------------------------------------------------------------------------------
-- Model

-- | Site page model representing a row from @site_pages@.
data Model = Model
  { spmId :: Id,
    spmSlug :: Text,
    spmTitle :: Text,
    spmContent :: Text,
    spmUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new site pages.
data Insert = Insert
  { spiSlug :: Text,
    spiTitle :: Text,
    spiContent :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Update Type

-- | Update type for modifying site pages.
data Update = Update
  { spuTitle :: Text,
    spuContent :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get all site pages ordered by slug.
getAllPages :: Hasql.Statement () [Model]
getAllPages =
  interp
    False
    [sql|
    SELECT id, slug, title, content, updated_at
    FROM site_pages
    ORDER BY slug
  |]

-- | Get a site page by slug.
getPageBySlug :: Text -> Hasql.Statement () (Maybe Model)
getPageBySlug slug =
  interp
    False
    [sql|
    SELECT id, slug, title, content, updated_at
    FROM site_pages
    WHERE slug = #{slug}
  |]

-- | Update a site page and return the updated model.
updatePage :: Id -> Update -> Hasql.Statement () (Maybe Model)
updatePage pageId Update {..} =
  interp
    False
    [sql|
    UPDATE site_pages
    SET title = #{spuTitle},
        content = #{spuContent},
        updated_at = NOW()
    WHERE id = #{pageId}
    RETURNING id, slug, title, content, updated_at
  |]

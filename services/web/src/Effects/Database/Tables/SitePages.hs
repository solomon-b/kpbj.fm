{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @site_pages@.
--
-- Stores dynamic site pages (About, Privacy Policy, Terms of Service) with
-- markdown content that can be edited by staff users through the dashboard.
module Effects.Database.Tables.SitePages
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    SitePage (..),
    sitePageSchema,

    -- * Model (Result alias)
    Model,

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

import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Rel8 hiding (Insert, Update)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for site page primary keys.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, Display, DBType, DBEq)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @site_pages@ table definition using rel8's higher-kinded data pattern.
data SitePage f = SitePage
  { spmId :: Column f Id,
    spmSlug :: Column f Text,
    spmTitle :: Column f Text,
    spmContent :: Column f Text,
    spmUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (SitePage f)

deriving stock instance (f ~ Result) => Eq (SitePage f)

instance DecodeRow (SitePage Result)

instance Display (SitePage Result) where
  displayBuilder page =
    "SitePage { id = "
      <> displayBuilder (spmId page)
      <> ", slug = "
      <> displayBuilder (spmSlug page)
      <> " }"

-- | Type alias for backwards compatibility.
type Model = SitePage Result

-- | Table schema connecting the Haskell type to the database table.
sitePageSchema :: TableSchema (SitePage Name)
sitePageSchema =
  TableSchema
    { name = "site_pages",
      columns =
        SitePage
          { spmId = "id",
            spmSlug = "slug",
            spmTitle = "title",
            spmContent = "content",
            spmUpdatedAt = "updated_at"
          }
    }

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
  run $
    select $
      orderBy (spmSlug >$< asc) do
        each sitePageSchema

-- | Get a site page by slug.
getPageBySlug :: Text -> Hasql.Statement () (Maybe Model)
getPageBySlug slug = fmap listToMaybe $ run $ select do
  page <- each sitePageSchema
  where_ $ spmSlug page ==. lit slug
  pure page

-- | Update a site page and return the updated model.
updatePage :: Id -> Update -> Hasql.Statement () (Maybe Model)
updatePage pageId Update {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = sitePageSchema,
            from = pure (),
            set = \_ page ->
              page
                { spmTitle = lit spuTitle,
                  spmContent = lit spuContent,
                  spmUpdatedAt = now
                },
            updateWhere = \_ page -> spmId page ==. lit pageId,
            returning = Returning Prelude.id
          }

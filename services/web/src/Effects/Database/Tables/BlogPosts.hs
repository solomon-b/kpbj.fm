{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @blog_posts@.
--
-- Uses rel8 for type-safe database queries.
module Effects.Database.Tables.BlogPosts
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    BlogPost (..),
    blogPostSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getAllBlogPosts,
    getPublishedBlogPosts,
    getBlogPostById,
    getBlogPostBySlug,
    insertBlogPost,
    updateBlogPost,
    deleteBlogPost,

    -- * Junction Table Queries (blog_post_tags)
    getTagsForPost,
    addTagToPost,
    removeTagFromPost,
    getPostsByTag,

    -- * Result Types
    BlogPostWithAuthor (..),
    BlogPostWithTags (..),
    BlogPostComplete (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Functor.Contravariant ((>$<))
import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..), RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Database.Tables.Util (nextId)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..))
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for blog post primary keys.
--
-- Provides type safety to prevent mixing up IDs from different tables.
newtype Id = Id {unId :: Int64}
  deriving stock (Generic)
  deriving anyclass (DecodeRow)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)
  deriving newtype (ToJSON, FromJSON, Display)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @blog_posts@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data BlogPost f = BlogPost
  { bpmId :: Column f Id,
    bpmTitle :: Column f Text,
    bpmSlug :: Column f Slug,
    bpmContent :: Column f Text,
    bpmExcerpt :: Column f (Maybe Text),
    bpmHeroImageUrl :: Column f (Maybe Text),
    bpmAuthorId :: Column f User.Id,
    bpmStatus :: Column f BlogPostStatus,
    bpmPublishedAt :: Column f (Maybe UTCTime),
    bpmCreatedAt :: Column f UTCTime,
    bpmUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (BlogPost f)

deriving stock instance (f ~ Result) => Eq (BlogPost f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (BlogPost Result)

-- | Display instance for BlogPost Result.
instance Display (BlogPost Result) where
  displayBuilder post =
    "BlogPost { id = "
      <> displayBuilder (bpmId post)
      <> ", title = "
      <> displayBuilder (bpmTitle post)
      <> ", slug = "
      <> displayBuilder (bpmSlug post)
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @BlogPost Result@.
type Model = BlogPost Result

-- | Table schema connecting the Haskell type to the database table.
blogPostSchema :: TableSchema (BlogPost Name)
blogPostSchema =
  TableSchema
    { name = "blog_posts",
      columns =
        BlogPost
          { bpmId = "id",
            bpmTitle = "title",
            bpmSlug = "slug",
            bpmContent = "content",
            bpmExcerpt = "excerpt",
            bpmHeroImageUrl = "hero_image_url",
            bpmAuthorId = "author_id",
            bpmStatus = "status",
            bpmPublishedAt = "published_at",
            bpmCreatedAt = "created_at",
            bpmUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Result Types

-- | Blog post with author information.
data BlogPostWithAuthor = BlogPostWithAuthor
  { bpwaPost :: Model,
    bpwaAuthor :: UserMetadata.Model
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithAuthor)

-- | Blog post with tags.
data BlogPostWithTags = BlogPostWithTags
  { bpwtPost :: Model,
    bpwtTags :: [BlogTags.Model]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithTags)

-- | Blog post with complete information (author + tags).
data BlogPostComplete = BlogPostComplete
  { bpcPost :: Model,
    bpcAuthor :: UserMetadata.Model,
    bpcTags :: [BlogTags.Model]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostComplete)

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new blog posts.
data Insert = Insert
  { bpiTitle :: Text,
    bpiSlug :: Slug,
    bpiContent :: Text,
    bpiExcerpt :: Maybe Text,
    bpiHeroImageUrl :: Maybe Text,
    bpiAuthorId :: User.Id,
    bpiStatus :: BlogPostStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Queries

-- | Get all blog posts (any status) ordered by creation date.
--
-- Used for admin dashboard to see all posts including drafts and archived.
getAllBlogPosts :: Limit -> Offset -> Hasql.Statement () [Model]
getAllBlogPosts (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (bpmCreatedAt >$< desc) do
            each blogPostSchema

-- | Get all published blog posts ordered by publication date.
getPublishedBlogPosts :: Limit -> Offset -> Hasql.Statement () [Model]
getPublishedBlogPosts (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (bpmPublishedAt >$< nullsLast desc) do
            post <- each blogPostSchema
            where_ $ bpmStatus post ==. lit Published
            pure post

-- | Get blog post by ID.
getBlogPostById :: Id -> Hasql.Statement () (Maybe Model)
getBlogPostById postId = fmap listToMaybe $ run $ select do
  post <- each blogPostSchema
  where_ $ bpmId post ==. lit postId
  pure post

-- | Get blog post by slug.
getBlogPostBySlug :: Slug -> Hasql.Statement () (Maybe Model)
getBlogPostBySlug postSlug = fmap listToMaybe $ run $ select do
  post <- each blogPostSchema
  where_ $ bpmSlug post ==. lit postSlug
  pure post

-- | Insert a new blog post.
insertBlogPost :: Insert -> Hasql.Statement () (Maybe Id)
insertBlogPost Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = blogPostSchema,
            rows =
              values
                [ BlogPost
                    { bpmId = nextId "blog_posts_id_seq",
                      bpmTitle = lit bpiTitle,
                      bpmSlug = lit bpiSlug,
                      bpmContent = lit bpiContent,
                      bpmExcerpt = lit bpiExcerpt,
                      bpmHeroImageUrl = lit bpiHeroImageUrl,
                      bpmAuthorId = lit bpiAuthorId,
                      bpmStatus = lit bpiStatus,
                      bpmPublishedAt = case bpiStatus of
                        Published -> nullify now
                        _ -> Rel8.null,
                      bpmCreatedAt = now,
                      bpmUpdatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning bpmId
          }

-- | Update a blog post.
--
-- The @published_at@ logic mirrors the original SQL CASE:
--
-- - Setting status to Published when @published_at@ is NULL → set to NOW()
-- - Setting status to non-Published → clear @published_at@ to NULL
-- - Otherwise → keep existing @published_at@
updateBlogPost :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateBlogPost postId Insert {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = blogPostSchema,
            from = pure (),
            set = \_ post ->
              post
                { bpmTitle = lit bpiTitle,
                  bpmSlug = lit bpiSlug,
                  bpmContent = lit bpiContent,
                  bpmExcerpt = lit bpiExcerpt,
                  bpmHeroImageUrl = lit bpiHeroImageUrl,
                  bpmStatus = lit bpiStatus,
                  bpmPublishedAt =
                    caseExpr
                      [ (lit bpiStatus ==. lit Published &&. isNull (bpmPublishedAt post), nullify now),
                        (not_ (lit bpiStatus ==. lit Published), Rel8.null)
                      ]
                      (bpmPublishedAt post),
                  bpmUpdatedAt = now
                },
            updateWhere = \_ post -> bpmId post ==. lit postId,
            returning = Returning bpmId
          }

-- | Delete a blog post.
deleteBlogPost :: Id -> Hasql.Statement () (Maybe Id)
deleteBlogPost postId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = blogPostSchema,
            using = pure (),
            deleteWhere = \_ post -> bpmId post ==. lit postId,
            returning = Returning bpmId
          }

--------------------------------------------------------------------------------
-- Junction Table (blog_post_tags)

-- | The @blog_post_tags@ junction table definition (internal, not exported).
data BlogPostTag f = BlogPostTag
  { bptPostId :: Column f Id,
    bptTagId :: Column f BlogTags.Id
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Schema for the @blog_post_tags@ junction table.
blogPostTagSchema :: TableSchema (BlogPostTag Name)
blogPostTagSchema =
  TableSchema
    { name = "blog_post_tags",
      columns =
        BlogPostTag
          { bptPostId = "post_id",
            bptTagId = "tag_id"
          }
    }

--------------------------------------------------------------------------------
-- Junction Table Queries (blog_post_tags)

-- | Get tags for a blog post.
getTagsForPost :: Id -> Hasql.Statement () [BlogTags.Model]
getTagsForPost postId =
  run $
    select $
      orderBy (BlogTags.btmName >$< asc) do
        bpt <- each blogPostTagSchema
        where_ $ bptPostId bpt ==. lit postId
        tag <- each BlogTags.blogTagSchema
        where_ $ BlogTags.btmId tag ==. bptTagId bpt
        pure tag

-- | Add tag to post.
addTagToPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
addTagToPost postId tagId =
  run_ $
    insert
      Rel8.Insert
        { into = blogPostTagSchema,
          rows = values [BlogPostTag {bptPostId = lit postId, bptTagId = lit tagId}],
          onConflict = DoNothing,
          returning = NoReturning
        }

-- | Remove tag from post.
removeTagFromPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
removeTagFromPost postId tagId =
  run_ $
    delete
      Rel8.Delete
        { from = blogPostTagSchema,
          using = pure (),
          deleteWhere = \_ bpt ->
            bptPostId bpt ==. lit postId &&. bptTagId bpt ==. lit tagId,
          returning = NoReturning
        }

-- | Get posts with specific tag.
getPostsByTag :: BlogTags.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPostsByTag tagId (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy ((bpmPublishedAt >$< nullsLast desc) <> (bpmCreatedAt >$< desc)) do
            post <- each blogPostSchema
            bpt <- each blogPostTagSchema
            where_ $ bptPostId bpt ==. bpmId post
            where_ $ bptTagId bpt ==. lit tagId
            where_ $ bpmStatus post ==. lit Published
            pure post

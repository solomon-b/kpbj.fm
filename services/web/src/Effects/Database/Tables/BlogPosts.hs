{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @blog_posts@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
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
import Data.Coerce (coerce)
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
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), interp, sql)
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

-- | Insert a new blog post.
insertBlogPost :: Insert -> Hasql.Statement () Id
insertBlogPost Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = blogPostSchema,
            rows =
              values
                [ BlogPost
                    { bpmId = coerce (nextval "blog_posts_id_seq"),
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
-- Uses raw SQL because rel8's UPDATE with complex CASE expressions
-- for published_at handling would be verbose.
updateBlogPost :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateBlogPost postId Insert {..} =
  interp
    False
    [sql|
    UPDATE blog_posts
    SET title = #{bpiTitle}, slug = #{bpiSlug}, content = #{bpiContent}, excerpt = #{bpiExcerpt},
        hero_image_url = #{bpiHeroImageUrl}, status = #{bpiStatus},
        published_at = CASE
          WHEN #{bpiStatus}::text = 'published' AND published_at IS NULL THEN NOW()
          WHEN #{bpiStatus}::text != 'published' THEN NULL
          ELSE published_at
        END,
        updated_at = NOW()
    WHERE id = #{postId}
    RETURNING id
  |]

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
-- Junction Table Queries (blog_post_tags)
--
-- These use raw SQL because they involve joins with other tables.

-- | Get tags for a blog post.
getTagsForPost :: Id -> Hasql.Statement () [BlogTags.Model]
getTagsForPost postId =
  interp
    False
    [sql|
    SELECT bt.id, bt.name, bt.created_at
    FROM blog_tags bt
    JOIN blog_post_tags bpt ON bt.id = bpt.tag_id
    WHERE bpt.post_id = #{postId}
    ORDER BY bt.name
  |]

-- | Add tag to post.
addTagToPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
addTagToPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from post.
removeTagFromPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
removeTagFromPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get posts with specific tag.
getPostsByTag :: BlogTags.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPostsByTag tagId (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT bp.id, bp.title, bp.slug, bp.content, bp.excerpt, bp.hero_image_url, bp.author_id, bp.status, bp.published_at, bp.created_at, bp.updated_at
    FROM blog_posts bp
    JOIN blog_post_tags bpt ON bp.id = bpt.post_id
    WHERE bpt.tag_id = #{tagId} AND bp.status = 'published'
    ORDER BY bp.published_at DESC NULLS LAST, bp.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

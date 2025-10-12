{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.BlogPosts where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Blog Post Models

newtype Id = Id Int64
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

-- | Database Model for the @blog_posts@ table
data Model = Model
  { bpmId :: Id,
    bpmTitle :: Text,
    bpmSlug :: Text,
    bpmContent :: Text,
    bpmExcerpt :: Maybe Text,
    bpmAuthorId :: User.Id,
    bpmStatus :: BlogPostStatus,
    bpmPublishedAt :: Maybe UTCTime,
    bpmCreatedAt :: UTCTime,
    bpmUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | Blog post with author information
data BlogPostWithAuthor = BlogPostWithAuthor
  { bpwaPost :: Model,
    bpwaAuthor :: UserMetadata.Model
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithAuthor)

-- | Blog post with tags
data BlogPostWithTags = BlogPostWithTags
  { bpwtPost :: Model,
    bpwtTags :: [BlogTags.Model]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithTags)

-- | Blog post with complete information (author + tags)
data BlogPostComplete = BlogPostComplete
  { bpcPost :: Model,
    bpcAuthor :: UserMetadata.Model,
    bpcTags :: [BlogTags.Model]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostComplete)

--------------------------------------------------------------------------------
-- Insert Types

data Insert = Insert
  { bpiTitle :: Text,
    bpiSlug :: Text,
    bpiContent :: Text,
    bpiExcerpt :: Maybe Text,
    bpiAuthorId :: User.Id,
    bpiStatus :: BlogPostStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via Insert
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all published blog posts ordered by publication date
getPublishedBlogPosts :: Int64 -> Int64 -> Hasql.Statement () [Model]
getPublishedBlogPosts limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get blog post by slug
getBlogPostBySlug :: Text -> Hasql.Statement () (Maybe Model)
getBlogPostBySlug slug =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE slug = #{slug}
  |]

-- | Get blog post by ID
getBlogPostById :: Id -> Hasql.Statement () (Maybe Model)
getBlogPostById postId =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE id = #{postId}
  |]

-- | Get blog posts by author
getBlogPostsByAuthor :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getBlogPostsByAuthor authorId limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE author_id = #{authorId}
    ORDER BY created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new blog post
insertBlogPost :: Insert -> Hasql.Statement () Id
insertBlogPost Insert {..} =
  case bpiStatus of
    Published ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO blog_posts(title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiStatus}, NOW(), NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO blog_posts(title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiStatus}, NULL, NOW(), NOW())
        RETURNING id
      |]

-- | Update a blog post
updateBlogPost :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateBlogPost postId Insert {..} =
  interp
    False
    [sql|
    UPDATE blog_posts
    SET title = #{bpiTitle}, slug = #{bpiSlug}, content = #{bpiContent}, excerpt = #{bpiExcerpt},
        status = #{bpiStatus},
        published_at = CASE
          WHEN #{bpiStatus}::text = 'published' AND published_at IS NULL THEN NOW()
          WHEN #{bpiStatus}::text != 'published' THEN NULL
          ELSE published_at
        END,
        updated_at = NOW()
    WHERE id = #{postId}
    RETURNING id
  |]

-- | Delete a blog post
deleteBlogPost :: Id -> Hasql.Statement () (Maybe Id)
deleteBlogPost postId =
  interp
    False
    [sql|
    DELETE FROM blog_posts
    WHERE id = #{postId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Junction Table Queries (blog_post_tags)

-- | Get tags for a blog post
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

-- | Add tag to post
addTagToPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
addTagToPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from post
removeTagFromPost :: Id -> BlogTags.Id -> Hasql.Statement () ()
removeTagFromPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get posts with specific tag
getPostsByTag :: BlogTags.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPostsByTag tagId limit offset =
  interp
    False
    [sql|
    SELECT bp.id, bp.title, bp.slug, bp.content, bp.excerpt, bp.author_id, bp.status, bp.published_at, bp.created_at, bp.updated_at
    FROM blog_posts bp
    JOIN blog_post_tags bpt ON bp.id = bpt.post_id
    WHERE bpt.tag_id = #{tagId} AND bp.status = 'published'
    ORDER BY bp.published_at DESC NULLS LAST, bp.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

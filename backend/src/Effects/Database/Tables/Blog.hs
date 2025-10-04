{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.Blog where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Blog Post Models

newtype BlogPostId = BlogPostId Int64
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

newtype BlogTagId = BlogTagId Int64
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
data BlogPostModel = BlogPostModel
  { bpmId :: BlogPostId,
    bpmTitle :: Text,
    bpmSlug :: Text,
    bpmContent :: Text,
    bpmExcerpt :: Maybe Text,
    bpmAuthorId :: User.Id,
    bpmCategory :: Text,
    bpmStatus :: BlogPostStatus,
    bpmPublishedAt :: Maybe UTCTime,
    bpmCreatedAt :: UTCTime,
    bpmUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance BlogPostModel)

-- | Database Model for the @blog_tags@ table
data BlogTagModel = BlogTagModel
  { btmId :: BlogTagId,
    btmName :: Text,
    btmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance BlogTagModel)

-- | API Domain Type for @BlogPost@
data BlogPostDomain = BlogPostDomain
  { bpdId :: BlogPostId,
    bpdTitle :: Text,
    bpdSlug :: Text,
    bpdContent :: Text,
    bpdExcerpt :: Maybe Text,
    bpdAuthorId :: User.Id,
    bpdCategory :: Text,
    bpdStatus :: BlogPostStatus,
    bpdPublishedAt :: Maybe UTCTime,
    bpdCreatedAt :: UTCTime,
    bpdUpdatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostDomain)
  deriving anyclass (FromJSON, ToJSON)

-- | API Domain Type for @BlogTag@
data BlogTagDomain = BlogTagDomain
  { btdId :: BlogTagId,
    btdName :: Text,
    btdCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogTagDomain)
  deriving anyclass (FromJSON, ToJSON)

-- | Blog post with author information
data BlogPostWithAuthor = BlogPostWithAuthor
  { bpwaPost :: BlogPostDomain,
    bpwaAuthor :: UserMetadata.Domain
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithAuthor)
  deriving anyclass (FromJSON, ToJSON)

-- | Blog post with tags
data BlogPostWithTags = BlogPostWithTags
  { bpwtPost :: BlogPostDomain,
    bpwtTags :: [BlogTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostWithTags)
  deriving anyclass (FromJSON, ToJSON)

-- | Blog post with complete information (author + tags)
data BlogPostComplete = BlogPostComplete
  { bpcPost :: BlogPostDomain,
    bpcAuthor :: UserMetadata.Domain,
    bpcTags :: [BlogTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance BlogPostComplete)
  deriving anyclass (FromJSON, ToJSON)

toDomainBlogPost :: BlogPostModel -> BlogPostDomain
toDomainBlogPost BlogPostModel {..} =
  BlogPostDomain
    { bpdId = bpmId,
      bpdTitle = bpmTitle,
      bpdSlug = bpmSlug,
      bpdContent = bpmContent,
      bpdExcerpt = bpmExcerpt,
      bpdAuthorId = bpmAuthorId,
      bpdCategory = bpmCategory,
      bpdStatus = bpmStatus,
      bpdPublishedAt = bpmPublishedAt,
      bpdCreatedAt = bpmCreatedAt,
      bpdUpdatedAt = bpmUpdatedAt
    }

toDomainBlogTag :: BlogTagModel -> BlogTagDomain
toDomainBlogTag BlogTagModel {..} =
  BlogTagDomain
    { btdId = btmId,
      btdName = btmName,
      btdCreatedAt = btmCreatedAt
    }

--------------------------------------------------------------------------------
-- Insert Types

data BlogPostInsert = BlogPostInsert
  { bpiTitle :: Text,
    bpiSlug :: Text,
    bpiContent :: Text,
    bpiExcerpt :: Maybe Text,
    bpiAuthorId :: User.Id,
    bpiCategory :: Text,
    bpiStatus :: BlogPostStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via BlogPostInsert
  deriving (Display) via (RecordInstance BlogPostInsert)

newtype BlogTagInsert = BlogTagInsert {btiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via BlogTagInsert
  deriving (Display) via (RecordInstance BlogTagInsert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all published blog posts ordered by publication date
getPublishedBlogPosts :: Int64 -> Int64 -> Hasql.Statement () [BlogPostModel]
getPublishedBlogPosts limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get blog post by slug
getBlogPostBySlug :: Text -> Hasql.Statement () (Maybe BlogPostModel)
getBlogPostBySlug slug =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE slug = #{slug}
  |]

-- | Get blog post by ID
getBlogPostById :: BlogPostId -> Hasql.Statement () (Maybe BlogPostModel)
getBlogPostById postId =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE id = #{postId}
  |]

-- | Get blog posts by category
getBlogPostsByCategory :: Text -> Int64 -> Int64 -> Hasql.Statement () [BlogPostModel]
getBlogPostsByCategory category limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE status = 'published' AND category = #{category}
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get blog posts by author
getBlogPostsByAuthor :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [BlogPostModel]
getBlogPostsByAuthor authorId limit offset =
  interp
    False
    [sql|
    SELECT id, title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at
    FROM blog_posts
    WHERE author_id = #{authorId}
    ORDER BY created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new blog post
insertBlogPost :: BlogPostInsert -> Hasql.Statement () BlogPostId
insertBlogPost BlogPostInsert {..} =
  case bpiStatus of
    Published ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO blog_posts(title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at)
        VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiCategory}, #{bpiStatus}, NOW(), NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO blog_posts(title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at)
        VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiCategory}, #{bpiStatus}, NULL, NOW(), NOW())
        RETURNING id
      |]

-- | Update a blog post
updateBlogPost :: BlogPostId -> BlogPostInsert -> Hasql.Statement () (Maybe BlogPostId)
updateBlogPost postId BlogPostInsert {..} =
  interp
    False
    [sql|
    UPDATE blog_posts
    SET title = #{bpiTitle}, slug = #{bpiSlug}, content = #{bpiContent}, excerpt = #{bpiExcerpt},
        category = #{bpiCategory}, status = #{bpiStatus},
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
deleteBlogPost :: BlogPostId -> Hasql.Statement () (Maybe BlogPostId)
deleteBlogPost postId =
  interp
    False
    [sql|
    DELETE FROM blog_posts
    WHERE id = #{postId}
    RETURNING id
  |]

--------------------------------------------------------------------------------
-- Tag Queries

-- | Get all tags
getAllTags :: Hasql.Statement () [BlogTagModel]
getAllTags =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM blog_tags
    ORDER BY name
  |]

-- | Get tag by name
getTagByName :: Text -> Hasql.Statement () (Maybe BlogTagModel)
getTagByName tagName =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM blog_tags
    WHERE name = #{tagName}
  |]

-- | Insert a new tag
insertTag :: BlogTagInsert -> Hasql.Statement () BlogTagId
insertTag BlogTagInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO blog_tags(name, created_at)
    VALUES (#{btiName}, NOW())
    RETURNING id
  |]

-- | Get tags for a blog post
getTagsForPost :: BlogPostId -> Hasql.Statement () [BlogTagModel]
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
addTagToPost :: BlogPostId -> BlogTagId -> Hasql.Statement () ()
addTagToPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from post
removeTagFromPost :: BlogPostId -> BlogTagId -> Hasql.Statement () ()
removeTagFromPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get posts with specific tag
getPostsByTag :: BlogTagId -> Int64 -> Int64 -> Hasql.Statement () [BlogPostModel]
getPostsByTag tagId limit offset =
  interp
    False
    [sql|
    SELECT bp.id, bp.title, bp.slug, bp.content, bp.excerpt, bp.author_id, bp.category, bp.status, bp.published_at, bp.created_at, bp.updated_at
    FROM blog_posts bp
    JOIN blog_post_tags bpt ON bp.id = bpt.post_id
    WHERE bpt.tag_id = #{tagId} AND bp.status = 'published'
    ORDER BY bp.published_at DESC NULLS LAST, bp.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- Tag with count for queries (flattened for easier decoding)
data BlogTagWithCount = BlogTagWithCount
  { btwcId :: BlogTagId,
    btwcName :: Text,
    btwcCreatedAt :: UTCTime,
    btwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance BlogTagWithCount)

-- | Get tags with their usage counts (only tags used by published posts)
getTagsWithCounts :: Hasql.Statement () [BlogTagWithCount]
getTagsWithCounts =
  interp
    False
    [sql|
    SELECT bt.id, bt.name, bt.created_at, COUNT(bpt.post_id)::bigint as post_count
    FROM blog_tags bt
    LEFT JOIN blog_post_tags bpt ON bt.id = bpt.tag_id
    LEFT JOIN blog_posts bp ON bpt.post_id = bp.id AND bp.status = 'published'
    GROUP BY bt.id, bt.name, bt.created_at
    HAVING COUNT(bpt.post_id) > 0
    ORDER BY COUNT(bpt.post_id) DESC, bt.name
  |]

-- Category with count for queries
data CategoryWithCount = CategoryWithCount
  { cwcCategory :: Text,
    cwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance CategoryWithCount)

-- | Get categories with their post counts
getCategoriesWithCounts :: Hasql.Statement () [CategoryWithCount]
getCategoriesWithCounts =
  interp
    False
    [sql|
    SELECT category, COUNT(*)::bigint as post_count
    FROM blog_posts
    WHERE status = 'published'
    GROUP BY category
    ORDER BY post_count DESC, category
  |]

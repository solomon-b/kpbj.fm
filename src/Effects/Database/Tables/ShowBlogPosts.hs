{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowBlogPosts where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Domain.Types.PostStatus (BlogPostStatus (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Database Models

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

data Model = Model
  { id :: Id,
    showId :: Shows.Id,
    title :: Text,
    slug :: Slug,
    content :: Text,
    excerpt :: Maybe Text,
    authorId :: User.Id,
    status :: BlogPostStatus,
    publishedAt :: Maybe UTCTime,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

data Insert = Insert
  { sbpiId :: Shows.Id,
    sbpiTitle :: Text,
    sbpiSlug :: Slug,
    sbpiContent :: Text,
    sbpiExcerpt :: Maybe Text,
    sbpiAuthorId :: User.Id,
    sbpiStatus :: BlogPostStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get published blog posts for a show
getPublishedShowBlogPosts :: Shows.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPublishedShowBlogPosts showId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE show_id = #{showId} AND status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get show blog post by show slug and post slug
getShowBlogPostBySlug :: Text -> Text -> Hasql.Statement () (Maybe Model)
getShowBlogPostBySlug showSlug postSlug =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN shows s ON sbp.show_id = s.id
    WHERE s.slug = #{showSlug} AND sbp.slug = #{postSlug}
  |]

-- | Get show blog post by ID
getShowBlogPostById :: Id -> Hasql.Statement () (Maybe Model)
getShowBlogPostById postId =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE id = #{postId}
  |]

-- | Get show blog posts by author
getShowBlogPostsByAuthor :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getShowBlogPostsByAuthor authorId limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE author_id = #{authorId}
    ORDER BY created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get recent published show blog posts across all shows
getRecentPublishedShowBlogPosts :: Int64 -> Int64 -> Hasql.Statement () [Model]
getRecentPublishedShowBlogPosts limit offset =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Insert a new show blog post
insertShowBlogPost :: Insert -> Hasql.Statement () Id
insertShowBlogPost Insert {..} =
  case sbpiStatus of
    Published ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO show_blog_posts(show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{sbpiId}, #{sbpiTitle}, #{sbpiSlug}, #{sbpiContent}, #{sbpiExcerpt}, #{sbpiAuthorId}, #{sbpiStatus}, NOW(), NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO show_blog_posts(show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{sbpiId}, #{sbpiTitle}, #{sbpiSlug}, #{sbpiContent}, #{sbpiExcerpt}, #{sbpiAuthorId}, #{sbpiStatus}, NULL, NOW(), NOW())
        RETURNING id
      |]

-- | Update a show blog post
updateShowBlogPost :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateShowBlogPost postId Insert {..} =
  interp
    False
    [sql|
    UPDATE show_blog_posts
    SET title = #{sbpiTitle}, slug = #{sbpiSlug}, content = #{sbpiContent}, excerpt = #{sbpiExcerpt},
        status = #{sbpiStatus},
        published_at = CASE
          WHEN #{sbpiStatus}::text = 'published' AND published_at IS NULL THEN NOW()
          WHEN #{sbpiStatus}::text != 'published' THEN NULL
          ELSE published_at
        END,
        updated_at = NOW()
    WHERE id = #{postId}
    RETURNING id
  |]

-- | Delete a show blog post
deleteShowBlogPost :: Id -> Hasql.Statement () (Maybe Id)
deleteShowBlogPost postId =
  interp
    False
    [sql|
    DELETE FROM show_blog_posts
    WHERE id = #{postId}
    RETURNING id
  |]

-- | Check if user can edit show blog post (must be host of the show)
canUserEditShowBlogPost :: User.Id -> Id -> Hasql.Statement () Bool
canUserEditShowBlogPost userId postId =
  let query =
        interp
          False
          [sql|
        SELECT EXISTS(
          SELECT 1 FROM show_blog_posts sbp
          JOIN show_hosts sh ON sbp.show_id = sh.show_id
          WHERE sbp.id = #{postId} AND sh.user_id = #{userId} AND sh.left_at IS NULL
        )
      |]
   in maybe False getOneColumn <$> query

-- | Get published blog posts for a show by show slug
getPublishedShowBlogPostsBySlug :: Text -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPublishedShowBlogPostsBySlug showSlug limit offset =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN shows s ON sbp.show_id = s.id
    WHERE s.slug = #{showSlug} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Get published blog posts for a show filtered by tag
getPublishedShowBlogPostsByShowAndTag :: Shows.Id -> ShowBlogTags.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getPublishedShowBlogPostsByShowAndTag showId tagId limit offset =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
    WHERE sbp.show_id = #{showId} AND sbpt.tag_id = #{tagId} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- | Count published blog posts for a show
countPublishedShowBlogPosts :: Shows.Id -> Hasql.Statement () Int64
countPublishedShowBlogPosts showId =
  let query =
        interp
          False
          [sql|
        SELECT COUNT(*)::bigint
        FROM show_blog_posts
        WHERE show_id = #{showId} AND status = 'published'
      |]
   in maybe 0 getOneColumn <$> query

-- | Count published blog posts for a show filtered by tag
countPublishedShowBlogPostsByTag :: Shows.Id -> ShowBlogTags.Id -> Hasql.Statement () Int64
countPublishedShowBlogPostsByTag showId tagId =
  let query =
        interp
          False
          [sql|
        SELECT COUNT(*)::bigint
        FROM show_blog_posts sbp
        JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
        WHERE sbp.show_id = #{showId} AND sbpt.tag_id = #{tagId} AND sbp.status = 'published'
      |]
   in maybe 0 getOneColumn <$> query

-- | Get all tags used by a show's blog posts
getTagsForShow :: Shows.Id -> Hasql.Statement () [ShowBlogTags.Model]
getTagsForShow showId =
  interp
    False
    [sql|
    SELECT DISTINCT sbt.id, sbt.name, sbt.created_at
    FROM show_blog_tags sbt
    JOIN show_blog_post_tags sbpt ON sbt.id = sbpt.tag_id
    JOIN show_blog_posts sbp ON sbpt.post_id = sbp.id
    WHERE sbp.show_id = #{showId} AND sbp.status = 'published'
    ORDER BY sbt.name
  |]

--------------------------------------------------------------------------------
-- Junction Table Queries (show_blog_post_tags)

-- | Get tags for a show blog post
getTagsForShowBlogPost :: Id -> Hasql.Statement () [ShowBlogTags.Model]
getTagsForShowBlogPost postId =
  interp
    False
    [sql|
    SELECT sbt.id, sbt.name, sbt.created_at
    FROM show_blog_tags sbt
    JOIN show_blog_post_tags sbpt ON sbt.id = sbpt.tag_id
    WHERE sbpt.post_id = #{postId}
    ORDER BY sbt.name
  |]

-- | Add tag to show blog post
addTagToShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
addTagToShowBlogPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO show_blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from show blog post
removeTagFromShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
removeTagFromShowBlogPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM show_blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get show blog posts with specific tag
getShowBlogPostsByTag :: ShowBlogTags.Id -> Int64 -> Int64 -> Hasql.Statement () [Model]
getShowBlogPostsByTag tagId limit offset =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
    WHERE sbpt.tag_id = #{tagId} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{limit} OFFSET #{offset}
  |]

-- Show blog tag with count for queries
data ShowBlogTagWithCount = ShowBlogTagWithCount
  { sbtwcId :: ShowBlogTags.Id,
    sbtwcName :: Text,
    sbtwcCreatedAt :: UTCTime,
    sbtwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagWithCount)

-- | Get show blog tags with their usage counts (only tags used by published posts)
getShowBlogTagsWithCounts :: Hasql.Statement () [ShowBlogTagWithCount]
getShowBlogTagsWithCounts =
  interp
    False
    [sql|
    SELECT sbt.id, sbt.name, sbt.created_at, COUNT(sbpt.post_id)::bigint as post_count
    FROM show_blog_tags sbt
    LEFT JOIN show_blog_post_tags sbpt ON sbt.id = sbpt.tag_id
    LEFT JOIN show_blog_posts sbp ON sbpt.post_id = sbp.id AND sbp.status = 'published'
    GROUP BY sbt.id, sbt.name, sbt.created_at
    HAVING COUNT(sbpt.post_id) > 0
    ORDER BY COUNT(sbpt.post_id) DESC, sbt.name
  |]

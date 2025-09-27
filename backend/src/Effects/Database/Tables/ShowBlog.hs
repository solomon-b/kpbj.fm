{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowBlog where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..), display, displayBuilder)
import Data.Time (UTCTime)
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import GHC.Generics
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneColumn (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Show Blog Status Type

data ShowBlogStatus = BlogDraft | BlogPublished | BlogArchived
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
  deriving anyclass (FromJSON, ToJSON)

instance Display ShowBlogStatus where
  displayBuilder BlogDraft = "draft"
  displayBuilder BlogPublished = "published"
  displayBuilder BlogArchived = "archived"

instance DecodeValue ShowBlogStatus where
  decodeValue = Decoders.enum decodeShowBlogStatus

decodeShowBlogStatus :: Text -> Maybe ShowBlogStatus
decodeShowBlogStatus = \case
  "draft" -> Just BlogDraft
  "published" -> Just BlogPublished
  "archived" -> Just BlogArchived
  _ -> Nothing

instance EncodeValue ShowBlogStatus where
  encodeValue = Encoders.enum $ \case
    BlogDraft -> "draft"
    BlogPublished -> "published"
    BlogArchived -> "archived"

instance Servant.FromHttpApiData ShowBlogStatus where
  parseUrlPiece "draft" = Right BlogDraft
  parseUrlPiece "published" = Right BlogPublished
  parseUrlPiece "archived" = Right BlogArchived
  parseUrlPiece invalid = Left $ "Invalid ShowBlogStatus: " <> invalid

instance Servant.ToHttpApiData ShowBlogStatus where
  toUrlPiece = display

--------------------------------------------------------------------------------
-- ID Types

newtype ShowBlogPostId = ShowBlogPostId Int64
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

newtype ShowBlogTagId = ShowBlogTagId Int64
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

--------------------------------------------------------------------------------
-- Database Models

data ShowBlogPostModel = ShowBlogPostModel
  { sbpmId :: ShowBlogPostId,
    sbpmShowId :: Show.ShowId,
    sbpmTitle :: Text,
    sbpmSlug :: Text,
    sbpmContent :: Text,
    sbpmExcerpt :: Maybe Text,
    sbpmAuthorId :: User.Id,
    sbpmStatus :: ShowBlogStatus,
    sbpmPublishedAt :: Maybe UTCTime,
    sbpmCreatedAt :: UTCTime,
    sbpmUpdatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowBlogPostModel)

data ShowBlogTagModel = ShowBlogTagModel
  { sbtmId :: ShowBlogTagId,
    sbtmName :: Text,
    sbtmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagModel)

--------------------------------------------------------------------------------
-- Domain Types

data ShowBlogPostDomain = ShowBlogPostDomain
  { sbpdId :: ShowBlogPostId,
    sbpdShowId :: Show.ShowId,
    sbpdTitle :: Text,
    sbpdSlug :: Text,
    sbpdContent :: Text,
    sbpdExcerpt :: Maybe Text,
    sbpdAuthorId :: User.Id,
    sbpdStatus :: ShowBlogStatus,
    sbpdPublishedAt :: Maybe UTCTime,
    sbpdCreatedAt :: UTCTime,
    sbpdUpdatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowBlogPostDomain)
  deriving anyclass (FromJSON, ToJSON)

data ShowBlogTagDomain = ShowBlogTagDomain
  { sbtdId :: ShowBlogTagId,
    sbtdName :: Text,
    sbtdCreatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowBlogTagDomain)
  deriving anyclass (FromJSON, ToJSON)

-- | Show blog post with author information
data ShowBlogPostWithAuthor = ShowBlogPostWithAuthor
  { sbpwaPost :: ShowBlogPostDomain,
    sbpwaAuthor :: UserMetadata.Domain
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowBlogPostWithAuthor)
  deriving anyclass (FromJSON, ToJSON)

-- | Show blog post with tags
data ShowBlogPostWithTags = ShowBlogPostWithTags
  { sbpwtPost :: ShowBlogPostDomain,
    sbpwtTags :: [ShowBlogTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowBlogPostWithTags)
  deriving anyclass (FromJSON, ToJSON)

-- | Show blog post with complete information (author + tags + show)
data ShowBlogPostComplete = ShowBlogPostComplete
  { sbpcPost :: ShowBlogPostDomain,
    sbpcAuthor :: UserMetadata.Domain,
    sbpcShow :: Show.ShowDomain,
    sbpcTags :: [ShowBlogTagDomain]
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance ShowBlogPostComplete)
  deriving anyclass (FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Conversion Functions

toDomainShowBlogPost :: ShowBlogPostModel -> ShowBlogPostDomain
toDomainShowBlogPost ShowBlogPostModel {..} =
  ShowBlogPostDomain
    { sbpdId = sbpmId,
      sbpdShowId = sbpmShowId,
      sbpdTitle = sbpmTitle,
      sbpdSlug = sbpmSlug,
      sbpdContent = sbpmContent,
      sbpdExcerpt = sbpmExcerpt,
      sbpdAuthorId = sbpmAuthorId,
      sbpdStatus = sbpmStatus,
      sbpdPublishedAt = sbpmPublishedAt,
      sbpdCreatedAt = sbpmCreatedAt,
      sbpdUpdatedAt = sbpmUpdatedAt
    }

toDomainShowBlogTag :: ShowBlogTagModel -> ShowBlogTagDomain
toDomainShowBlogTag ShowBlogTagModel {..} =
  ShowBlogTagDomain
    { sbtdId = sbtmId,
      sbtdName = sbtmName,
      sbtdCreatedAt = sbtmCreatedAt
    }

--------------------------------------------------------------------------------
-- Insert Types

data ShowBlogPostInsert = ShowBlogPostInsert
  { sbpiShowId :: Show.ShowId,
    sbpiTitle :: Text,
    sbpiSlug :: Text,
    sbpiContent :: Text,
    sbpiExcerpt :: Maybe Text,
    sbpiAuthorId :: User.Id,
    sbpiStatus :: ShowBlogStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance ShowBlogPostInsert)

newtype ShowBlogTagInsert = ShowBlogTagInsert {sbtiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagInsert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get published blog posts for a show
getPublishedShowBlogPosts :: Show.ShowId -> Int64 -> Int64 -> Hasql.Statement () [ShowBlogPostModel]
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
getShowBlogPostBySlug :: Text -> Text -> Hasql.Statement () (Maybe ShowBlogPostModel)
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
getShowBlogPostById :: ShowBlogPostId -> Hasql.Statement () (Maybe ShowBlogPostModel)
getShowBlogPostById postId =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE id = #{postId}
  |]

-- | Get show blog posts by author
getShowBlogPostsByAuthor :: User.Id -> Int64 -> Int64 -> Hasql.Statement () [ShowBlogPostModel]
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
getRecentPublishedShowBlogPosts :: Int64 -> Int64 -> Hasql.Statement () [ShowBlogPostModel]
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
insertShowBlogPost :: ShowBlogPostInsert -> Hasql.Statement () ShowBlogPostId
insertShowBlogPost ShowBlogPostInsert {..} =
  case sbpiStatus of
    BlogPublished ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO show_blog_posts(show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{sbpiShowId}, #{sbpiTitle}, #{sbpiSlug}, #{sbpiContent}, #{sbpiExcerpt}, #{sbpiAuthorId}, #{sbpiStatus}, NOW(), NOW(), NOW())
        RETURNING id
      |]
    _ ->
      getOneRow
        <$> interp
          False
          [sql|
        INSERT INTO show_blog_posts(show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at)
        VALUES (#{sbpiShowId}, #{sbpiTitle}, #{sbpiSlug}, #{sbpiContent}, #{sbpiExcerpt}, #{sbpiAuthorId}, #{sbpiStatus}, NULL, NOW(), NOW())
        RETURNING id
      |]

-- | Update a show blog post
updateShowBlogPost :: ShowBlogPostId -> ShowBlogPostInsert -> Hasql.Statement () (Maybe ShowBlogPostId)
updateShowBlogPost postId ShowBlogPostInsert {..} =
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
deleteShowBlogPost :: ShowBlogPostId -> Hasql.Statement () (Maybe ShowBlogPostId)
deleteShowBlogPost postId =
  interp
    False
    [sql|
    DELETE FROM show_blog_posts
    WHERE id = #{postId}
    RETURNING id
  |]

-- | Check if user can edit show blog post (must be host of the show) (TODO: Fix Bool DecodeRow issue)
canUserEditShowBlogPost :: User.Id -> ShowBlogPostId -> Hasql.Statement () (OneColumn Bool)
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
   in fromMaybe (OneColumn False) <$> query

--------------------------------------------------------------------------------
-- Show Blog Tag Queries

-- | Get all show blog tags
getAllShowBlogTags :: Hasql.Statement () [ShowBlogTagModel]
getAllShowBlogTags =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM show_blog_tags
    ORDER BY name
  |]

-- | Get show blog tag by name
getShowBlogTagByName :: Text -> Hasql.Statement () (Maybe ShowBlogTagModel)
getShowBlogTagByName tagName =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM show_blog_tags
    WHERE name = #{tagName}
  |]

-- | Insert a new show blog tag
insertShowBlogTag :: ShowBlogTagInsert -> Hasql.Statement () ShowBlogTagId
insertShowBlogTag ShowBlogTagInsert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO show_blog_tags(name, created_at)
    VALUES (#{sbtiName}, NOW())
    RETURNING id
  |]

-- | Get tags for a show blog post
getTagsForShowBlogPost :: ShowBlogPostId -> Hasql.Statement () [ShowBlogTagModel]
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
addTagToShowBlogPost :: ShowBlogPostId -> ShowBlogTagId -> Hasql.Statement () ()
addTagToShowBlogPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO show_blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from show blog post
removeTagFromShowBlogPost :: ShowBlogPostId -> ShowBlogTagId -> Hasql.Statement () ()
removeTagFromShowBlogPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM show_blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get show blog posts with specific tag
getShowBlogPostsByTag :: ShowBlogTagId -> Int64 -> Int64 -> Hasql.Statement () [ShowBlogPostModel]
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
  { sbtwcId :: ShowBlogTagId,
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

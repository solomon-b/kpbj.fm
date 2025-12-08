{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-x-partial #-}

-- | Database table definition and queries for @show_blog_posts@.
--
-- Uses rel8 for simple queries and raw SQL (hasql-interpolate) for complex joins.
module Effects.Database.Tables.ShowBlogPosts
  ( -- * Id Type
    Id (..),

    -- * Table Definition
    ShowBlogPost (..),
    showBlogPostSchema,

    -- * Model (Result alias)
    Model,

    -- * Insert Type
    Insert (..),

    -- * Queries
    getShowBlogPosts,
    getPublishedShowBlogPosts,
    getAllRecentShowBlogPosts,
    getShowBlogPostBySlug,
    getShowBlogPostById,
    getShowBlogPostsByAuthor,
    getRecentPublishedShowBlogPosts,
    insertShowBlogPost,
    updateShowBlogPost,
    deleteShowBlogPost,
    canUserEditShowBlogPost,
    getPublishedShowBlogPostsBySlug,
    getPublishedShowBlogPostsByShowAndTag,
    countPublishedShowBlogPosts,
    countPublishedShowBlogPostsByTag,
    getTagsForShow,

    -- * Junction Table Queries (show_blog_post_tags)
    getTagsForShowBlogPost,
    addTagToShowBlogPost,
    removeTagFromShowBlogPost,
    getShowBlogPostsByTag,
    getShowBlogTagsWithCounts,

    -- * Result Types
    ShowBlogTagWithCount (..),
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
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeValue (..), OneColumn (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8 hiding (Insert)
import Rel8 qualified
import Rel8.Expr.Time (now)
import Servant qualified

--------------------------------------------------------------------------------
-- Id Type

-- | Newtype wrapper for show blog post primary keys.
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

-- | The @show_blog_posts@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
data ShowBlogPost f = ShowBlogPost
  { id :: Column f Id,
    showId :: Column f Shows.Id,
    title :: Column f Text,
    slug :: Column f Slug,
    content :: Column f Text,
    excerpt :: Column f (Maybe Text),
    authorId :: Column f User.Id,
    status :: Column f BlogPostStatus,
    publishedAt :: Column f (Maybe UTCTime),
    createdAt :: Column f UTCTime,
    updatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (ShowBlogPost f)

deriving stock instance (f ~ Result) => Eq (ShowBlogPost f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (ShowBlogPost Result)

-- | Display instance for ShowBlogPost Result.
instance Display (ShowBlogPost Result) where
  displayBuilder post =
    "ShowBlogPost { id = "
      <> displayBuilder post.id
      <> ", title = "
      <> displayBuilder post.title
      <> ", slug = "
      <> displayBuilder post.slug
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @ShowBlogPost Result@.
type Model = ShowBlogPost Result

-- | Table schema connecting the Haskell type to the database table.
showBlogPostSchema :: TableSchema (ShowBlogPost Name)
showBlogPostSchema =
  TableSchema
    { name = "show_blog_posts",
      columns =
        ShowBlogPost
          { id = "id",
            showId = "show_id",
            title = "title",
            slug = "slug",
            content = "content",
            excerpt = "excerpt",
            authorId = "author_id",
            status = "status",
            publishedAt = "published_at",
            createdAt = "created_at",
            updatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Insert Type

-- | Insert type for creating new show blog posts.
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
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Result Types

-- | Show blog tag with count for aggregate queries.
data ShowBlogTagWithCount = ShowBlogTagWithCount
  { sbtwcId :: ShowBlogTags.Id,
    sbtwcName :: Text,
    sbtwcCreatedAt :: UTCTime,
    sbtwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagWithCount)

--------------------------------------------------------------------------------
-- Queries

-- | Get blog posts for a show (any status) ordered by status then publication date.
getShowBlogPosts :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getShowBlogPosts showIdVal (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE show_id = #{showIdVal}
    ORDER BY status, published_at DESC NULLS LAST, created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get published blog posts for a show.
getPublishedShowBlogPosts :: Shows.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedShowBlogPosts showIdVal (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE show_id = #{showIdVal} AND status = 'published'
    ORDER BY published_at DESC NULLS LAST, created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get all recent published show blog posts across all shows (for admin dashboard).
getAllRecentShowBlogPosts :: Limit -> Hasql.Statement () [Model]
getAllRecentShowBlogPosts (Limit lim) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        orderBy (publishedAt >$< nullsLast desc) do
          post <- each showBlogPostSchema
          where_ $ status post ==. lit Published
          pure post

-- | Get show blog post by show slug and post slug.
--
-- Uses raw SQL because it joins with the shows table.
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

-- | Get show blog post by ID.
getShowBlogPostById :: Id -> Hasql.Statement () (Maybe Model)
getShowBlogPostById postId = fmap listToMaybe $ run $ select do
  post <- each showBlogPostSchema
  where_ $ (.id) post ==. lit postId
  pure post

-- | Get show blog posts by author.
getShowBlogPostsByAuthor :: User.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getShowBlogPostsByAuthor authorIdVal (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT id, show_id, title, slug, content, excerpt, author_id, status, published_at, created_at, updated_at
    FROM show_blog_posts
    WHERE author_id = #{authorIdVal}
    ORDER BY created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get recent published show blog posts across all shows.
getRecentPublishedShowBlogPosts :: Limit -> Offset -> Hasql.Statement () [Model]
getRecentPublishedShowBlogPosts (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (publishedAt >$< nullsLast desc) do
            post <- each showBlogPostSchema
            where_ $ status post ==. lit Published
            pure post

-- | Insert a new show blog post.
insertShowBlogPost :: Insert -> Hasql.Statement () Id
insertShowBlogPost Insert {..} =
  fmap head $
    run $
      insert
        Rel8.Insert
          { into = showBlogPostSchema,
            rows =
              values
                [ ShowBlogPost
                    { id = coerce (nextval "show_blog_posts_id_seq"),
                      showId = lit sbpiId,
                      title = lit sbpiTitle,
                      slug = lit sbpiSlug,
                      content = lit sbpiContent,
                      excerpt = lit sbpiExcerpt,
                      authorId = lit sbpiAuthorId,
                      status = lit sbpiStatus,
                      publishedAt = case sbpiStatus of
                        Published -> nullify now
                        _ -> Rel8.null,
                      createdAt = now,
                      updatedAt = now
                    }
                ],
            onConflict = Abort,
            returning = Returning (.id)
          }

-- | Update a show blog post.
--
-- Uses raw SQL because rel8's UPDATE with complex CASE expressions
-- for published_at handling would be verbose.
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

-- | Delete a show blog post.
deleteShowBlogPost :: Id -> Hasql.Statement () (Maybe Id)
deleteShowBlogPost postId =
  fmap listToMaybe $
    run $
      delete
        Rel8.Delete
          { from = showBlogPostSchema,
            using = pure (),
            deleteWhere = \_ post -> (.id) post ==. lit postId,
            returning = Returning (.id)
          }

-- | Check if user can edit show blog post (must be host of the show).
--
-- Uses raw SQL because it joins multiple tables.
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

-- | Get published blog posts for a show by show slug.
--
-- Uses raw SQL because it joins with the shows table.
getPublishedShowBlogPostsBySlug :: Text -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedShowBlogPostsBySlug showSlug (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN shows s ON sbp.show_id = s.id
    WHERE s.slug = #{showSlug} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get published blog posts for a show filtered by tag.
--
-- Uses raw SQL because it joins with the junction table.
getPublishedShowBlogPostsByShowAndTag :: Shows.Id -> ShowBlogTags.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedShowBlogPostsByShowAndTag showIdVal tagId (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
    WHERE sbp.show_id = #{showIdVal} AND sbpt.tag_id = #{tagId} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Count published blog posts for a show.
countPublishedShowBlogPosts :: Shows.Id -> Hasql.Statement () Int64
countPublishedShowBlogPosts showIdVal =
  let query =
        interp
          False
          [sql|
        SELECT COUNT(*)::bigint
        FROM show_blog_posts
        WHERE show_id = #{showIdVal} AND status = 'published'
      |]
   in maybe 0 getOneColumn <$> query

-- | Count published blog posts for a show filtered by tag.
countPublishedShowBlogPostsByTag :: Shows.Id -> ShowBlogTags.Id -> Hasql.Statement () Int64
countPublishedShowBlogPostsByTag showIdVal tagId =
  let query =
        interp
          False
          [sql|
        SELECT COUNT(*)::bigint
        FROM show_blog_posts sbp
        JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
        WHERE sbp.show_id = #{showIdVal} AND sbpt.tag_id = #{tagId} AND sbp.status = 'published'
      |]
   in maybe 0 getOneColumn <$> query

-- | Get all tags used by a show's blog posts.
--
-- Uses raw SQL because it joins multiple tables.
getTagsForShow :: Shows.Id -> Hasql.Statement () [ShowBlogTags.Model]
getTagsForShow showIdVal =
  interp
    False
    [sql|
    SELECT DISTINCT sbt.id, sbt.name, sbt.created_at
    FROM show_blog_tags sbt
    JOIN show_blog_post_tags sbpt ON sbt.id = sbpt.tag_id
    JOIN show_blog_posts sbp ON sbpt.post_id = sbp.id
    WHERE sbp.show_id = #{showIdVal} AND sbp.status = 'published'
    ORDER BY sbt.name
  |]

--------------------------------------------------------------------------------
-- Junction Table Queries (show_blog_post_tags)
--
-- These use raw SQL because they involve joins with other tables.

-- | Get tags for a show blog post.
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

-- | Add tag to show blog post.
addTagToShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
addTagToShowBlogPost postId tagId =
  interp
    False
    [sql|
    INSERT INTO show_blog_post_tags(post_id, tag_id)
    VALUES (#{postId}, #{tagId})
    ON CONFLICT (post_id, tag_id) DO NOTHING
  |]

-- | Remove tag from show blog post.
removeTagFromShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
removeTagFromShowBlogPost postId tagId =
  interp
    False
    [sql|
    DELETE FROM show_blog_post_tags
    WHERE post_id = #{postId} AND tag_id = #{tagId}
  |]

-- | Get show blog posts with specific tag.
getShowBlogPostsByTag :: ShowBlogTags.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getShowBlogPostsByTag tagId (Limit lim) (Offset off) =
  interp
    False
    [sql|
    SELECT sbp.id, sbp.show_id, sbp.title, sbp.slug, sbp.content, sbp.excerpt, sbp.author_id, sbp.status, sbp.published_at, sbp.created_at, sbp.updated_at
    FROM show_blog_posts sbp
    JOIN show_blog_post_tags sbpt ON sbp.id = sbpt.post_id
    WHERE sbpt.tag_id = #{tagId} AND sbp.status = 'published'
    ORDER BY sbp.published_at DESC NULLS LAST, sbp.created_at DESC
    LIMIT #{lim} OFFSET #{off}
  |]

-- | Get show blog tags with their usage counts (only tags used by published posts).
--
-- Uses raw SQL because this query involves multiple joins and aggregation.
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

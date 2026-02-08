{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}

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
    getShowBlogPostById,
    insertShowBlogPost,
    updateShowBlogPost,
    deleteShowBlogPost,
    getPublishedShowBlogPostsBySlug,
    getPublishedShowBlogPostsByShowAndTag,
    countPublishedShowBlogPosts,
    countPublishedShowBlogPostsByTag,
    getTagsForShow,

    -- * Junction Table Queries (show_blog_post_tags)
    getTagsForShowBlogPost,
    addTagToShowBlogPost,
    removeTagFromShowBlogPost,
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
import Effects.Database.Tables.ShowBlogTags qualified as ShowBlogTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.Util (nextId)
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
-- Junction Table (show_blog_post_tags)

-- | The @show_blog_post_tags@ junction table definition (internal, not exported).
data ShowBlogPostTag f = ShowBlogPostTag
  { sbptPostId :: Column f Id,
    sbptTagId :: Column f ShowBlogTags.Id
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

-- | Schema for the @show_blog_post_tags@ junction table.
showBlogPostTagSchema :: TableSchema (ShowBlogPostTag Name)
showBlogPostTagSchema =
  TableSchema
    { name = "show_blog_post_tags",
      columns =
        ShowBlogPostTag
          { sbptPostId = "post_id",
            sbptTagId = "tag_id"
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
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (((.publishedAt) >$< nullsLast desc) <> ((.createdAt) >$< desc)) do
            post <- each showBlogPostSchema
            where_ $ (.showId) post ==. lit showIdVal
            where_ $ (.status) post ==. lit Published
            pure post

-- | Get show blog post by ID.
getShowBlogPostById :: Id -> Hasql.Statement () (Maybe Model)
getShowBlogPostById postId = fmap listToMaybe $ run $ select do
  post <- each showBlogPostSchema
  where_ $ (.id) post ==. lit postId
  pure post

-- | Insert a new show blog post.
insertShowBlogPost :: Insert -> Hasql.Statement () (Maybe Id)
insertShowBlogPost Insert {..} =
  fmap listToMaybe $
    run $
      insert
        Rel8.Insert
          { into = showBlogPostSchema,
            rows =
              values
                [ ShowBlogPost
                    { id = nextId "show_blog_posts_id_seq",
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
-- The @published_at@ logic mirrors the original SQL CASE:
--
-- - Setting status to Published when @published_at@ is NULL → set to NOW()
-- - Setting status to non-Published → clear @published_at@ to NULL
-- - Otherwise → keep existing @published_at@
updateShowBlogPost :: Id -> Insert -> Hasql.Statement () (Maybe Id)
updateShowBlogPost postId Insert {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = showBlogPostSchema,
            from = pure (),
            set = \_ post ->
              post
                { title = lit sbpiTitle,
                  slug = lit sbpiSlug,
                  content = lit sbpiContent,
                  excerpt = lit sbpiExcerpt,
                  status = lit sbpiStatus,
                  publishedAt =
                    caseExpr
                      [ (lit sbpiStatus ==. lit Published &&. isNull ((.publishedAt) post), nullify now),
                        (not_ (lit sbpiStatus ==. lit Published), Rel8.null)
                      ]
                      ((.publishedAt) post),
                  updatedAt = now
                },
            updateWhere = \_ post -> (.id) post ==. lit postId,
            returning = Returning (.id)
          }

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

-- | Get published blog posts for a show by show slug.
--
-- Joins with the shows table and excludes soft-deleted shows.
getPublishedShowBlogPostsBySlug :: Text -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedShowBlogPostsBySlug showSlug (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (((.publishedAt) >$< nullsLast desc) <> ((.createdAt) >$< desc)) do
            sbp <- each showBlogPostSchema
            s <- each Shows.showSchema
            where_ $ (.showId) sbp ==. (.id) s
            where_ $ (.slug) s ==. lit (Slug showSlug)
            where_ $ isNull ((.deletedAt) s)
            where_ $ (.status) sbp ==. lit Published
            pure sbp

-- | Get published blog posts for a show filtered by tag.
--
-- Joins with the junction table to filter by tag.
getPublishedShowBlogPostsByShowAndTag :: Shows.Id -> ShowBlogTags.Id -> Limit -> Offset -> Hasql.Statement () [Model]
getPublishedShowBlogPostsByShowAndTag showIdVal tagId (Limit lim) (Offset off) =
  run $
    select $
      Rel8.limit (fromIntegral lim) $
        Rel8.offset (fromIntegral off) $
          orderBy (((.publishedAt) >$< nullsLast desc) <> ((.createdAt) >$< desc)) do
            sbp <- each showBlogPostSchema
            sbpt <- each showBlogPostTagSchema
            where_ $ (.id) sbp ==. sbptPostId sbpt
            where_ $ (.showId) sbp ==. lit showIdVal
            where_ $ sbptTagId sbpt ==. lit tagId
            where_ $ (.status) sbp ==. lit Published
            pure sbp

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

-- | Get tags for a show blog post.
getTagsForShowBlogPost :: Id -> Hasql.Statement () [ShowBlogTags.Model]
getTagsForShowBlogPost postId =
  run $
    select $
      orderBy (ShowBlogTags.sbtmName >$< asc) do
        sbpt <- each showBlogPostTagSchema
        where_ $ sbptPostId sbpt ==. lit postId
        tag <- each ShowBlogTags.showBlogTagSchema
        where_ $ ShowBlogTags.sbtmId tag ==. sbptTagId sbpt
        pure tag

-- | Add tag to show blog post.
addTagToShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
addTagToShowBlogPost postId tagId =
  run_ $
    insert
      Rel8.Insert
        { into = showBlogPostTagSchema,
          rows = values [ShowBlogPostTag {sbptPostId = lit postId, sbptTagId = lit tagId}],
          onConflict = DoNothing,
          returning = NoReturning
        }

-- | Remove tag from show blog post.
removeTagFromShowBlogPost :: Id -> ShowBlogTags.Id -> Hasql.Statement () ()
removeTagFromShowBlogPost postId tagId =
  run_ $
    delete
      Rel8.Delete
        { from = showBlogPostTagSchema,
          using = pure (),
          deleteWhere = \_ sbpt ->
            sbptPostId sbpt ==. lit postId &&. sbptTagId sbpt ==. lit tagId,
          returning = NoReturning
        }

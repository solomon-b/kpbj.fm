module Domain.Types.ShowBlog where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Servant qualified

--------------------------------------------------------------------------------

data ShowBlogStatus = BlogDraft | BlogPublished | BlogArchived
  deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------

data ShowBlogPost = ShowBlogPost
  { blogPostId :: Int64,
    blogPostShowId :: Int64,
    blogPostTitle :: Text,
    blogPostSlug :: Text,
    blogPostContent :: Text,
    blogPostExcerpt :: Maybe Text,
    blogPostAuthorId :: Int64,
    blogPostStatus :: ShowBlogStatus,
    blogPostPublishedAt :: Maybe UTCTime,
    blogPostCreatedAt :: UTCTime,
    blogPostUpdatedAt :: UTCTime
  }
  deriving (Show, Eq)

data ShowBlogTag = ShowBlogTag
  { blogTagId :: Int64,
    blogTagName :: Text,
    blogTagCreatedAt :: UTCTime
  }
  deriving (Show, Eq)

data ShowBlogPostTag = ShowBlogPostTag
  { postTagPostId :: Int64,
    postTagTagId :: Int64
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

instance Servant.FromHttpApiData ShowBlogStatus where
  parseQueryParam = \case
    "draft" -> Right BlogDraft
    "published" -> Right BlogPublished
    "archived" -> Right BlogArchived
    _ -> Left "Invalid blog post status"

instance Servant.ToHttpApiData ShowBlogStatus where
  toQueryParam = \case
    BlogDraft -> "draft"
    BlogPublished -> "published"
    BlogArchived -> "archived"

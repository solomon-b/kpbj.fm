{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.BlogTags where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..), OneRow (..), interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Database Model

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

-- | Database Model for the @blog_tags@ table
data Model = Model
  { btmId :: Id,
    btmName :: Text,
    btmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

-- | Tag with count for queries (flattened for easier decoding)
data BlogTagWithCount = BlogTagWithCount
  { btwcId :: Id,
    btwcName :: Text,
    btwcCreatedAt :: UTCTime,
    btwcCount :: Int64
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance BlogTagWithCount)

--------------------------------------------------------------------------------
-- Insert Types

newtype Insert = Insert {btiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via Insert
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all tags
getAllTags :: Hasql.Statement () [Model]
getAllTags =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM blog_tags
    ORDER BY name
  |]

-- | Get tag by name
getTagByName :: Text -> Hasql.Statement () (Maybe Model)
getTagByName tagName =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM blog_tags
    WHERE name = #{tagName}
  |]

-- | Insert a new tag
insertTag :: Insert -> Hasql.Statement () Id
insertTag Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO blog_tags(name, created_at)
    VALUES (#{btiName}, NOW())
    RETURNING id
  |]

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

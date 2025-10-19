{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowBlogTags where

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

data Model = Model
  { sbtmId :: Id,
    sbtmName :: Text,
    sbtmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

newtype Insert = Insert {sbtiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get all show blog tags
getAllShowBlogTags :: Hasql.Statement () [Model]
getAllShowBlogTags =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM show_blog_tags
    ORDER BY name
  |]

-- | Get show blog tag by name
getShowBlogTagByName :: Text -> Hasql.Statement () (Maybe Model)
getShowBlogTagByName tagName =
  interp
    False
    [sql|
    SELECT id, name, created_at
    FROM show_blog_tags
    WHERE name = #{tagName}
  |]

-- | Insert a new show blog tag
insertShowBlogTag :: Insert -> Hasql.Statement () Id
insertShowBlogTag Insert {..} =
  getOneRow
    <$> interp
      False
      [sql|
    INSERT INTO show_blog_tags(name, created_at)
    VALUES (#{sbtiName}, NOW())
    RETURNING id
  |]

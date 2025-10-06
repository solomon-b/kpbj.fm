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
-- ID Types

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

data ShowBlogTagModel = ShowBlogTagModel
  { sbtmId :: ShowBlogTagId,
    sbtmName :: Text,
    sbtmCreatedAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagModel)

--------------------------------------------------------------------------------
-- Insert Types

newtype ShowBlogTagInsert = ShowBlogTagInsert {sbtiName :: Text}
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance ShowBlogTagInsert)

--------------------------------------------------------------------------------
-- Database Queries

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

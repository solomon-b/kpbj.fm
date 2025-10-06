{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.HostDetails where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import GHC.Generics
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.UTCTime ()

--------------------------------------------------------------------------------
-- Host Details Model

data HostDetailsModel = HostDetailsModel
  { id :: Int64,
    userId :: User.Id,
    bio :: Maybe Text,
    websiteUrl :: Maybe Text,
    instagramHandle :: Maybe Text,
    twitterHandle :: Maybe Text,
    soundcloudUrl :: Maybe Text,
    bandcampUrl :: Maybe Text,
    createdAt :: UTCTime,
    updatedAt :: UTCTime
  }
  deriving stock (Show, Generic, Eq)
  deriving (Display) via (RecordInstance HostDetailsModel)
  deriving anyclass (DecodeRow, FromJSON, ToJSON)

--------------------------------------------------------------------------------
-- Database Queries

-- | Get host details for a user
getHostDetailsByUserId :: User.Id -> Hasql.Statement () (Maybe HostDetailsModel)
getHostDetailsByUserId userId =
  interp
    False
    [sql|
    SELECT id, user_id, bio, website_url, instagram_handle, twitter_handle, soundcloud_url, bandcamp_url, created_at, updated_at
    FROM host_details
    WHERE user_id = #{userId}
  |]

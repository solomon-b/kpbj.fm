{-# LANGUAGE DuplicateRecordFields #-}

module Effects.Database.Tables.EpisodeTrack where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text.Display (Display, RecordInstance (..))
import Data.Time (UTCTime)
import {-# SOURCE #-} Effects.Database.Tables.Episodes qualified as Episodes
import GHC.Generics
import Hasql.Interpolate (DecodeRow, DecodeValue (..), EncodeRow, EncodeValue (..))
import OrphanInstances.UTCTime ()
import Servant qualified

--------------------------------------------------------------------------------
-- Database Models

newtype Id = Id Int64
  --------------------------------------------------------------------------------
  -- Update Types
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
    episodeId :: Episodes.Id,
    trackNumber :: Int64,
    title :: Text,
    artist :: Text,
    album :: Maybe Text,
    year :: Maybe Int64,
    duration :: Maybe Text,
    label :: Maybe Text,
    isExclusivePremiere :: Bool,
    createdAt :: UTCTime
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (DecodeRow)
  deriving (Display) via (RecordInstance Model)

data Insert = Insert
  { etiEpisodeId :: Episodes.Id,
    etiTrackNumber :: Int64,
    etiTitle :: Text,
    etiArtist :: Text,
    etiAlbum :: Maybe Text,
    etiYear :: Maybe Int64,
    etiDuration :: Maybe Text,
    etiLabel :: Maybe Text,
    etiIsExclusivePremiere :: Bool
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (EncodeRow)
  deriving (Display) via (RecordInstance Insert)

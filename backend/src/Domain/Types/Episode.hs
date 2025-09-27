module Domain.Types.Episode where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Servant qualified

--------------------------------------------------------------------------------

data EpisodeStatus = Draft | Scheduled | Published | Archived
  deriving (Show, Eq, Enum, Bounded)

--------------------------------------------------------------------------------

data Episode = Episode
  { episodeId :: Int64,
    episodeShowId :: Int64,
    episodeTitle :: Text,
    episodeSlug :: Text,
    episodeDescription :: Maybe Text,
    episodeNumber :: Maybe Int,
    episodeSeasonNumber :: Int,
    episodeAudioFilePath :: Maybe Text,
    episodeAudioFileSize :: Maybe Int64,
    episodeAudioMimeType :: Maybe Text,
    episodeDurationSeconds :: Maybe Int,
    episodeArtworkUrl :: Maybe Text,
    episodeScheduledAt :: Maybe UTCTime,
    episodePublishedAt :: Maybe UTCTime,
    episodeStatus :: EpisodeStatus,
    episodeCreatedBy :: Int64,
    episodeCreatedAt :: UTCTime,
    episodeUpdatedAt :: UTCTime
  }
  deriving (Show, Eq)

data EpisodeTrack = EpisodeTrack
  { trackId :: Int64,
    trackEpisodeId :: Int64,
    trackNumber :: Int,
    trackTitle :: Text,
    trackArtist :: Text,
    trackAlbum :: Maybe Text,
    trackYear :: Maybe Int,
    trackDuration :: Maybe Text,
    trackLabel :: Maybe Text,
    trackIsExclusivePremiere :: Bool,
    trackCreatedAt :: UTCTime
  }
  deriving (Show, Eq)

data FileUpload = FileUpload
  { uploadOriginalName :: Text,
    uploadTempPath :: Text,
    uploadMimeType :: Text,
    uploadFileSize :: Int64,
    uploadStoragePath :: Text -- Final storage path in /tmp/kpbj/...
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------

instance Servant.FromHttpApiData EpisodeStatus where
  parseQueryParam = \case
    "draft" -> Right Draft
    "scheduled" -> Right Scheduled
    "published" -> Right Published
    "archived" -> Right Archived
    _ -> Left "Invalid episode status"

instance Servant.ToHttpApiData EpisodeStatus where
  toQueryParam = \case
    Draft -> "draft"
    Scheduled -> "scheduled"
    Published -> "published"
    Archived -> "archived"

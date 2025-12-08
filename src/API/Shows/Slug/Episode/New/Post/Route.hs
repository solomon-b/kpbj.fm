module API.Shows.Slug.Episode.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.Slug (Slug)
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/episodes/new"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem EpisodeUploadForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------
-- Form Data Types

data TrackInfo = TrackInfo
  { tiTitle :: Text,
    tiArtist :: Text,
    tiAlbum :: Maybe Text,
    tiYear :: Maybe Int64,
    tiDuration :: Maybe Text,
    tiLabel :: Maybe Text,
    tiIsExclusive :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

data EpisodeUploadForm = EpisodeUploadForm
  { -- Show and scheduling
    eufId :: Text,
    eufScheduledDate :: Maybe Text,
    -- Episode metadata
    eufTitle :: Text,
    eufDescription :: Text,
    eufTags :: Maybe Text,
    eufDurationSeconds :: Maybe Text, -- Duration from browser audio detection
    -- Publishing status
    eufStatus :: Text, -- "draft" or "published"
    -- Track data (JSON encoded)
    eufTracksJson :: Maybe Text,
    -- File uploads
    eufAudioFile :: Maybe (FileData Mem),
    eufArtworkFile :: Maybe (FileData Mem)
  }
  deriving stock (Show, Generic, Eq)

instance FromMultipart Mem EpisodeUploadForm where
  fromMultipart multipartData =
    EpisodeUploadForm
      <$> lookupInput "show_id" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "scheduled_date" multipartData))
      <*> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "duration_seconds" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tracks_json" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "audio_file" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "artwork_file" multipartData))
    where
      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

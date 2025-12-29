module API.Dashboard.Episodes.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FileData,
    FromMultipart,
    Mem,
    MultipartForm,
    fdFileName,
    fromMultipart,
    lookupFile,
    lookupInput,
  )
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Form data for episode editing
data EpisodeEditForm = EpisodeEditForm
  { eefDescription :: Maybe Text,
    eefTags :: Maybe Text, -- Comma-separated list of tags
    eefStatus :: Text,
    eefScheduledDate :: Maybe Text, -- Format: "template_id|scheduled_at"
    eefTracksJson :: Maybe Text, -- JSON array of tracks
    -- File uploads (optional - only present if scheduled date is in the future)
    eefAudioFile :: Maybe (FileData Mem),
    eefArtworkFile :: Maybe (FileData Mem)
  }
  deriving (Show)

-- | Track data from JSON submission.
--
-- The field names use @ti@ prefix to match the JavaScript JSON format.
data TrackInfo = TrackInfo
  { tiTitle :: Text,
    tiArtist :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

instance FromMultipart Mem EpisodeEditForm where
  fromMultipart multipartData = do
    let description = either (const Nothing) Just (lookupInput "description" multipartData)
    let tags = either (const Nothing) Just (lookupInput "tags" multipartData)
    status <- lookupInput "status" multipartData
    -- Parse scheduled date (optional) - format: "template_id|scheduled_at"
    let scheduledDate = either (const Nothing) Just (lookupInput "scheduled_date" multipartData)
    -- Parse tracks JSON (optional) - JSON array of {tiTitle, tiArtist} objects
    let tracksJson = either (const Nothing) Just (lookupInput "tracks_json" multipartData)
    -- File lookups - these must be done with lookupFile, not lookupInput
    let audioFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "audio_file" multipartData)
        artworkFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "artwork_file" multipartData)

    pure
      EpisodeEditForm
        { eefDescription = emptyToNothing description,
          eefTags = emptyToNothing tags,
          eefStatus = status,
          eefScheduledDate = emptyToNothing scheduledDate,
          eefTracksJson = emptyToNothing tracksJson,
          eefAudioFile = audioFile,
          eefArtworkFile = artworkFile
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

-- | Parse episode status from text
parseStatus :: Text -> Maybe Episodes.Status
parseStatus "draft" = Just Episodes.Draft
parseStatus "published" = Just Episodes.Published
parseStatus "deleted" = Just Episodes.Deleted
parseStatus _ = Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/episodes/:show_slug/:episode_number/edit"
    ( "dashboard"
        :> "episodes"
        :> Servant.Capture "show_slug" Slug
        :> Servant.Capture "episode_number" Episodes.EpisodeNumber
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem EpisodeEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

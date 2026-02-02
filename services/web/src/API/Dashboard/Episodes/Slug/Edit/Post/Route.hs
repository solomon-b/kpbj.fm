module API.Dashboard.Episodes.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Episodes qualified as Episodes (EpisodeNumber)
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
    eefScheduledDate :: Maybe Text, -- Format: "template_id|scheduled_at"
    eefTracksJson :: Maybe Text, -- JSON array of tracks
    eefDurationSeconds :: Maybe Text, -- Duration from browser audio detection
    -- File uploads
    eefArtworkFile :: Maybe (FileData Mem), -- Direct upload only (small files)
    eefAudioClear :: Bool, -- True if user explicitly removed the audio
    eefArtworkClear :: Bool, -- True if user explicitly removed the artwork
    -- Staged upload tokens
    eefAudioToken :: Maybe Text -- Audio uploaded via XHR, token claimed on form submit
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
    -- Parse scheduled date (optional) - format: "template_id|scheduled_at"
    let scheduledDate = either (const Nothing) Just (lookupInput "scheduled_date" multipartData)
    -- Parse tracks JSON (optional) - JSON array of {tiTitle, tiArtist} objects
    let tracksJson = either (const Nothing) Just (lookupInput "tracks_json" multipartData)
    -- Parse duration seconds (optional) - extracted from audio metadata in browser
    let durationSeconds = either (const Nothing) Just (lookupInput "duration_seconds" multipartData)
    -- File lookups - artwork only (audio uses staged uploads)
    let artworkFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "episode_artwork" multipartData)
        audioClear = parseClearFlag "episode_audio_clear" multipartData
        artworkClear = parseClearFlag "episode_artwork_clear" multipartData
    -- Staged upload tokens (audio uploaded via XHR, token claimed on form submit)
    let audioToken = either (const Nothing) nonEmptyText (lookupInput "episode_audio_token" multipartData)

    pure
      EpisodeEditForm
        { eefDescription = emptyToNothing description,
          eefTags = emptyToNothing tags,
          eefScheduledDate = emptyToNothing scheduledDate,
          eefTracksJson = emptyToNothing tracksJson,
          eefDurationSeconds = emptyToNothing durationSeconds,
          eefArtworkFile = artworkFile,
          eefAudioClear = audioClear,
          eefArtworkClear = artworkClear,
          eefAudioToken = audioToken
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

      -- \| Convert empty text to Nothing
      nonEmptyText :: Text -> Maybe Text
      nonEmptyText t
        | Text.null t = Nothing
        | otherwise = Just t

      -- \| Parse a clear flag (true if the hidden input has value "true")
      parseClearFlag :: Text -> a -> Bool
      parseClearFlag name _mp = case lookupInput name multipartData of
        Right "true" -> True
        _ -> False

--------------------------------------------------------------------------------

-- | "POST /dashboard/episodes/:show_slug/:episode_number/edit"
type Route =
  "dashboard"
    :> "episodes"
    :> Servant.Capture "show_slug" Slug
    :> Servant.Capture "episode_number" Episodes.EpisodeNumber
    :> "edit"
    :> Servant.Header "Cookie" Cookie
    :> MultipartForm Mem EpisodeEditForm
    :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))

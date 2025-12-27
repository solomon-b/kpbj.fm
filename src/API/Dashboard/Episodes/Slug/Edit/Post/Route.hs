module API.Dashboard.Episodes.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Observability qualified as Observability
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
import Servant.Multipart qualified
import Text.HTML (HTML)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- | Form data for episode editing
data EpisodeEditForm = EpisodeEditForm
  { eefDescription :: Maybe Text,
    eefTags :: Maybe Text, -- Comma-separated list of tags
    eefStatus :: Text,
    eefScheduledDate :: Maybe Text, -- Format: "template_id|scheduled_at"
    eefTracks :: [TrackInfo],
    -- File uploads (optional - only present if scheduled date is in the future)
    eefAudioFile :: Maybe (FileData Mem),
    eefArtworkFile :: Maybe (FileData Mem)
  }
  deriving (Show)

-- | Track data from form submission (includes ID for existing tracks)
data TrackInfo = TrackInfo
  { tiId :: Maybe EpisodeTrack.Id, -- Existing track ID (Nothing for new tracks)
    tiTrackNumber :: Int64,
    tiTitle :: Text,
    tiArtist :: Text
  }
  deriving (Show)

instance FromMultipart Mem EpisodeEditForm where
  fromMultipart multipartData = do
    let description = either (const Nothing) Just (lookupInput "description" multipartData)
    let tags = either (const Nothing) Just (lookupInput "tags" multipartData)
    status <- lookupInput "status" multipartData
    -- Parse scheduled date (optional) - format: "template_id|scheduled_at"
    let scheduledDate = either (const Nothing) Just (lookupInput "scheduled_date" multipartData)
    -- Parse tracks - using safe parsing that handles multipart correctly
    let tracks = parseTracksFromMultipart multipartData
    -- File lookups - these must be done with lookupFile, not lookupInput
    let audioFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "audio_file" multipartData)
        artworkFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "artwork_file" multipartData)

    pure
      EpisodeEditForm
        { eefDescription = emptyToNothing description,
          eefTags = emptyToNothing tags,
          eefStatus = status,
          eefScheduledDate = emptyToNothing scheduledDate,
          eefTracks = tracks,
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

-- | Parse track data from multipart form fields like tracks[0][title], tracks[0][artist], etc.
-- We try to parse tracks by checking indices starting from 0 until we can't find a title field.
-- This uses the `inputs` list from MultipartData which only contains text inputs, not file data.
parseTracksFromMultipart :: Servant.Multipart.MultipartData Mem -> [TrackInfo]
parseTracksFromMultipart multipartData = parseTracksFromIndex 0
  where
    -- Get all inputs as a map for efficient lookup
    -- This is safer than using lookupInput because we can filter and handle errors
    inputsMap :: [(Text, Text)]
    inputsMap = [(Servant.Multipart.iName inp, Servant.Multipart.iValue inp) | inp <- Servant.Multipart.inputs multipartData]

    lookupInputSafe :: Text -> Maybe Text
    lookupInputSafe key = lookup key inputsMap

    parseTracksFromIndex :: Int -> [TrackInfo]
    parseTracksFromIndex idx =
      let prefix = "tracks[" <> Text.pack (show idx) <> "]"
          titleKey = prefix <> "[title]"
       in -- Try to parse title for this index - if it doesn't exist, we're done
          case lookupInputSafe titleKey of
            Nothing -> [] -- No more tracks
            Just _ ->
              -- Parse this track and remaining tracks
              parseTrack prefix : parseTracksFromIndex (idx + 1)

    parseTrack :: Text -> TrackInfo
    parseTrack prefix =
      let getField field = fromMaybe "" (lookupInputSafe (prefix <> "[" <> field <> "]"))
          getFieldMaybe field = lookupInputSafe (prefix <> "[" <> field <> "]")
          trackIdText = getFieldMaybe "id"
          trackId = trackIdText >>= (fmap EpisodeTrack.Id . readMaybe . Text.unpack)
          trackNumber = getField "track_number"
          title = getField "title"
          artist = getField "artist"
       in TrackInfo
            { tiId = trackId,
              tiTrackNumber = fromMaybe 1 (readMaybe $ Text.unpack trackNumber),
              tiTitle = title,
              tiArtist = artist
            }

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

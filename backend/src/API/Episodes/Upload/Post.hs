module API.Episodes.Upload.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeUploadPostLink)
import App.Auth qualified as Auth
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as EpisodeDB
import Effects.Database.Tables.Show qualified as ShowDB
import Effects.Database.Tables.User qualified as User
import Effects.FileUpload qualified as FileUpload
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Network.Wai.Parse (FileInfo (..), fileName)
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.OneRow ()
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileCType, fdFileName, fromMultipart, lookupInput)
import Text.HTML (HTML)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /episodes/upload"
    ( "episodes"
        :> "upload"
        :> Servant.Header "Cookie" Text
        :> MultipartForm Mem EpisodeUploadForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------
-- FileData to FileInfo conversion

convertFileDataToInfo :: FileData Mem -> FileInfo FilePath
convertFileDataToInfo fileData =
  FileInfo
    { fileName = Text.encodeUtf8 $ fdFileName fileData,
      fileContent = "/tmp/placeholder", -- This is a hack - we need the actual temp file path
      fileContentType = Text.encodeUtf8 $ fdFileCType fileData
    }

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
    eufShowId :: Text,
    eufScheduledDate :: Maybe Text,
    eufEpisodeType :: Text,
    -- Episode metadata
    eufTitle :: Text,
    eufDescription :: Text,
    eufTags :: Maybe Text,
    eufEpisodeNumber :: Maybe Text,
    eufSeasonNumber :: Maybe Text,
    -- Publishing action
    eufAction :: Text, -- "draft" or "publish"
    -- Track data (JSON encoded)
    eufTracksJson :: Maybe Text
  }
  deriving stock (Show, Generic, Eq)

instance FromMultipart Mem EpisodeUploadForm where
  fromMultipart multipartData =
    EpisodeUploadForm
      <$> lookupInput "show_id" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "scheduled_date" multipartData))
      <*> lookupInput "episode_type" multipartData
      <*> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "episode_number" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "season_number" multipartData))
      <*> lookupInput "action" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tracks_json" multipartData))

--------------------------------------------------------------------------------
-- URL helpers

episodeUploadPostUrl :: Links.URI
episodeUploadPostUrl = Links.linkURI episodeUploadPostLink

--------------------------------------------------------------------------------

-- | Process episode upload form
processEpisodeUpload ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  User.Model ->
  EpisodeUploadForm ->
  [FileData Mem] -> -- Audio and image files
  m (Either Text EpisodeDB.EpisodeId)
processEpisodeUpload user form fileInfos = do
  _currentTime <- liftIO getCurrentTime

  -- Parse and validate form data
  case parseFormData form of
    Left err -> pure $ Left err
    Right episodeData -> do
      -- Verify user is host of the show
      isHostResult <- execQuerySpan (ShowDB.isUserHostOfShow (User.mId user) (ShowDB.ShowId episodeData.showId))

      case isHostResult of
        Left _err -> pure $ Left "Database error checking host permissions"
        Right isHost ->
          if not isHost
            then pure $ Left "You are not authorized to create episodes for this show"
            else do
              -- Handle file uploads
              uploadResults <- processFileUploads episodeData.showSlug fileInfos

              case uploadResults of
                Left uploadErr -> pure $ Left uploadErr
                Right (audioPath, artworkPath) -> do
                  -- Create episode insert
                  let episodeInsert =
                        EpisodeDB.EpisodeInsert
                          { EpisodeDB.eiShowId = ShowDB.ShowId episodeData.showId,
                            EpisodeDB.eiTitle = episodeData.title,
                            EpisodeDB.eiSlug = generateEpisodeSlug episodeData.title,
                            EpisodeDB.eiDescription = episodeData.description,
                            EpisodeDB.eiEpisodeNumber = episodeData.episodeNumber,
                            EpisodeDB.eiSeasonNumber = fromMaybe 1 episodeData.seasonNumber,
                            EpisodeDB.eiAudioFilePath = audioPath,
                            EpisodeDB.eiAudioFileSize = Nothing, -- TODO: Get from upload
                            EpisodeDB.eiAudioMimeType = Nothing, -- TODO: Get from upload
                            EpisodeDB.eiDurationSeconds = Nothing, -- TODO: Calculate from file
                            EpisodeDB.eiArtworkUrl = artworkPath,
                            EpisodeDB.eiScheduledAt = episodeData.scheduledAt,
                            EpisodeDB.eiStatus = episodeData.status,
                            EpisodeDB.eiCreatedBy = User.mId user
                          }

                  -- Insert episode
                  episodeResult <- execQuerySpan (EpisodeDB.insertEpisode episodeInsert)

                  case episodeResult of
                    Left err -> do
                      Log.logInfo "Failed to insert episode" (Text.pack $ show err)
                      pure $ Left "Failed to create episode"
                    Right episodeId -> do
                      -- Insert tracks if provided
                      _ <- insertTracks episodeId episodeData.tracks
                      pure $ Right episodeId

-- | Parse form data into structured format
parseFormData :: EpisodeUploadForm -> Either Text ParsedEpisodeData
parseFormData form = do
  -- Parse show ID
  showId <- case readMaybe (Text.unpack (eufShowId form)) of
    Nothing -> Left "Invalid show ID"
    Just sid -> Right sid

  -- Parse scheduled date
  scheduledAt <- case eufScheduledDate form of
    Nothing -> Right Nothing
    Just dateStr -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack dateStr) of
      Nothing -> Left "Invalid scheduled date format"
      Just date -> Right (Just date)

  -- Parse episode number
  episodeNumber <- case eufEpisodeNumber form of
    Nothing -> Right Nothing
    Just numStr -> case readMaybe (Text.unpack numStr) of
      Nothing -> Left "Invalid episode number"
      Just num -> Right (Just (EpisodeDB.EpisodeNumber num))

  -- Parse season number
  seasonNumber <- case eufSeasonNumber form of
    Nothing -> Right Nothing
    Just numStr -> case readMaybe (Text.unpack numStr) of
      Nothing -> Left "Invalid season number"
      Just num -> Right (Just num)

  -- Determine episode status from action
  status <- case eufAction form of
    "draft" -> Right EpisodeDB.Draft
    "publish" -> Right EpisodeDB.Published
    _ -> Left "Invalid publish action"

  -- Parse tracks JSON
  tracks <- case eufTracksJson form of
    Nothing -> Right []
    Just tracksJson -> case Aeson.eitherDecodeStrict (Text.encodeUtf8 tracksJson) of
      Left err -> Left ("Invalid tracks JSON: " <> Text.pack err)
      Right trackList -> Right trackList

  Right $
    ParsedEpisodeData
      { showId = showId,
        showSlug = "unknown", -- TODO: Look up from database
        title = eufTitle form,
        description = Just (eufDescription form),
        episodeNumber = episodeNumber,
        seasonNumber = seasonNumber,
        scheduledAt = scheduledAt,
        status = status,
        tracks = tracks
      }

data ParsedEpisodeData = ParsedEpisodeData
  { showId :: Int64,
    showSlug :: Text,
    title :: Text,
    description :: Maybe Text,
    episodeNumber :: Maybe EpisodeDB.EpisodeNumber,
    seasonNumber :: Maybe Int64,
    scheduledAt :: Maybe UTCTime,
    status :: EpisodeDB.EpisodeStatus,
    tracks :: [TrackInfo]
  }
  deriving stock (Show, Eq)

-- | Process file uploads (audio and artwork)
processFileUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Text -> -- Show slug
  [FileData Mem] ->
  m (Either Text (Maybe Text, Maybe Text)) -- (audioPath, artworkPath)
processFileUploads showSlug fileInfos = do
  let audioFiles = filter isAudioFile fileInfos
      imageFiles = filter isImageFile fileInfos

  -- Process main audio file (required)
  audioResult <- case audioFiles of
    [] -> pure $ Right Nothing
    (audioFile : _) -> do
      -- Convert FileData to FileInfo (simplified approach)
      let fileInfo = convertFileDataToInfo audioFile
      result <- FileUpload.uploadEpisodeAudio showSlug fileInfo
      case result of
        Left err -> do
          Log.logInfo "Failed to upload audio file" (Text.pack $ show err)
          pure $ Left "Failed to upload audio file"
        Right uploadResult ->
          pure $ Right $ Just $ uploadResultStoragePath uploadResult

  -- Process artwork file (optional)
  artworkResult <- case imageFiles of
    [] -> pure $ Right Nothing
    (imageFile : _) -> do
      let fileInfo = convertFileDataToInfo imageFile
      result <- FileUpload.uploadEpisodeArtwork showSlug fileInfo
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Right Nothing -- Not critical, continue without artwork
        Right uploadResult ->
          pure $ Right $ Just $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right audioPath, Left _artworkErr) -> pure $ Right (Text.pack <$> audioPath, Nothing)
    (Right audioPath, Right artworkPath) -> pure $ Right (Text.pack <$> audioPath, Text.pack <$> artworkPath)

-- | Insert track listings for episode
insertTracks ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  EpisodeDB.EpisodeId ->
  [TrackInfo] ->
  m (Either Text [EpisodeDB.EpisodeTrackId])
insertTracks episodeId tracks = do
  results <- mapM (insertTrack episodeId) (zip [1 ..] tracks)
  let (errors, trackIds) = partitionEithers results

  if null errors
    then pure $ Right trackIds
    else do
      Log.logInfo "Some tracks failed to insert" errors
      pure $ Left "Failed to insert some tracks"
  where
    insertTrack ::
      ( MonadIO m,
        MonadDB m,
        Log.MonadLog m,
        MonadReader env m,
        MonadUnliftIO m,
        Has HSQL.Pool.Pool env,
        Has Tracer env
      ) =>
      EpisodeDB.EpisodeId ->
      (Int64, TrackInfo) ->
      m (Either Text EpisodeDB.EpisodeTrackId)
    insertTrack epId (trackNum, track) = do
      let trackInsert =
            EpisodeDB.EpisodeTrackInsert
              { EpisodeDB.etiEpisodeId = epId,
                EpisodeDB.etiTrackNumber = trackNum,
                EpisodeDB.etiTitle = tiTitle track,
                EpisodeDB.etiArtist = tiArtist track,
                EpisodeDB.etiAlbum = tiAlbum track,
                EpisodeDB.etiYear = tiYear track,
                EpisodeDB.etiDuration = tiDuration track,
                EpisodeDB.etiLabel = tiLabel track,
                EpisodeDB.etiIsExclusivePremiere = tiIsExclusive track
              }

      result <- execQuerySpan (EpisodeDB.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right trackId -> pure $ Right trackId

-- | Generate URL-friendly slug from episode title
generateEpisodeSlug :: Text -> Text
generateEpisodeSlug title =
  Text.toLower $
    Text.map (\c -> if c `elem` (" -_" :: String) then '-' else c) $
      Text.filter (\c -> c `elem` ("abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789 -_" :: String)) title

-- | File type helpers
isAudioFile :: FileData Mem -> Bool
isAudioFile fileData =
  let name = Text.toLower $ fdFileName fileData
   in any (`Text.isSuffixOf` name) [".mp3", ".wav", ".flac", ".aac", ".ogg", ".m4a"]

isImageFile :: FileData Mem -> Bool
isImageFile fileData =
  let name = Text.toLower $ fdFileName fileData
   in any (`Text.isSuffixOf` name) [".jpg", ".jpeg", ".png", ".webp", ".gif"]

-- | Success template
successTemplate :: EpisodeDB.EpisodeId -> Lucid.Html ()
successTemplate episodeId = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-600"] "Episode Uploaded Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6 text-gray-600"] $ do
      "Your episode has been created with ID: " <> Lucid.toHtml (show episodeId)
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Back to Dashboard"
      Lucid.a_ [Lucid.href_ "/episodes/upload", Lucid.class_ "border-2 border-gray-800 bg-white text-gray-800 px-6 py-3 font-bold hover:bg-gray-100"] "Upload Another"

-- | Error template
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-600"] "Upload Failed"
    Lucid.p_ [Lucid.class_ "mb-6 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_ [Lucid.href_ "/episodes/upload", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Try Again"
      Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "border-2 border-gray-800 bg-white text-gray-800 px-6 py-3 font-bold hover:bg-gray-100"] "Back to Dashboard"

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (ls, rs) = (a : ls, rs)
    right b (ls, rs) = (ls, b : rs)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Text ->
  EpisodeUploadForm ->
  m (Lucid.Html ())
handler _tracer cookie form = do
  -- Check authentication
  loginState <- Auth.userLoginState cookie

  case loginState of
    Auth.IsNotLoggedIn -> do
      -- Redirect to login
      loadFrame $ do
        Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
          Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to upload episodes."
          Lucid.a_ [Lucid.href_ "/user/login", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Login"
    Auth.IsLoggedIn user -> do
      -- For now, create empty file list since file upload integration needs more work
      let fileInfos = []

      -- Process upload
      result <- processEpisodeUpload user form fileInfos

      case result of
        Left err -> do
          Log.logInfo "Episode upload failed" err
          loadFrame $ errorTemplate err
        Right episodeId -> do
          Log.logInfo ("Episode uploaded successfully: " <> Text.pack (show episodeId)) ()
          loadFrame $ successTemplate episodeId

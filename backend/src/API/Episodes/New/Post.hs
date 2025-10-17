module API.Episodes.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesNewPostLink)
import API.Episodes.New.Post.Templates.Result (errorTemplate, successTemplate)
import App.Common (getUserInfo)
import Component.Frame (loadFrame)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Has (Has)
import Data.Int (Int64)
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTracks
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
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
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileCType, fdFileName, fdPayload, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/episodes/new"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem EpisodeUploadForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------
-- FileData to FileInfo conversion

-- | Convert FileData to temporary file for processing
convertFileDataToTempFile :: (MonadIO m) => FileData Mem -> m (Either Text FilePath)
convertFileDataToTempFile fileData = liftIO $ do
  let content = BSL.toStrict $ fdPayload fileData
      tempName = "/tmp/kpbj-upload-" <> Text.unpack (fdFileName fileData)
  -- Write directly to temp file
  BS.writeFile tempName content
  pure $ Right tempName

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
    eufEpisodeType :: Text,
    -- Episode metadata
    eufTitle :: Text,
    eufDescription :: Text,
    eufTags :: Maybe Text,
    eufDurationSeconds :: Maybe Text, -- Duration from browser audio detection
    -- Publishing action
    eufAction :: Text, -- "draft" or "publish"
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
      <*> lookupInput "episode_type" multipartData
      <*> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "duration_seconds" multipartData))
      <*> lookupInput "action" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tracks_json" multipartData))
      <*> pure (either (const Nothing) Just (lookupFile "audio_file" multipartData))
      <*> pure (either (const Nothing) Just (lookupFile "artwork_file" multipartData))

--------------------------------------------------------------------------------
-- URL helpers

episodesNewPostUrl :: Slug -> Links.URI
episodesNewPostUrl showSlug = Links.linkURI $ episodesNewPostLink showSlug

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
  Slug ->
  Maybe Cookie ->
  EpisodeUploadForm ->
  m (Lucid.Html ())
handler _tracer showSlug cookie form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      loadFrame $ do
        Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
          Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to upload episodes."
          Lucid.a_ [Lucid.href_ "/user/login", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Login"
    Just (user, _userInfo) -> do
      -- Verify show exists and user is authorized
      showResult <- execQuerySpan (Shows.getShowBySlug showSlug)
      case showResult of
        Left _err -> do
          Log.logInfo "Failed to fetch show" showSlug
          loadFrame $ errorTemplate "Failed to load show information."
        Right Nothing -> do
          Log.logInfo "Show not found" showSlug
          loadFrame $ errorTemplate "Show not found."
        Right (Just showModel) -> do
          -- Verify user is host
          isHostResult <- execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id)
          case isHostResult of
            Left _err -> do
              Log.logInfo "Failed to check host permissions" showSlug
              loadFrame $ errorTemplate "Failed to verify permissions."
            Right False -> do
              Log.logInfo "User is not host of show" (user.mId, showSlug)
              loadFrame $ errorTemplate "You are not authorized to upload episodes for this show."
            Right True -> do
              -- Process upload
              result <- processEpisodeUpload user form

              case result of
                Left err -> do
                  Log.logInfo "Episode upload failed" err
                  loadFrame $ errorTemplate err
                Right episodeId -> do
                  Log.logInfo ("Episode uploaded successfully: " <> display episodeId) ()
                  loadFrame $ successTemplate episodeId

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
  m (Either Text Episodes.Id)
processEpisodeUpload user form = do
  _currentTime <- liftIO getCurrentTime

  -- Debug logging for duration
  Log.logInfo "Duration from form" (Text.pack $ show $ eufDurationSeconds form)

  -- Parse show ID first
  case readMaybe (Text.unpack (eufId form)) of
    Nothing -> pure $ Left "Invalid show ID"
    Just showIdInt -> do
      let showId = Shows.Id showIdInt

      -- Look up show to get slug
      showResult <- execQuerySpan (Shows.getShowById showId)

      case showResult of
        Left _err -> pure $ Left "Database error looking up show"
        Right Nothing -> pure $ Left "Show not found"
        Right (Just showModel) -> do
          -- Parse remaining form data
          case parseFormDataWithShow showModel.id showModel.slug form of
            Left err -> pure $ Left err
            Right episodeData -> do
              -- Verify user is host of the show
              isHostResult <- execQuerySpan (Shows.isUserHostOfShow (User.mId user) (Shows.Id episodeData.showId))

              case isHostResult of
                Left _err -> pure $ Left "Database error checking host permissions"
                Right isHost ->
                  if not isHost
                    then pure $ Left "You are not authorized to create episodes for this show"
                    else do
                      -- Generate episode slug for filename
                      let episodeSlug = Slug.mkSlug episodeData.title
                      -- Handle file uploads (pass scheduled date for file organization)
                      uploadResults <- processFileUploads episodeData.showSlug episodeSlug episodeData.scheduledAt (eufAudioFile form) (eufArtworkFile form)

                      case uploadResults of
                        Left uploadErr -> pure $ Left uploadErr
                        Right (audioPath, artworkPath) -> do
                          -- Create episode insert
                          let episodeInsert =
                                Episodes.Insert
                                  { Episodes.eiId = Shows.Id episodeData.showId,
                                    Episodes.eiTitle = episodeData.title,
                                    Episodes.eiSlug = Slug.mkSlug episodeData.title,
                                    Episodes.eiDescription = episodeData.description,
                                    Episodes.eiAudioFilePath = audioPath,
                                    Episodes.eiAudioFileSize = Nothing, -- TODO: Get from upload
                                    Episodes.eiAudioMimeType = Nothing, -- TODO: Get from upload
                                    Episodes.eiDurationSeconds = episodeData.durationSeconds,
                                    Episodes.eiArtworkUrl = artworkPath,
                                    Episodes.eiScheduledAt = episodeData.scheduledAt,
                                    Episodes.eiStatus = episodeData.status,
                                    Episodes.eiCreatedBy = User.mId user
                                  }

                          -- Insert episode
                          episodeResult <- execQuerySpan (Episodes.insertEpisode episodeInsert)

                          case episodeResult of
                            Left err -> do
                              Log.logInfo "Failed to insert episode" (Text.pack $ show err)
                              pure $ Left "Failed to create episode"
                            Right episodeId -> do
                              -- Insert tracks if provided
                              _ <- insertTracks episodeId episodeData.tracks
                              pure $ Right episodeId

-- | Parse form data into structured format with show info
parseFormDataWithShow :: Shows.Id -> Slug -> EpisodeUploadForm -> Either Text ParsedEpisodeData
parseFormDataWithShow (Shows.Id showId) showSlug form = do
  -- Parse scheduled date
  scheduledAt <- case eufScheduledDate form of
    Nothing -> Right Nothing
    Just dateStr -> case parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack dateStr) of
      Nothing -> Left "Invalid scheduled date format"
      Just date -> Right (Just date)

  -- Determine episode status from action
  status <- case eufAction form of
    "draft" -> Right Episodes.Draft
    "publish" -> Right Episodes.Published
    _ -> Left "Invalid publish action"

  -- Parse tracks JSON
  tracks <- case eufTracksJson form of
    Nothing -> Right []
    Just tracksJson -> case Aeson.eitherDecodeStrict (Text.encodeUtf8 tracksJson) of
      Left err -> Left ("Invalid tracks JSON: " <> Text.pack err)
      Right trackList -> Right trackList

  -- Parse duration seconds (with debugging)
  let durationSeconds = case eufDurationSeconds form of
        Nothing -> Nothing
        Just "" -> Nothing
        Just durStr -> readMaybe (Text.unpack durStr)

  Right $
    ParsedEpisodeData
      { showId = showId,
        showSlug = showSlug,
        title = eufTitle form,
        description = Just (eufDescription form),
        scheduledAt = scheduledAt,
        status = status,
        tracks = tracks,
        durationSeconds = durationSeconds
      }

data ParsedEpisodeData = ParsedEpisodeData
  { showId :: Int64,
    showSlug :: Slug,
    title :: Text,
    description :: Maybe Text,
    scheduledAt :: Maybe UTCTime,
    status :: Episodes.Status,
    tracks :: [TrackInfo],
    durationSeconds :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | Process file uploads (audio and artwork)
processFileUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Slug -> -- Show slug
  Slug -> -- Episode slug (for filename)
  Maybe UTCTime -> -- Scheduled date (for file organization)
  Maybe (FileData Mem) -> -- Audio file
  Maybe (FileData Mem) -> -- Artwork file
  m (Either Text (Maybe Text, Maybe Text)) -- (audioPath, artworkPath)
processFileUploads showSlug episodeSlug mScheduledDate mAudioFile mArtworkFile = do
  -- Process main audio file (required)
  audioResult <- case mAudioFile of
    Nothing -> pure $ Right Nothing
    Just audioFile -> do
      -- Convert FileData to temp file
      tempFileResult <- convertFileDataToTempFile audioFile
      case tempFileResult of
        Left err -> pure $ Left err
        Right tempPath -> do
          let fileInfo =
                FileInfo
                  { fileName = Text.encodeUtf8 $ fdFileName audioFile,
                    fileContent = tempPath,
                    fileContentType = Text.encodeUtf8 $ fdFileCType audioFile
                  }
          result <- FileUpload.uploadEpisodeAudio showSlug episodeSlug mScheduledDate fileInfo
          case result of
            Left err -> do
              Log.logInfo "Failed to upload audio file" (Text.pack $ show err)
              pure $ Left "Failed to upload audio file"
            Right uploadResult ->
              pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  -- Process artwork file (optional)
  artworkResult <- case mArtworkFile of
    Nothing -> pure $ Right Nothing
    Just artworkFile -> do
      -- Convert FileData to temp file
      tempFileResult <- convertFileDataToTempFile artworkFile
      case tempFileResult of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" err
          pure $ Right Nothing -- Not critical, continue without artwork
        Right tempPath -> do
          let fileInfo =
                FileInfo
                  { fileName = Text.encodeUtf8 $ fdFileName artworkFile,
                    fileContent = tempPath,
                    fileContentType = Text.encodeUtf8 $ fdFileCType artworkFile
                  }
          result <- FileUpload.uploadEpisodeArtwork showSlug episodeSlug mScheduledDate fileInfo
          case result of
            Left err -> do
              Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
              pure $ Right Nothing -- Not critical, continue without artwork
            Right uploadResult ->
              pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right audioPath, Left _artworkErr) -> pure $ Right (audioPath, Nothing)
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

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
  Episodes.Id ->
  [TrackInfo] ->
  m (Either Text [EpisodeTracks.Id])
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
      Episodes.Id ->
      (Int64, TrackInfo) ->
      m (Either Text EpisodeTracks.Id)
    insertTrack epId (trackNum, track) = do
      let trackInsert =
            EpisodeTracks.Insert
              { EpisodeTracks.etiEpisodeId = epId,
                EpisodeTracks.etiTrackNumber = trackNum,
                EpisodeTracks.etiTitle = tiTitle track,
                EpisodeTracks.etiArtist = tiArtist track,
                EpisodeTracks.etiAlbum = tiAlbum track,
                EpisodeTracks.etiYear = tiYear track,
                EpisodeTracks.etiDuration = tiDuration track,
                EpisodeTracks.etiLabel = tiLabel track,
                EpisodeTracks.etiIsExclusivePremiere = tiIsExclusive track
              }

      result <- execQuerySpan (Episodes.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right trackId -> pure $ Right trackId

-- | Strip /tmp/kpbj/ prefix from storage path to get relative path for media serving
stripStorageRoot :: FilePath -> Text
stripStorageRoot path =
  let prefix = "/tmp/kpbj/"
   in case List.stripPrefix prefix path of
        Just relativePath -> Text.pack relativePath
        Nothing -> Text.pack path -- Fallback if prefix not found

-- | File type helpers
isAudioFile :: FileData Mem -> Bool
isAudioFile fileData =
  let name = Text.toLower $ fdFileName fileData
   in any (`Text.isSuffixOf` name) [".mp3", ".wav", ".flac", ".aac", ".ogg", ".m4a"]

isImageFile :: FileData Mem -> Bool
isImageFile fileData =
  let name = Text.toLower $ fdFileName fileData
   in any (`Text.isSuffixOf` name) [".jpg", ".jpeg", ".png", ".webp", ".gif"]

partitionEithers :: [Either a b] -> ([a], [b])
partitionEithers = foldr (either left right) ([], [])
  where
    left a (ls, rs) = (a : ls, rs)
    right b (ls, rs) = (ls, b : rs)

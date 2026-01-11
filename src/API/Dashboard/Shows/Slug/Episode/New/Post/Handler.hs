{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Episode.New.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Post.Route (EpisodeUploadForm (..), TrackInfo (..))
import API.Links (apiLinks, dashboardEpisodesLinks, dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTracks
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot)
import Effects.FileUpload qualified as FileUpload
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import OrphanInstances.OneRow ()
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)
import Text.Read (readMaybe)
import Utils (partitionEithers)

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
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug cookie form =
  handleRedirectErrors "Episode upload" apiLinks.rootGet $ do
    -- 1. Require authentication
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId showSlug userMetadata

    -- 2. Verify the show exists
    showModel <- fetchShowOrNotFound showSlug

    -- 3. Process the upload
    processEpisodeUploadAndRespond userMetadata user showModel form

-- | Fetch show by slug or throw NotFound
fetchShowOrNotFound ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    Has Tracer env
  ) =>
  Slug ->
  m Shows.Model
fetchShowOrNotFound showSlug =
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

-- | Process the episode upload and return appropriate response
processEpisodeUploadAndRespond ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  UserMetadata.Model ->
  User.Model ->
  Shows.Model ->
  EpisodeUploadForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
processEpisodeUploadAndRespond userMetadata user showModel form = do
  let newEpisodeUri = Links.linkURI $ dashboardShowsLinks.episodeNewGet showModel.slug
      newEpisodeUrl = [i|/#{newEpisodeUri}|] :: Text

  processEpisodeUpload userMetadata user showModel form >>= \case
    Left err -> do
      Log.logInfo "Episode upload failed" err
      let banner = BannerParams Error "Upload Failed" err
      pure $ Servant.addHeader (buildRedirectUrl newEpisodeUrl banner) (redirectWithBanner newEpisodeUrl banner)
    Right episodeId -> do
      Log.logInfo ("Episode uploaded successfully: " <> display episodeId) ()
      handleUploadSuccess showModel episodeId

-- | Handle successful upload by redirecting to episode page with banner
handleUploadSuccess ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  Shows.Model ->
  Episodes.Id ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handleUploadSuccess showModel episodeId = do
  -- Fetch the created episode
  execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
    Right (Just episode) -> do
      let detailLinkUri = Links.linkURI $ dashboardEpisodesLinks.detail showModel.slug episode.episodeNumber
          detailUrl = [i|/#{detailLinkUri}|] :: Text
          banner = BannerParams Success "Episode Uploaded" "Your episode has been uploaded successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
    _ -> do
      Log.logInfo_ "Created episode but failed to retrieve it"
      let showListUri = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug Nothing
          showListUrl = [i|/#{showListUri}|] :: Text
          banner = BannerParams Warning "Episode Created" "Episode was created but there was an error loading details."
          redirectUrl = buildRedirectUrl showListUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner showListUrl banner)

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
  UserMetadata.Model ->
  User.Model ->
  Shows.Model ->
  EpisodeUploadForm ->
  m (Either Text Episodes.Id)
processEpisodeUpload userMetadata user showModel form = do
  -- Debug logging for duration
  Log.logInfo "Duration from form" (Text.pack $ show $ eufDurationSeconds form)

  -- Parse remaining form data
  case parseFormDataWithShow showModel.id showModel.slug form of
    Left validationError -> pure $ Left $ Sanitize.displayContentValidationError validationError
    Right episodeData -> do
      -- Ensure only staff can publish (hosts can only create drafts)
      let allowPublish = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      if not allowPublish && episodeData.status /= Episodes.Draft
        then pure $ Left "Only staff can publish episodes"
        else do
          -- Verify user is host of the show (staff/admins can upload to any show)
          isAuthorized <-
            if UserMetadata.isStaffOrHigher userMetadata.mUserRole
              then pure (Right True) -- Staff and Admins always authorized
              else execQuerySpan (ShowHost.isUserHostOfShow (User.mId user) (Shows.Id episodeData.showId))

          case isAuthorized of
            Left _err -> pure $ Left "Database error checking host permissions"
            Right isHost ->
              if not isHost
                then pure $ Left "You are not authorized to create episodes for this show"
                else do
                  -- Generate a unique identifier for file naming (based on timestamp)
                  currentTime <- liftIO getCurrentTime
                  let fileId = Slug.mkSlug $ Text.pack $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime
                  -- Handle file uploads (pass scheduled date for file organization)
                  uploadResults <- processFileUploads episodeData.showSlug fileId (Just episodeData.scheduledAt) episodeData.status (eufAudioFile form) (eufArtworkFile form)

                  case uploadResults of
                    Left uploadErr -> pure $ Left uploadErr
                    Right (audioPath, artworkPath) -> do
                      -- Create episode insert
                      let episodeInsert =
                            Episodes.Insert
                              { Episodes.eiId = Shows.Id episodeData.showId,
                                Episodes.eiDescription = episodeData.description,
                                Episodes.eiAudioFilePath = audioPath,
                                Episodes.eiAudioFileSize = Nothing, -- TODO: Get from upload
                                Episodes.eiAudioMimeType = Nothing, -- TODO: Get from upload
                                Episodes.eiDurationSeconds = episodeData.durationSeconds,
                                Episodes.eiArtworkUrl = artworkPath,
                                Episodes.eiScheduleTemplateId = episodeData.scheduleTemplateId,
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
                          -- Replace tags with provided ones (single atomic query)
                          let tagNames = maybe [] parseTagList (eufTags form)
                          _ <- execQuerySpan (Episodes.replaceEpisodeTags episodeId tagNames)
                          pure $ Right episodeId

-- | Parse form data into structured format with show info
parseFormDataWithShow :: Shows.Id -> Slug -> EpisodeUploadForm -> Either Sanitize.ContentValidationError ParsedEpisodeData
parseFormDataWithShow (Shows.Id showId) showSlug form = do
  -- Parse schedule value (format: "template_id|scheduled_at")
  (scheduleTemplateId, scheduledAt) <- case eufScheduledDate form of
    Nothing -> Left $ Sanitize.ContentInvalid "Scheduled date is required"
    Just "" -> Left $ Sanitize.ContentInvalid "Scheduled date is required"
    Just scheduleStr -> parseScheduleValue scheduleStr

  -- Determine episode status
  status <- case eufStatus form of
    "draft" -> Right Episodes.Draft
    "published" -> Right Episodes.Published
    _ -> Left $ Sanitize.ContentInvalid "Invalid status value"

  -- Sanitize and validate episode metadata
  let sanitizedDescription = Sanitize.sanitizeUserContent (eufDescription form)

  -- Validate content lengths
  validDescription <- Sanitize.validateContentLength 10000 sanitizedDescription

  -- Parse and sanitize tracks JSON
  tracks <- case eufTracksJson form of
    Nothing -> Right []
    Just tracksJson -> case Aeson.eitherDecodeStrict (Text.encodeUtf8 tracksJson) of
      Left err -> Left $ Sanitize.ContentInvalid $ "Invalid tracks JSON: " <> Text.pack err
      Right trackList -> Right $ sanitizeTrackList trackList

  -- Parse duration seconds (with debugging)
  let durationSeconds = case eufDurationSeconds form of
        Nothing -> Nothing
        Just "" -> Nothing
        Just durStr -> readMaybe (Text.unpack durStr)

  Right $
    ParsedEpisodeData
      { showId = showId,
        showSlug = showSlug,
        description = Just validDescription,
        scheduleTemplateId = scheduleTemplateId,
        scheduledAt = scheduledAt,
        status = status,
        tracks = tracks,
        durationSeconds = durationSeconds
      }

-- | Parse schedule value from form (format: "template_id|scheduled_at")
parseScheduleValue :: Text -> Either Sanitize.ContentValidationError (ShowSchedule.TemplateId, UTCTime)
parseScheduleValue txt =
  case Text.splitOn "|" txt of
    [tidStr, timeStr] -> do
      tid <- case readMaybe (Text.unpack tidStr) of
        Just n -> Right $ ShowSchedule.TemplateId n
        Nothing -> Left $ Sanitize.ContentInvalid $ "Invalid template ID: " <> tidStr
      time <- case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" (Text.unpack timeStr) of
        Just t -> Right t
        Nothing -> Left $ Sanitize.ContentInvalid $ "Invalid timestamp: " <> timeStr
      Right (tid, time)
    _ -> Left $ Sanitize.ContentInvalid $ "Invalid schedule format: " <> txt

-- | Sanitize track list by sanitizing all text fields
sanitizeTrackList :: [TrackInfo] -> [TrackInfo]
sanitizeTrackList = map sanitizeTrack
  where
    sanitizeTrack track =
      TrackInfo
        { tiTitle = Sanitize.sanitizePlainText (tiTitle track),
          tiArtist = Sanitize.sanitizePlainText (tiArtist track)
        }

data ParsedEpisodeData = ParsedEpisodeData
  { showId :: Int64,
    showSlug :: Slug,
    description :: Maybe Text,
    scheduleTemplateId :: ShowSchedule.TemplateId,
    scheduledAt :: UTCTime,
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
  -- | Show slug
  Slug ->
  -- | Episode slug (for filename)
  Slug ->
  -- | Scheduled date (for file organization)
  Maybe UTCTime ->
  -- | Episode status (audio only required for published)
  Episodes.Status ->
  -- | Audio file
  Maybe (FileData Mem) ->
  -- | Artwork file
  Maybe (FileData Mem) ->
  -- | (audioPath, artworkPath)
  m (Either Text (Maybe Text, Maybe Text))
processFileUploads showSlug episodeSlug mScheduledDate status mAudioFile mArtworkFile = do
  -- Process main audio file (required for published, optional for draft)
  audioResult <- case mAudioFile of
    Nothing ->
      case status of
        Episodes.Draft -> pure $ Right Nothing -- Audio optional for drafts
        _ -> pure $ Left "Audio file is required for published episodes"
    Just audioFile -> do
      result <- FileUpload.uploadEpisodeAudio showSlug episodeSlug mScheduledDate audioFile
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
      result <- FileUpload.uploadEpisodeArtwork showSlug episodeSlug mScheduledDate artworkFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err -- Invalid file provided, reject entire operation
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
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
                EpisodeTracks.etiArtist = tiArtist track
              }

      result <- execQuerySpan (EpisodeTracks.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right trackId -> pure $ Right trackId

--------------------------------------------------------------------------------
-- Tag Processing

-- | Parse comma-separated tag list into individual tag names.
parseTagList :: Text -> [Text]
parseTagList = filter (not . Text.null) . map Text.strip . Text.splitOn ","

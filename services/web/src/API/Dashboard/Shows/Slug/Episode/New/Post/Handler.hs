{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Episode.New.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Post.Route (EpisodeUploadForm (..), TrackInfo (..))
import API.Links (dashboardEpisodesLinks, dashboardShowsLinks)
import API.Types
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as T (null)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTracks
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import OrphanInstances.OneRow ()
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)
import Text.Read (readMaybe)
import Utils (fromMaybeM, fromRightM, partitionEithers)

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Cookie ->
  EpisodeUploadForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler showSlug cookie form =
  handleRedirectErrors "Episode upload" (dashboardShowsLinks.episodeNewGet showSlug) $ do
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId showSlug userMetadata
    episodeId <- action user userMetadata showSlug form
    buildUploadRedirect showSlug episodeId

--------------------------------------------------------------------------------

-- | Business logic: fetch show, process upload.
--
-- Returns the created episode ID on success.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  EpisodeUploadForm ->
  ExceptT HandlerError AppM Episodes.Id
action user userMetadata showSlug form = do
  -- 1. Verify the show exists
  showModel <- fetchShowOrNotFound showSlug

  -- 2. Process the upload
  uploadResult <- lift $ processEpisodeUpload userMetadata user showModel form
  case uploadResult of
    Left err -> do
      Log.logInfo "Episode upload failed" err
      throwValidationError err
    Right episodeId -> do
      Log.logInfo ("Episode uploaded successfully: " <> display episodeId) ()
      pure episodeId

-- | Build redirect response after successful episode upload.
buildUploadRedirect ::
  Slug ->
  Episodes.Id ->
  ExceptT HandlerError AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
buildUploadRedirect showSlug episodeId = do
  fetchResult <- execQuery (Episodes.getEpisodeById episodeId)
  case fetchResult of
    Right (Just episode) -> do
      let detailLinkUri = Links.linkURI $ dashboardEpisodesLinks.detail showSlug episode.episodeNumber
          detailUrl = [i|/#{detailLinkUri}|] :: Text
          banner = BannerParams Success "Episode Uploaded" "Your episode has been uploaded successfully."
          redirectUrl = buildRedirectUrl detailUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
    _ -> do
      Log.logInfo_ "Created episode but failed to retrieve it"
      let showListUri = Links.linkURI $ dashboardEpisodesLinks.list showSlug Nothing
          showListUrl = [i|/#{showListUri}|] :: Text
          banner = BannerParams Warning "Episode Created" "Episode was created but there was an error loading details."
          redirectUrl = buildRedirectUrl showListUrl banner
      pure $ Servant.addHeader redirectUrl (redirectWithBanner showListUrl banner)

-- | Fetch show by slug or throw NotFound
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound showSlug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug showSlug)

-- | Process episode upload form
processEpisodeUpload ::
  UserMetadata.Model ->
  User.Model ->
  Shows.Model ->
  EpisodeUploadForm ->
  AppM (Either Text Episodes.Id)
processEpisodeUpload _userMetadata user showModel form = do
  -- Get storage configuration from context
  backend <- asks (Has.getter @StorageBackend)
  mAwsEnv <- asks (Has.getter @(Maybe AWS.Env))
  -- Debug logging for duration
  Log.logInfo "Duration from form" (Text.pack $ show $ eufDurationSeconds form)

  -- Parse remaining form data
  case parseFormDataWithShow ((.id) showModel) showModel.slug form of
    Left validationError -> pure $ Left $ Sanitize.displayContentValidationError validationError
    Right episodeData -> do
      -- Verify user is host of the show (staff/admins can upload to any show)
      isAuthorized <-
        if UserMetadata.isStaffOrHigher _userMetadata.mUserRole
          then pure (Right True) -- Staff and Admins always authorized
          else execQuery (ShowHost.isUserHostOfShow (User.mId user) (Shows.Id episodeData.showId))

      case isAuthorized of
        Left _err -> pure $ Left "Database error checking host permissions"
        Right isHost ->
          if not isHost
            then pure $ Left "You are not authorized to create episodes for this show"
            else do
              -- Handle file uploads (pass scheduled date for file organization)
              -- Audio: Claimed from staged upload via token (uploaded via XHR before form submission)
              -- Artwork: Direct upload only (small files don't benefit from staged uploads)
              uploadResults <- processFileUploads backend mAwsEnv (User.mId user) episodeData.showSlug (Just episodeData.scheduledAt) (eufArtworkFile form) (eufAudioToken form)

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
                            Episodes.eiCreatedBy = User.mId user
                          }

                  -- Insert episode
                  episodeResult <- execQuery (Episodes.insertEpisode episodeInsert)

                  case episodeResult of
                    Left err -> do
                      Log.logInfo "Failed to insert episode" (Text.pack $ show err)
                      pure $ Left "Failed to create episode"
                    Right Nothing -> do
                      Log.logInfo_ "Episode insert returned Nothing"
                      pure $ Left "Failed to create episode"
                    Right (Just episodeId) -> do
                      -- Insert tracks if provided
                      _ <- insertTracks episodeId episodeData.tracks
                      -- Replace tags with provided ones (single atomic query)
                      let tagNames = maybe [] parseTagList (eufTags form)
                      _ <- execQuery (Episodes.replaceEpisodeTags episodeId tagNames)
                      pure $ Right episodeId

-- | Parse form data into structured format with show info
parseFormDataWithShow :: Shows.Id -> Slug -> EpisodeUploadForm -> Either Sanitize.ContentValidationError ParsedEpisodeData
parseFormDataWithShow (Shows.Id showId) showSlug form = do
  -- Parse schedule value (format: "template_id|scheduled_at")
  (scheduleTemplateId, scheduledAt) <- case eufScheduledDate form of
    Nothing -> Left $ Sanitize.ContentInvalid "Scheduled date is required"
    Just "" -> Left $ Sanitize.ContentInvalid "Scheduled date is required"
    Just scheduleStr -> parseScheduleValue scheduleStr

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
    tracks :: [TrackInfo],
    durationSeconds :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | Process file uploads (audio and artwork)
--
-- Audio uses staged uploads - file is uploaded via XHR before form submission,
-- and we receive a token to claim the upload. The file is then moved from
-- the staging area to its final location based on the air date.
--
-- Artwork uses direct form submission (small files don't benefit from staged uploads).
--
-- Audio is always required for episode creation.
processFileUploads ::
  -- | Storage backend
  StorageBackend ->
  -- | AWS environment (for S3)
  Maybe AWS.Env ->
  -- | User ID (for claiming staged uploads)
  User.Id ->
  -- | Show slug (for file organization)
  Slug ->
  -- | Scheduled date (air date for file organization)
  Maybe UTCTime ->
  -- | Artwork file (direct upload)
  Maybe (FileData Mem) ->
  -- | Audio token (staged upload)
  Maybe Text ->
  -- | (audioPath, artworkPath)
  AppM (Either Text (Maybe Text, Maybe Text))
processFileUploads backend mAwsEnv userId showSlug mScheduledDate mArtworkFile mAudioToken = do
  -- Get the air date for file organization (use current time as fallback)
  airDate <- maybe currentSystemTime pure mScheduledDate

  -- Process main audio file (always required)
  -- Audio is uploaded via staged upload, then moved to final location with air date
  audioResult <- case mAudioToken of
    Just token | not (T.null token) -> do
      -- Claim and relocate to audio/{air_date}/episodes/{show-slug}-{random}.ext
      result <-
        claimAndRelocateUpload
          userId
          token
          StagedUploads.EpisodeAudio
          AudioBucket
          EpisodeAudio
          airDate
          (display showSlug)
      pure $ case result of
        Left err -> Left err
        Right path -> Right (Just path)
    _ -> pure $ Left "Audio file is required"

  -- Process artwork file (optional, direct upload only)
  artworkResult <- case mArtworkFile of
    Nothing -> pure $ Right Nothing
    Just artworkFile -> do
      result <- FileUpload.uploadEpisodeArtwork backend mAwsEnv showSlug mScheduledDate artworkFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err -- Invalid file provided, reject entire operation
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
          pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right audioPath, Left _artworkErr) -> pure $ Right (audioPath, Nothing)
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

-- | Insert track listings for episode
insertTracks ::
  Episodes.Id ->
  [TrackInfo] ->
  AppM (Either Text [EpisodeTracks.Id])
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
      Episodes.Id ->
      (Int64, TrackInfo) ->
      AppM (Either Text EpisodeTracks.Id)
    insertTrack epId (trackNum, track) = do
      let trackInsert =
            EpisodeTracks.Insert
              { EpisodeTracks.etiEpisodeId = epId,
                EpisodeTracks.etiTrackNumber = trackNum,
                EpisodeTracks.etiTitle = tiTitle track,
                EpisodeTracks.etiArtist = tiArtist track
              }

      result <- execQuery (EpisodeTracks.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right (Just trackId) -> pure $ Right trackId
        Right Nothing -> pure $ Left "Track insert returned Nothing"

--------------------------------------------------------------------------------
-- Tag Processing

-- | Parse comma-separated tag list into individual tag names.
parseTagList :: Text -> [Text]
parseTagList = filter (not . Text.null) . map Text.strip . Text.splitOn ","

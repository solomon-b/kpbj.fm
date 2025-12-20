{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Episode.New.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Episode.New.Get.Templates.Form (episodeUploadForm)
import API.Dashboard.Shows.Slug.Episode.New.Post.Route (EpisodeUploadForm (..), TrackInfo (..))
import API.Links (showEpisodesLinks)
import API.Shows.Slug.Episode.Get.Templates.Page qualified as DetailPage
import API.Types (ShowEpisodesRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
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

-- | Helper to render the new episode form with an error banner using dashboard frame
renderFormWithError ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Shows.Model ->
  Text ->
  Text ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
renderFormWithError hxRequest userMetadata allShows showModel title message = do
  -- Fetch upcoming dates for the form
  upcomingDatesResult <- execQuerySpan (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id 4)
  let upcomingDates = fromRight [] upcomingDatesResult
      banner = renderBanner Error title message
      formContent = case hxRequest of
        IsHxRequest -> do
          episodeUploadForm showModel upcomingDates
          banner
        IsNotHxRequest -> do
          banner
          episodeUploadForm showModel upcomingDates
  html <- renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing formContent
  pure $ Servant.noHeader html

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
  Maybe HxRequest ->
  EpisodeUploadForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer showSlug cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = renderBanner Error "Login Required" "You must be logged in to upload episodes."
      html <- renderTemplate hxRequest Nothing (banner :: Lucid.Html ())
      pure $ Servant.noHeader html
    Just (user, userMetadata)
      | UserMetadata.isAdmin userMetadata.mUserRole -> do
          -- Admin user - fetch all shows
          execQuerySpan Shows.getAllActiveShows >>= \case
            Left _err -> do
              Log.logInfo "Failed to fetch shows for admin" ()
              let banner = renderBanner Error "Error" "Failed to load shows."
              html <- renderTemplate hxRequest (Just userMetadata) banner
              pure $ Servant.noHeader html
            Right allShows ->
              processUploadForUser user userMetadata allShows showSlug hxRequest form
    Just (user, userMetadata) -> do
      -- Regular host - fetch their assigned shows
      execQuerySpan (Shows.getShowsForUser (User.mId user)) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch shows for user" (User.mId user)
          let banner = renderBanner Error "Error" "Failed to load your shows."
          html <- renderTemplate hxRequest (Just userMetadata) banner
          pure $ Servant.noHeader html
        Right userShows ->
          processUploadForUser user userMetadata userShows showSlug hxRequest form

-- | Process upload for an authenticated user with their shows list
processUploadForUser ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  User.Model ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Slug ->
  HxRequest ->
  EpisodeUploadForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
processUploadForUser user userMetadata allShows showSlug hxRequest form = do
  -- Verify show exists
  showResult <- execQuerySpan (Shows.getShowBySlug showSlug)
  case showResult of
    Left _err -> do
      Log.logInfo "Failed to fetch show" showSlug
      let banner = renderBanner Error "Error" "Failed to load show information."
          content = banner :: Lucid.Html ()
      html <- renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes Nothing Nothing content
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Show not found" showSlug
      let banner = renderBanner Error "Error" "Show not found."
          content = banner :: Lucid.Html ()
      html <- renderDashboardTemplate hxRequest userMetadata allShows Nothing NavEpisodes Nothing Nothing content
      pure $ Servant.noHeader html
    Right (Just showModel) -> do
      -- Verify user is host (admins can upload to any show)
      isAuthorized <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then pure (Right True) -- Admins always authorized
          else execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id)
      case isAuthorized of
        Left _err -> do
          Log.logInfo "Failed to check host permissions" showSlug
          renderFormWithError hxRequest userMetadata allShows showModel "Error" "Failed to verify permissions."
        Right True | not (UserMetadata.isSuspended userMetadata) -> do
          processEpisodeUpload userMetadata user form >>= \case
            Left err -> do
              Log.logInfo "Episode upload failed" err
              renderFormWithError hxRequest userMetadata allShows showModel "Upload Failed" err
            Right episodeId -> do
              Log.logInfo ("Episode uploaded successfully: " <> display episodeId) ()
              handleUploadSuccess hxRequest userMetadata allShows showModel episodeId
        Right _ -> do
          Log.logInfo "User is not host of show" (user.mId, showSlug)
          renderFormWithError hxRequest userMetadata allShows showModel "Permission Denied" "You are not authorized to upload episodes for this show."

-- | Handle successful upload by redirecting to episode page with banner
handleUploadSuccess ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  [Shows.Model] ->
  Shows.Model ->
  Episodes.Id ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handleUploadSuccess hxRequest userMetadata allShows showModel episodeId = do
  -- Fetch the created episode
  execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
    Right (Just episode) -> do
      -- Fetch tracks for the episode
      tracksResult <- execQuerySpan (EpisodeTracks.getTracksForEpisode episodeId)
      let tracks = fromRight [] tracksResult
          detailUrl = Links.linkURI $ showEpisodesLinks.detailWithSlug showModel.slug episodeId episode.slug
          banner = renderBanner Success "Episode Uploaded" "Your episode has been uploaded successfully."
          -- Dashboard users (hosts/staff/admin) can always view drafts
          canViewDrafts = True
          content = case hxRequest of
            IsHxRequest -> do
              DetailPage.template showModel episode tracks canViewDrafts
              banner
            IsNotHxRequest -> do
              banner
              DetailPage.template showModel episode tracks canViewDrafts
      html <- renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing content
      pure $ Servant.addHeader [i|/#{detailUrl}|] html
    _ -> do
      Log.logInfo_ "Created episode but failed to retrieve it"
      let banner = renderBanner Warning "Episode Created" "Episode was created successfully, but there was an error displaying the confirmation."
          content = banner :: Lucid.Html ()
      html <- renderDashboardTemplate hxRequest userMetadata allShows (Just showModel) NavEpisodes Nothing Nothing content
      pure $ Servant.noHeader html

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
  EpisodeUploadForm ->
  m (Either Text Episodes.Id)
processEpisodeUpload userMetadata user form = do
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
            Left validationError -> pure $ Left $ Sanitize.displayContentValidationError validationError
            Right episodeData -> do
              -- Verify user is host of the show (admins can upload to any show)
              isAuthorized <-
                if UserMetadata.isAdmin userMetadata.mUserRole
                  then pure (Right True) -- Admins always authorized
                  else execQuerySpan (ShowHost.isUserHostOfShow (User.mId user) (Shows.Id episodeData.showId))

              case isAuthorized of
                Left _err -> pure $ Left "Database error checking host permissions"
                Right isHost ->
                  if not isHost
                    then pure $ Left "You are not authorized to create episodes for this show"
                    else do
                      -- Generate episode slug for filename
                      let episodeSlug = Slug.mkSlug episodeData.title
                      -- Handle file uploads (pass scheduled date for file organization)
                      uploadResults <- processFileUploads episodeData.showSlug episodeSlug episodeData.scheduledAt episodeData.status (eufAudioFile form) (eufArtworkFile form)

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
parseFormDataWithShow :: Shows.Id -> Slug -> EpisodeUploadForm -> Either Sanitize.ContentValidationError ParsedEpisodeData
parseFormDataWithShow (Shows.Id showId) showSlug form = do
  -- Parse scheduled timestamp (now receives full UTC timestamp from form)
  scheduledAt <- case eufScheduledDate form of
    Nothing -> Right Nothing
    Just timestampStr ->
      -- Try parsing as full UTCTime (format: "YYYY-MM-DD HH:MM:SS.ssssss UTC")
      case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" (Text.unpack timestampStr) of
        Just timestamp -> Right (Just timestamp)
        Nothing -> Left $ Sanitize.ContentInvalid $ "Invalid scheduled timestamp format: " <> timestampStr

  -- Determine episode status
  status <- case eufStatus form of
    "draft" -> Right Episodes.Draft
    "published" -> Right Episodes.Published
    _ -> Left $ Sanitize.ContentInvalid "Invalid status value"

  -- Sanitize and validate episode metadata
  let sanitizedTitle = Sanitize.sanitizeTitle (eufTitle form)
      sanitizedDescription = Sanitize.sanitizeUserContent (eufDescription form)

  -- Validate content lengths
  validTitle <- Sanitize.validateContentLength 200 sanitizedTitle
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
        title = validTitle,
        description = Just validDescription,
        scheduledAt = scheduledAt,
        status = status,
        tracks = tracks,
        durationSeconds = durationSeconds
      }

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
                EpisodeTracks.etiArtist = tiArtist track
              }

      result <- execQuerySpan (EpisodeTracks.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right trackId -> pure $ Right trackId

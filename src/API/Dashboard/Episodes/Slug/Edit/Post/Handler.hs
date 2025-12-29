{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Dashboard.Episodes.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Post.Route (EpisodeEditForm (..), TrackInfo (..), parseStatus)
import API.Links (dashboardEpisodesLinks, userLinks)
import API.Types
import App.Common (getUserInfo)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Has (Has)
import Data.Maybe (isJust)
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
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
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
import Servant qualified
import Servant.Links qualified as Links
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- URL helpers
userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

userLoginGetUrlText :: Text
userLoginGetUrlText = [i|/#{userLoginGetUrl}|]

dashboardEpisodesUrl :: Slug -> Links.URI
dashboardEpisodesUrl showSlug = Links.linkURI $ dashboardEpisodesLinks.list showSlug Nothing

dashboardEpisodesUrlText :: Slug -> Text
dashboardEpisodesUrlText showSlug = [i|/#{dashboardEpisodesUrl showSlug}|]

episodeEditUrl :: Slug -> Episodes.EpisodeNumber -> Links.URI
episodeEditUrl showSlug episodeNum = Links.linkURI $ dashboardEpisodesLinks.editGet showSlug episodeNum

episodeEditUrlText :: Slug -> Episodes.EpisodeNumber -> Text
episodeEditUrlText showSlug episodeNum = [i|/#{episodeEditUrl showSlug episodeNum}|]

--------------------------------------------------------------------------------

-- | Parse a schedule value in format "template_id|scheduled_at"
parseScheduleValue :: Text -> Either Text (ShowSchedule.TemplateId, UTCTime)
parseScheduleValue txt =
  case Text.splitOn "|" txt of
    [tidStr, timeStr] -> do
      tid <- case readMaybe (Text.unpack tidStr) of
        Just n -> Right $ ShowSchedule.TemplateId n
        Nothing -> Left $ "Invalid template ID: " <> tidStr
      time <- case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" (Text.unpack timeStr) of
        Just t -> Right t
        Nothing -> Left $ "Invalid timestamp: " <> timeStr
      Right (tid, time)
    _ -> Left $ "Invalid schedule format: " <> txt

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
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  EpisodeEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug episodeNumber cookie editForm = do
  let editUrl = episodeEditUrlText showSlug episodeNumber
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized episode edit attempt" episodeNumber
      let banner = BannerParams Error "Access Denied" "You must be logged in to edit episodes."
      pure $ Servant.addHeader (buildRedirectUrl userLoginGetUrlText banner) (redirectWithBanner userLoginGetUrlText banner)
    Just (user, userMetadata) -> do
      -- Fetch the episode to verify it exists and check authorization
      execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
        Left err -> do
          Log.logAttention "getEpisodeByShowAndNumber execution error" (show err)
          let banner = BannerParams Error "Episode Not Found" "The episode you're trying to update doesn't exist."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right Nothing -> do
          Log.logInfo_ $ "No episode: show='" <> display showSlug <> "' number=" <> display episodeNumber
          let banner = BannerParams Error "Episode Not Found" "The episode you're trying to update doesn't exist."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        Right (Just episode) -> do
          -- Fetch show info
          execQuerySpan (Shows.getShowById episode.showId) >>= \case
            Left err -> do
              Log.logAttention "getShowById execution error" (show err)
              let banner = BannerParams Error "Episode Not Found" "The episode's show was not found."
              pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
            Right Nothing -> do
              Log.logInfo "Episode's show not found" episode.showId
              let banner = BannerParams Error "Episode Not Found" "The episode's show was not found."
              pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
            Right (Just showModel) -> do
              -- Check authorization - user must be creator, host, or staff+
              -- Admins don't need explicit host check since they have access to all shows
              if UserMetadata.isAdmin userMetadata.mUserRole
                then updateEpisode showSlug episodeNumber user userMetadata episode showModel editForm
                else
                  execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id) >>= \case
                    Left err -> do
                      Log.logAttention "isUserHostOfShow execution error" (show err)
                      let banner = BannerParams Error "Access Denied" "You can only edit episodes you created, or episodes for shows you host."
                      pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                    Right True -> updateEpisode showSlug episodeNumber user userMetadata episode showModel editForm
                    Right False ->
                      if UserMetadata.isStaffOrHigher userMetadata.mUserRole || episode.createdBy == user.mId
                        then updateEpisode showSlug episodeNumber user userMetadata episode showModel editForm
                        else do
                          Log.logInfo "User attempted to edit episode they don't own" episode.id
                          let banner = BannerParams Error "Access Denied" "You can only edit episodes you created, or episodes for shows you host."
                          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = episode.scheduledAt > now

-- | Check if the episode's scheduled date has passed
isScheduledInPast :: UTCTime -> Episodes.Model -> Bool
isScheduledInPast now episode = episode.scheduledAt <= now

updateEpisode ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    Has HSQL.Pool.Pool env
  ) =>
  Slug ->
  Episodes.EpisodeNumber ->
  User.Model ->
  UserMetadata.Model ->
  Episodes.Model ->
  Shows.Model ->
  EpisodeEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateEpisode showSlug episodeNumber _user userMetadata episode showModel editForm = do
  let editUrl = episodeEditUrlText showSlug episodeNumber
  -- Parse and validate form data
  case parseStatus (eefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in episode edit form" (eefStatus editForm)
      let banner = BannerParams Error "Invalid Status" "Invalid episode status value."
      pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
    Just parsedStatus -> do
      -- Check if user is trying to change status on a past-scheduled episode
      currentTime <- liftIO getCurrentTime
      let statusChanged = parsedStatus /= episode.status
          isPast = isScheduledInPast currentTime episode
          isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole

      -- Restrict status changes on past episodes to staff/admin only
      if statusChanged && isPast && not isStaffOrAdmin
        then do
          Log.logInfo "Host attempted to change status on past episode" (episode.id, episode.status, parsedStatus)
          let banner = BannerParams Error "Status Change Not Allowed" "This episode's scheduled date has passed. Only staff or admin users can change the status of past episodes."
          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
        else do
          -- Sanitize user input to prevent XSS attacks
          let sanitizedDescription = maybe "" Sanitize.sanitizeUserContent (eefDescription editForm)

          -- Validate content lengths
          case Sanitize.validateContentLength 10000 sanitizedDescription of
            Left descError -> do
              let errorMsg = Sanitize.displayContentValidationError descError
                  banner = BannerParams Error "Validation Error" errorMsg
              pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
            Right validDescription -> do
              -- Update episode metadata (basic update, doesn't change audio/artwork)
              let updateData =
                    Episodes.Update
                      { euId = episode.id,
                        euDescription = Just validDescription,
                        euStatus = Just parsedStatus
                      }

              -- Update the episode
              execQuerySpan (Episodes.updateEpisode updateData) >>= \case
                Left err -> do
                  Log.logInfo "Failed to update episode" (episode.id, show err)
                  let banner = BannerParams Error "Update Failed" "Database error occurred. Please try again."
                  pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                Right Nothing -> do
                  Log.logInfo "Episode update returned Nothing" episode.id
                  let banner = BannerParams Error "Update Failed" "Failed to update episode. Please try again."
                  pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                Right (Just _updatedId) -> do
                  -- Check if file uploads are allowed (scheduled date is in the future OR user is staff/admin)
                  let allowFileUpload = isScheduledInFuture currentTime episode || isStaffOrAdmin

                  -- Process file uploads if allowed and provided
                  fileUpdateResult <-
                    if allowFileUpload && (isJust (eefAudioFile editForm) || isJust (eefArtworkFile editForm))
                      then processFileUploads showModel episode editForm
                      else pure $ Right (Nothing, Nothing)

                  case fileUpdateResult of
                    Left fileErr -> do
                      Log.logInfo "Failed to upload files" (episode.id, fileErr)
                      let banner = BannerParams Error "File Upload Failed" ("Episode updated but file upload failed: " <> fileErr)
                      pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                    Right (mAudioPath, mArtworkPath) -> do
                      -- Update file paths in database if any files were uploaded
                      when (isJust mAudioPath || isJust mArtworkPath) $ do
                        let fileUpdate =
                              Episodes.FileUpdate
                                { efuId = episode.id,
                                  efuAudioFilePath = mAudioPath,
                                  efuArtworkUrl = mArtworkPath
                                }
                        execQuerySpan (Episodes.updateEpisodeFiles fileUpdate) >>= \case
                          Left err -> Log.logInfo "Failed to update file paths" (episode.id, show err)
                          Right Nothing -> Log.logInfo "File path update returned Nothing" episode.id
                          Right (Just _) -> Log.logInfo "Successfully updated file paths" episode.id

                      -- Update schedule slot if changed (only allowed for future episodes or staff/admin)
                      scheduleUpdateResult <- case eefScheduledDate editForm of
                        Nothing -> pure $ Right () -- No schedule change requested
                        Just scheduleDateValue -> do
                          case parseScheduleValue scheduleDateValue of
                            Left parseErr -> do
                              Log.logInfo "Failed to parse schedule value" parseErr
                              pure $ Left parseErr
                            Right (newTemplateId, newScheduledAt) -> do
                              -- Check if schedule actually changed
                              let scheduleChanged = newTemplateId /= episode.scheduleTemplateId || newScheduledAt /= episode.scheduledAt
                              if not scheduleChanged
                                then pure $ Right () -- No change
                                else do
                                  -- Non-staff can only change schedule for future episodes
                                  if isPast && not isStaffOrAdmin
                                    then do
                                      Log.logInfo "Host attempted to change schedule on past episode" episode.id
                                      pure $ Left "Schedule changes not allowed for past episodes"
                                    else do
                                      let slotUpdate =
                                            Episodes.ScheduleSlotUpdate
                                              { Episodes.essuId = episode.id,
                                                Episodes.essuScheduleTemplateId = newTemplateId,
                                                Episodes.essuScheduledAt = newScheduledAt
                                              }
                                      execQuerySpan (Episodes.updateScheduledSlot slotUpdate) >>= \case
                                        Left err -> do
                                          Log.logInfo "Failed to update schedule slot" (episode.id, show err)
                                          pure $ Left "Failed to update schedule"
                                        Right Nothing -> do
                                          Log.logInfo "Schedule slot update returned Nothing" episode.id
                                          pure $ Left "Failed to update schedule"
                                        Right (Just _) -> do
                                          Log.logInfo "Successfully updated schedule slot" episode.id
                                          pure $ Right ()

                      case scheduleUpdateResult of
                        Left scheduleErr -> do
                          let banner = BannerParams Error "Schedule Update Failed" scheduleErr
                          pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                        Right () -> do
                          -- Update tracks from JSON
                          updateTracksResult <- updateTracksFromJson episode.id (eefTracksJson editForm)
                          case updateTracksResult of
                            Left trackErr -> do
                              Log.logInfo "Failed to update tracks" (episode.id, trackErr)
                              let banner = BannerParams Error "Track Update Failed" ("Episode updated but track update failed: " <> trackErr)
                              pure $ Servant.addHeader (buildRedirectUrl editUrl banner) (redirectWithBanner editUrl banner)
                            Right _ -> do
                              -- Replace tags with new ones (single atomic query)
                              let tagNames = maybe [] parseTagList (eefTags editForm)
                              _ <- execQuerySpan (Episodes.replaceEpisodeTags episode.id tagNames)

                              -- Success! Redirect to dashboard episodes list with success banner
                              Log.logInfo "Successfully updated episode and tracks" episode.id
                              let listUrl = dashboardEpisodesUrlText showModel.slug
                                  banner = BannerParams Success "Episode Updated" "Your episode has been updated successfully."
                                  redirectUrl = buildRedirectUrl listUrl banner
                              pure $ Servant.addHeader redirectUrl (redirectWithBanner listUrl banner)

-- | Process file uploads for episode editing
processFileUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Shows.Model ->
  Episodes.Model ->
  EpisodeEditForm ->
  m (Either Text (Maybe Text, Maybe Text))
processFileUploads showModel episode editForm = do
  -- Generate a unique identifier for file naming (based on timestamp)
  currentTime <- liftIO getCurrentTime
  let fileId = Slug.mkSlug $ Text.pack $ formatTime defaultTimeLocale "%Y%m%d-%H%M%S" currentTime

  -- Process audio file if provided
  audioResult <- case eefAudioFile editForm of
    Nothing -> pure $ Right Nothing
    Just audioFile -> do
      result <- FileUpload.uploadEpisodeAudio showModel.slug fileId (Just episode.scheduledAt) audioFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload audio file" (Text.pack $ show err)
          pure $ Left "Failed to upload audio file"
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  -- Process artwork file if provided
  artworkResult <- case eefArtworkFile editForm of
    Nothing -> pure $ Right Nothing
    Just artworkFile -> do
      result <- FileUpload.uploadEpisodeArtwork showModel.slug fileId (Just episode.scheduledAt) artworkFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right _audioPath, Left artworkErr) -> pure $ Left artworkErr
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

--------------------------------------------------------------------------------
-- Track Update Logic

-- | Update tracks for an episode from JSON.
--
-- Strategy: Delete all existing tracks and insert new ones from JSON.
-- Track numbers are auto-calculated from array position (1-indexed).
updateTracksFromJson ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  Episodes.Id ->
  Maybe Text ->
  m (Either Text ())
updateTracksFromJson episodeId mTracksJson = do
  -- Delete all existing tracks for this episode
  deleteResult <- execQuerySpan (EpisodeTrack.deleteAllTracksForEpisode episodeId)
  case deleteResult of
    Left err -> do
      Log.logInfo "Failed to delete existing tracks" (show err)
      pure $ Left "Failed to delete existing tracks"
    Right _ -> do
      -- Parse and insert new tracks
      case mTracksJson of
        Nothing -> pure $ Right ()
        Just "" -> pure $ Right ()
        Just "[]" -> pure $ Right ()
        Just jsonStr -> do
          case Aeson.eitherDecodeStrict (Text.encodeUtf8 jsonStr) of
            Left err -> do
              Log.logInfo "Failed to parse tracks JSON" err
              pure $ Left $ "Invalid tracks JSON: " <> Text.pack err
            Right (trackList :: [TrackInfo]) -> do
              -- Insert tracks with auto-calculated track numbers (1-indexed)
              results <- mapM (insertTrack episodeId) (zip [1 ..] trackList)
              let (errors, _trackIds) = partitionEithers results
              if null errors
                then pure $ Right ()
                else do
                  Log.logInfo "Some tracks failed to insert" errors
                  pure $ Left "Failed to insert some tracks"

-- | Insert a single track with the given track number.
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
  (Int, TrackInfo) ->
  m (Either Text EpisodeTrack.Id)
insertTrack episodeId (trackNum, track) = do
  let trackInsert =
        EpisodeTrack.Insert
          { EpisodeTrack.etiEpisodeId = episodeId,
            EpisodeTrack.etiTrackNumber = fromIntegral trackNum,
            EpisodeTrack.etiTitle = Sanitize.sanitizePlainText (tiTitle track),
            EpisodeTrack.etiArtist = Sanitize.sanitizePlainText (tiArtist track)
          }
  execQuerySpan (EpisodeTrack.insertEpisodeTrack trackInsert) >>= \case
    Left err -> do
      Log.logInfo "Failed to insert track" (show err)
      pure $ Left "Failed to insert track"
    Right trackId -> pure $ Right trackId

--------------------------------------------------------------------------------
-- Tag Parsing

-- | Parse a comma-separated tag string into a list of trimmed, non-empty tag names.
parseTagList :: Text -> [Text]
parseTagList = filter (not . Text.null) . map Text.strip . Text.splitOn ","

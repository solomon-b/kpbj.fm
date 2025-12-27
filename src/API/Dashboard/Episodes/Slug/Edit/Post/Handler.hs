{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.Page qualified as EpisodesListPage
import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form qualified as EditForm
import API.Dashboard.Episodes.Slug.Edit.Post.Route (EpisodeEditForm (..), TrackInfo (..), parseStatus)
import API.Links (dashboardEpisodesLinks)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight, partitionEithers)
import Data.Has (Has)
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
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
dashboardEpisodesUrl :: Slug -> Links.URI
dashboardEpisodesUrl showSlug = Links.linkURI $ dashboardEpisodesLinks.list showSlug Nothing

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
  Maybe HxRequest ->
  EpisodeEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer showSlug episodeNumber cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized episode edit attempt" episodeNumber
      let banner = renderBanner Error "Access Denied" "You must be logged in to edit episodes."
      html <- renderTemplate hxRequest Nothing (banner :: Lucid.Html ())
      pure $ Servant.noHeader html
    Just (user, userMetadata) -> do
      -- Fetch the episode to verify it exists and check authorization
      execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
        Left err -> do
          Log.logAttention "getEpisodeByShowAndNumber execution error" (show err)
          let banner = renderBanner Error "Episode Not Found" "The episode you're trying to update doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) banner
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo_ $ "No episode: show='" <> display showSlug <> "' number=" <> display episodeNumber
          let banner = renderBanner Error "Episode Not Found" "The episode you're trying to update doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) banner
          pure $ Servant.noHeader html
        Right (Just episode) -> do
          -- Fetch show info
          execQuerySpan (Shows.getShowById episode.showId) >>= \case
            Left err -> do
              Log.logAttention "getShowById execution error" (show err)
              let banner = renderBanner Error "Episode Not Found" "The episode's show was not found."
              html <- renderTemplate hxRequest (Just userMetadata) banner
              pure $ Servant.noHeader html
            Right Nothing -> do
              Log.logInfo "Episode's show not found" episode.showId
              let banner = renderBanner Error "Episode Not Found" "The episode's show was not found."
              html <- renderTemplate hxRequest (Just userMetadata) banner
              pure $ Servant.noHeader html
            Right (Just showModel) -> do
              -- Check authorization - user must be creator, host, or staff+
              -- Admins don't need explicit host check since they have access to all shows
              if UserMetadata.isAdmin userMetadata.mUserRole
                then updateEpisode hxRequest user userMetadata episode showModel editForm
                else
                  execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id) >>= \case
                    Left err -> do
                      Log.logAttention "isUserHostOfShow execution error" (show err)
                      currentTime <- liftIO getCurrentTime
                      let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                      tracksResult <- execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
                      upcomingDatesResult <- execQuerySpan (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id (Limit 52))
                      let tracks = fromRight [] tracksResult
                          upcomingDates = fromRight [] upcomingDatesResult
                          banner = renderBanner Error "Access Denied" "You can only edit episodes you created, or episodes for shows you host."
                      html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                        IsHxRequest -> do
                          EditForm.template currentTime showModel episode tracks [] Nothing upcomingDates userMetadata isStaff
                          banner
                        IsNotHxRequest -> do
                          banner
                          EditForm.template currentTime showModel episode tracks [] Nothing upcomingDates userMetadata isStaff
                      pure $ Servant.noHeader html
                    Right True -> updateEpisode hxRequest user userMetadata episode showModel editForm
                    Right False ->
                      if UserMetadata.isStaffOrHigher userMetadata.mUserRole || episode.createdBy == user.mId
                        then updateEpisode hxRequest user userMetadata episode showModel editForm
                        else do
                          Log.logInfo "User attempted to edit episode they don't own" episode.id
                          currentTime <- liftIO getCurrentTime
                          let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                          tracksResult <- execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
                          upcomingDatesResult <- execQuerySpan (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id (Limit 52))
                          let tracks = fromRight [] tracksResult
                              upcomingDates = fromRight [] upcomingDatesResult
                              banner = renderBanner Error "Access Denied" "You can only edit episodes you created, or episodes for shows you host."
                          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                            IsHxRequest -> do
                              EditForm.template currentTime showModel episode tracks [] Nothing upcomingDates userMetadata isStaff
                              banner
                            IsNotHxRequest -> do
                              banner
                              EditForm.template currentTime showModel episode tracks [] Nothing upcomingDates userMetadata isStaff
                          pure $ Servant.noHeader html

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = episode.scheduledAt > now

-- | Check if the episode's scheduled date has passed
isScheduledInPast :: UTCTime -> Episodes.Model -> Bool
isScheduledInPast now episode = episode.scheduledAt <= now

-- | Helper to render the edit form with an error banner
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
  Episodes.Model ->
  Shows.Model ->
  Text ->
  Text ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
renderFormWithError hxRequest userMetadata episode showModel title message = do
  currentTime <- liftIO getCurrentTime
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
  tracksResult <- execQuerySpan (EpisodeTrack.getTracksForEpisode episode.id)
  tagsResult <- execQuerySpan (Episodes.getTagsForEpisode episode.id)
  upcomingDatesResult <- execQuerySpan (ShowSchedule.getUpcomingUnscheduledShowDates showModel.id (Limit 52))
  -- Fetch the schedule template for the current slot (for consistent display)
  mCurrentTemplate <- execQuerySpan (ShowSchedule.getScheduleTemplateById episode.scheduleTemplateId)
  let tracks = fromRight [] tracksResult
      episodeTags = fromRight [] tagsResult
      upcomingDates = fromRight [] upcomingDatesResult
      mCurrentSlot = case mCurrentTemplate of
        Right (Just scheduleTemplate) -> Just $ ShowSchedule.makeUpcomingShowDateFromTemplate scheduleTemplate episode.scheduledAt
        _ -> Nothing
      banner = renderBanner Error title message
  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
    IsHxRequest -> do
      EditForm.template currentTime showModel episode tracks episodeTags mCurrentSlot upcomingDates userMetadata isStaff
      banner
    IsNotHxRequest -> do
      banner
      EditForm.template currentTime showModel episode tracks episodeTags mCurrentSlot upcomingDates userMetadata isStaff
  pure $ Servant.noHeader html

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
  HxRequest ->
  User.Model ->
  UserMetadata.Model ->
  Episodes.Model ->
  Shows.Model ->
  EpisodeEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
updateEpisode hxRequest _user userMetadata episode showModel editForm = do
  -- Parse and validate form data
  case parseStatus (eefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in episode edit form" (eefStatus editForm)
      renderFormWithError hxRequest userMetadata episode showModel "Invalid Status" "Invalid episode status value."
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
          renderFormWithError hxRequest userMetadata episode showModel "Status Change Not Allowed" "This episode's scheduled date has passed. Only staff or admin users can change the status of past episodes."
        else do
          -- Sanitize user input to prevent XSS attacks
          let sanitizedDescription = maybe "" Sanitize.sanitizeUserContent (eefDescription editForm)

          -- Validate content lengths
          case Sanitize.validateContentLength 10000 sanitizedDescription of
            Left descError -> do
              let errorMsg = Sanitize.displayContentValidationError descError
              renderFormWithError hxRequest userMetadata episode showModel "Validation Error" errorMsg
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
                  renderFormWithError hxRequest userMetadata episode showModel "Update Failed" "Database error occurred. Please try again."
                Right Nothing -> do
                  Log.logInfo "Episode update returned Nothing" episode.id
                  renderFormWithError hxRequest userMetadata episode showModel "Update Failed" "Failed to update episode. Please try again."
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
                      renderFormWithError hxRequest userMetadata episode showModel "File Upload Failed" ("Episode updated but file upload failed: " <> fileErr)
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
                        Left scheduleErr ->
                          renderFormWithError hxRequest userMetadata episode showModel "Schedule Update Failed" scheduleErr
                        Right () -> do
                          -- Update tracks
                          updateTracksResult <- updateTracks episode.id (eefTracks editForm)
                          case updateTracksResult of
                            Left trackErr -> do
                              Log.logInfo "Failed to update tracks" (episode.id, trackErr)
                              renderFormWithError hxRequest userMetadata episode showModel "Track Update Failed" ("Episode updated but track update failed: " <> trackErr)
                            Right _ -> do
                              -- Replace tags with new ones (single atomic query)
                              let tagNames = maybe [] parseTagList (eefTags editForm)
                              _ <- execQuerySpan (Episodes.replaceEpisodeTags episode.id tagNames)

                              -- Success! Redirect to dashboard episodes list with success banner
                              Log.logInfo "Successfully updated episode and tracks" episode.id
                              -- Fetch all episodes for the show to display in the list
                              episodesResult <- execQuerySpan (Episodes.getEpisodesForShowIncludingDrafts showModel.id (Limit 100) (Offset 0))
                              let episodes = fromRight [] episodesResult
                                  listUrl = dashboardEpisodesUrl showModel.slug
                                  banner = renderBanner Success "Episode Updated" "Your episode has been updated successfully."
                              html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                                IsHxRequest -> do
                                  EpisodesListPage.template userMetadata (Just showModel) episodes 1 False
                                  banner
                                IsNotHxRequest -> do
                                  banner
                                  EpisodesListPage.template userMetadata (Just showModel) episodes 1 False
                              pure $ Servant.addHeader [i|/#{listUrl}|] html

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
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right _audioPath, Left artworkErr) -> pure $ Left artworkErr
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

--------------------------------------------------------------------------------
-- Track Update Logic

-- | Update tracks for an episode
-- Strategy: For each track in the form:
--   - If it has an ID, update the existing track
--   - If it has no ID, insert a new track
-- Note: Removed tracks are handled by the frontend (they won't be in the form submission)
updateTracks ::
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
  m (Either Text [EpisodeTrack.Id])
updateTracks episodeId tracks = do
  results <- mapM (processTrack episodeId) tracks
  let (errors, trackIds) = partitionEithers results
  if null errors
    then pure $ Right trackIds
    else do
      Log.logInfo "Some tracks failed to process" errors
      pure $ Left "Failed to process some tracks"

-- | Process a single track (update existing or insert new)
processTrack ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  Episodes.Id ->
  TrackInfo ->
  m (Either Text EpisodeTrack.Id)
processTrack episodeId track = do
  let trackInsert =
        EpisodeTrack.Insert
          { EpisodeTrack.etiEpisodeId = episodeId,
            EpisodeTrack.etiTrackNumber = tiTrackNumber track,
            EpisodeTrack.etiTitle = tiTitle track,
            EpisodeTrack.etiArtist = tiArtist track
          }

  case tiId track of
    -- Update existing track
    Just trackId -> do
      execQuerySpan (EpisodeTrack.updateEpisodeTrack trackId trackInsert) >>= \case
        Left err -> do
          Log.logInfo "Failed to update track" (trackId, show err)
          pure $ Left "Failed to update track"
        Right Nothing -> do
          Log.logInfo "Track update returned Nothing" trackId
          pure $ Left "Track not found"
        Right (Just updatedId) -> pure $ Right updatedId
    -- Insert new track
    Nothing -> do
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

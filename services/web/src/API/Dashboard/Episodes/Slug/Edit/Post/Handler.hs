{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}

module API.Dashboard.Episodes.Slug.Edit.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Post.Route (EpisodeEditForm (..), TrackInfo (..))
import API.Links (dashboardEpisodesLinks, rootLink)
import API.Types
import App.Handler.Combinators (requireAuth)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotAuthorized, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (unless, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Aeson qualified as Aeson
import Data.Either (partitionEithers)
import Data.Has (getter)
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (Day, UTCTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (localDay)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Timezone (utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (isEmptyUpload)
import Effects.FileUpload qualified as FileUpload
import Effects.StagedUploads (claimAndRelocateUpload)
import Hasql.Transaction qualified as HT
import Log qualified
import Servant qualified
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: thin glue composing action + redirect.
handler ::
  Slug ->
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  EpisodeEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler showSlug episodeNumber cookie editForm =
  handleRedirectErrors "Episode update" (dashboardEpisodesLinks.editGet showSlug episodeNumber) $ do
    (user, userMetadata) <- requireAuth cookie
    warnings <- action user userMetadata showSlug episodeNumber editForm
    let listUrl = rootLink $ dashboardEpisodesLinks.list showSlug Nothing
        flash = makeSuccessFlash warnings
    pure $ Servant.addHeader listUrl $ Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

--------------------------------------------------------------------------------

-- | Business logic: fetch, authorize, update.
--
-- Returns a list of warning messages from optional updates (file uploads,
-- schedule changes, track updates). An empty list means everything succeeded.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  Episodes.EpisodeNumber ->
  EpisodeEditForm ->
  ExceptT HandlerError AppM [Text]
action user userMetadata showSlug episodeNumber editForm = do
  -- 1. Fetch episode context (episode, show, isHost) in transaction
  (episode, showModel, isHost) <- fetchEpisodeContext showSlug episodeNumber user userMetadata

  -- 2. Check authorization
  let isAuthorized =
        episode.createdBy == user.mId
          || isHost
          || UserMetadata.isStaffOrHigher userMetadata.mUserRole
  unless isAuthorized $
    throwNotAuthorized "You can only edit episodes you created, or episodes for shows you host." (Just userMetadata.mUserRole)

  -- 3. Perform the update
  updateEpisode showSlug episodeNumber user userMetadata episode showModel editForm

--------------------------------------------------------------------------------
-- Context Fetching

-- | Fetch episode, show, and host status in a single transaction
type EpisodeEditContext = (Episodes.Model, Shows.Model, Bool)

fetchEpisodeContext ::
  Slug ->
  Episodes.EpisodeNumber ->
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM EpisodeEditContext
fetchEpisodeContext showSlug episodeNumber user userMetadata = do
  mResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          episode <- MaybeT $ HT.statement () (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber Episodes.IncludeArchived)
          showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
          isHost <-
            if UserMetadata.isAdmin userMetadata.mUserRole
              then pure True
              else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
          MaybeT $ pure $ Just (episode, showResult, isHost)
  case mResult of
    Nothing -> throwNotFound "Episode"
    -- The read includes the archived episodes so that this message can name the
    -- reason. The edit form refuses the same way, so a stale form cannot save.
    Just ctx@(episode, _, _)
      | isJust episode.deletedAt -> throwValidationError "This episode is archived. Unarchive it before you edit it."
      | otherwise -> pure ctx

--------------------------------------------------------------------------------
-- Schedule Parsing

-- | Parse a schedule value with the format "template_id|air_date".
--
-- The value gives a slot and a date. It does not give a time.
-- 'ShowSchedule.templateAirTimeOn' reads the air time from the template. The
-- client therefore cannot set an air time that differs from the template.
parseScheduleValue :: Text -> Either Text (ShowSchedule.TemplateId, Day)
parseScheduleValue txt =
  case Text.splitOn "|" txt of
    [tidStr, dateStr] -> do
      tid <- case readMaybe (Text.unpack tidStr) of
        Just n -> Right $ ShowSchedule.TemplateId n
        Nothing -> Left $ "Invalid template ID: " <> tidStr
      airDate <- case parseTimeM True defaultTimeLocale "%Y-%m-%d" (Text.unpack dateStr) of
        Just d -> Right d
        Nothing -> Left $ "Invalid air date: " <> dateStr
      Right (tid, airDate)
    _ -> Left $ "Invalid schedule format: " <> txt

-- | The request that the @scheduled_date@ field makes.
--
-- The field had three meanings and two values. An unchanged dropdown and a
-- deliberate "Unscheduled" choice both sent the empty string. The handler read
-- both as no change. The form now preselects the episode's own slot. An unchanged
-- dropdown therefore sends that slot, and the empty string has one meaning.
data SlotChange
  = -- | The form did not show the field. The slot does not change.
    LeaveAlone
  | -- | Clear the slot. The episode keeps its number, its audio, and its tracks.
    Unschedule
  | -- | Put the episode in this slot on this date.
    Assign ShowSchedule.TemplateId Day

parseSlotChange :: Maybe Text -> Either Text SlotChange
parseSlotChange = \case
  Nothing -> Right LeaveAlone
  Just "" -> Right Unschedule
  Just txt -> uncurry Assign <$> parseScheduleValue txt

--------------------------------------------------------------------------------

updateEpisode ::
  Slug ->
  Episodes.EpisodeNumber ->
  User.Model ->
  UserMetadata.Model ->
  Episodes.Model ->
  Shows.Model ->
  EpisodeEditForm ->
  ExceptT HandlerError AppM [Text]
updateEpisode _showSlug _episodeNumber _user userMetadata episode showModel editForm = do
  currentTime <- currentSystemTime
  let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      isPast = Episodes.isAired currentTime episode

  -- 1. Validation phase (throws on error)
  validDescription <- validateDescription (eefDescription editForm)

  -- 2. Core update (throws on error)
  let updateData =
        Episodes.Update
          { euId = episode.id,
            euDescription = Just validDescription
          }
  fromRightM throwDatabaseError (execQuery (Episodes.updateEpisode updateData)) >>= \case
    Nothing -> throwHandlerFailure "Failed to update episode"
    Just _ -> pure ()

  -- 3. Optional updates (accumulate warnings, don't throw)
  warnings <- lift $ execOptionalUpdates _user.mId currentTime isStaffOrAdmin isPast episode showModel editForm

  -- 4. Return warnings
  Log.logInfo "Successfully updated episode" episode.id
  pure warnings

--------------------------------------------------------------------------------
-- Validation Helpers

validateDescription :: Maybe Text -> ExceptT HandlerError AppM Text
validateDescription mDescription = do
  let sanitized = maybe "" Sanitize.sanitizeUserContent mDescription
  case Sanitize.validateContentLength 10000 sanitized of
    Left err -> throwValidationError (Sanitize.displayContentValidationError err)
    Right valid -> pure valid

--------------------------------------------------------------------------------
-- Optional Updates (accumulate warnings)

execOptionalUpdates ::
  User.Id ->
  UTCTime ->
  Bool -> -- isStaffOrAdmin
  Bool -> -- isPast
  Episodes.Model ->
  Shows.Model ->
  EpisodeEditForm ->
  AppM [Text] -- Returns list of warning messages
execOptionalUpdates userId currentTime isStaffOrAdmin isPast episode showModel editForm = do
  let allowFileUpload = Episodes.isUnaired currentTime episode || isStaffOrAdmin

  -- File uploads
  fileWarning <- processFileUploadsWithWarning userId allowFileUpload showModel episode editForm

  -- Schedule update
  scheduleWarning <- processScheduleUpdate isPast isStaffOrAdmin episode editForm

  -- Track updates
  trackWarning <- processTrackUpdates episode.id (eefTracksJson editForm)

  -- Tags (fire-and-forget, no warning on failure)
  let tagNames = maybe [] parseTagList (eefTags editForm)
  _ <- execQuery (Episodes.replaceEpisodeTags episode.id tagNames)

  pure $ concat [fileWarning, scheduleWarning, trackWarning]

-- | Process file uploads, returning a warning if it fails
--
-- Audio supports both staged uploads (tokens) and direct file uploads.
-- Artwork only uses direct file uploads (no staged upload for images).
processFileUploadsWithWarning ::
  User.Id ->
  Bool -> -- allowFileUpload
  Shows.Model ->
  Episodes.Model ->
  EpisodeEditForm ->
  AppM [Text]
processFileUploadsWithWarning userId allowFileUpload showModel episode editForm =
  let hasFileUploads = isJust (eefArtworkFile editForm) || eefAudioClear editForm || eefArtworkClear editForm
      hasStagedUploads = isJust (eefAudioToken editForm)
   in if not allowFileUpload || not (hasFileUploads || hasStagedUploads)
        then pure []
        else do
          result <- processFileUploads userId showModel episode editForm
          case result of
            Left fileErr -> do
              Log.logInfo "Failed to upload files" (episode.id, fileErr)
              pure ["File upload failed: " <> fileErr]
            Right (mAudioPath, mArtworkPath) -> do
              -- Determine final paths: new upload > explicit clear > keep existing
              let audioClear = eefAudioClear editForm
                  artworkClear = eefArtworkClear editForm
                  shouldUpdateFiles = isJust mAudioPath || isJust mArtworkPath || audioClear || artworkClear
                  -- Parse duration from form (only when new audio is uploaded)
                  mDuration = case (mAudioPath, eefDurationSeconds editForm) of
                    (Just _, Just durStr) -> readMaybe (Text.unpack durStr)
                    _ -> Nothing
              when shouldUpdateFiles $ do
                let fileUpdate =
                      Episodes.FileUpdate
                        { efuId = episode.id,
                          efuAudioFilePath = mAudioPath,
                          efuArtworkUrl = mArtworkPath,
                          efuDurationSeconds = mDuration,
                          efuClearAudio = audioClear && isNothing mAudioPath,
                          efuClearArtwork = artworkClear && isNothing mArtworkPath
                        }
                execQuery (Episodes.updateEpisodeFiles fileUpdate) >>= \case
                  Left err -> Log.logInfo "Failed to update file paths" (episode.id, show err)
                  Right Nothing -> Log.logInfo "File path update returned Nothing" episode.id
                  Right (Just _) -> Log.logInfo "Successfully updated file paths" episode.id
              pure []

-- | Update the schedule. Return a warning if the update fails.
--
-- A move of the slot and a clear of the slot need the same permission. Before the
-- episode airs, any host of the show can do both. After the episode airs, only
-- staff and admins can. The air date is then a record of the broadcast, not a
-- plan.
--
-- A request that gives the slot the episode already holds writes nothing. It also
-- runs no query. The form preselects that slot, so most saves take this path.
processScheduleUpdate ::
  Bool -> -- isPast
  Bool -> -- isStaffOrAdmin
  Episodes.Model ->
  EpisodeEditForm ->
  AppM [Text]
processScheduleUpdate isPast isStaffOrAdmin episode editForm =
  case parseSlotChange (eefScheduledDate editForm) of
    Left parseErr -> do
      Log.logInfo "Failed to parse schedule value" parseErr
      pure ["Schedule update failed: " <> parseErr]
    Right LeaveAlone -> pure []
    Right Unschedule
      | alreadyUnscheduled -> pure []
      | otherwise -> withScheduleRights clearSlot
    Right (Assign newTemplateId newAirDate)
      | sameSlot newTemplateId newAirDate -> pure []
      | otherwise -> withScheduleRights (assignSlot newTemplateId newAirDate)
  where
    alreadyUnscheduled =
      isNothing episode.scheduleTemplateId && isNothing episode.scheduledAt

    sameSlot newTemplateId newAirDate =
      Just newTemplateId == episode.scheduleTemplateId
        && fmap airDateOf episode.scheduledAt == Just newAirDate

    withScheduleRights act
      | isPast && not isStaffOrAdmin = do
          Log.logInfo "Host attempted to change schedule on past episode" episode.id
          pure ["Schedule changes not allowed for past episodes"]
      | otherwise = act

    clearSlot =
      execQuery (Episodes.clearScheduledSlot episode.id) >>= \case
        Left err -> do
          Log.logInfo "Failed to clear schedule slot" (episode.id, show err)
          pure ["Failed to update schedule"]
        Right Nothing -> do
          Log.logInfo "Schedule slot clear returned Nothing" episode.id
          pure ["Failed to update schedule"]
        Right (Just _) -> do
          Log.logInfo "Successfully cleared schedule slot" episode.id
          pure []

    assignSlot newTemplateId newAirDate =
      templateAirTimeOn newTemplateId episode.showId newAirDate >>= \case
        Nothing -> do
          Log.logAttention "Rejected schedule update: the template does not air on that date" (episode.id, newTemplateId, newAirDate)
          pure ["Schedule update failed: that show does not air in that time slot on that date"]
        Just newScheduledAt -> do
          let slotUpdate =
                Episodes.ScheduleSlotUpdate
                  { Episodes.essuId = episode.id,
                    Episodes.essuScheduleTemplateId = newTemplateId,
                    Episodes.essuScheduledAt = newScheduledAt
                  }
          execQuery (Episodes.updateScheduledSlot slotUpdate) >>= \case
            Left err -> do
              Log.logInfo "Failed to update schedule slot" (episode.id, show err)
              pure ["Failed to update schedule"]
            Right Nothing -> do
              Log.logInfo "Schedule slot update returned Nothing" episode.id
              pure ["Failed to update schedule"]
            Right (Just _) -> do
              Log.logInfo "Successfully updated schedule slot" episode.id
              pure []

-- | The station date of an instant.
airDateOf :: UTCTime -> Day
airDateOf = localDay . utcToPacific

-- | 'ShowSchedule.templateAirTimeOn' in 'AppM'.
--
-- This function fails closed. A database error refuses the schedule change.
templateAirTimeOn :: ShowSchedule.TemplateId -> Shows.Id -> Day -> AppM (Maybe UTCTime)
templateAirTimeOn templateId showId airDate =
  execQuery (ShowSchedule.templateAirTimeOn templateId showId airDate) >>= \case
    Left err -> do
      Log.logAttention "Failed to check the schedule template air time" (Text.pack $ show err)
      pure Nothing
    Right airTime -> pure airTime

-- | Process track updates, returning a warning if it fails
processTrackUpdates ::
  Episodes.Id ->
  Maybe Text ->
  AppM [Text]
processTrackUpdates episodeId mTracksJson = do
  result <- updateTracksFromJson episodeId mTracksJson
  case result of
    Left trackErr -> do
      Log.logInfo "Failed to update tracks" (episodeId, trackErr)
      pure ["Track update failed: " <> trackErr]
    Right _ -> pure []

-- | Build success flash message, incorporating any warnings
makeSuccessFlash :: [Text] -> FlashMessage
makeSuccessFlash [] = FlashMessage Success "Episode Updated" "Your episode has been updated successfully."
makeSuccessFlash warnings =
  let warningText = Text.intercalate "; " warnings
   in FlashMessage Warning "Episode Updated with Issues" ("Episode saved, but: " <> warningText)

-- | Process file uploads for episode editing
--
-- Audio uses staged uploads - file is uploaded via XHR before form submission,
-- and we receive a token to claim the upload. The file is then moved from
-- the staging area to its final location based on the episode's air date.
--
-- Artwork uses direct form submission (small files don't benefit from staged uploads).
processFileUploads ::
  User.Id ->
  Shows.Model ->
  Episodes.Model ->
  EpisodeEditForm ->
  AppM (Either Text (Maybe Text, Maybe Text))
processFileUploads userId showModel episode editForm = do
  storageBackend <- asks getter
  mAwsEnv <- asks getter

  -- Get the air date for file organization (use current time as fallback).
  -- NOTE: File paths are set at upload time and never relocated on reschedule.
  -- If downloads are added later, generate user-facing filenames from episode
  -- metadata (scheduled_at, slug, etc.) rather than relying on storage paths.
  airDate <- maybe currentSystemTime pure episode.scheduledAt

  -- Process audio: staged upload, then move to final location with air date
  audioResult <- case eefAudioToken editForm of
    Just token -> do
      -- Claim and relocate to audio/{air_date}/episodes/{show-slug}-{random}.ext
      result <-
        claimAndRelocateUpload
          userId
          token
          StagedUploads.EpisodeAudio
          AudioBucket
          EpisodeAudio
          airDate
          (display showModel.slug)
      pure $ case result of
        Left err -> Left err
        Right path -> Right (Just path)
    Nothing -> pure $ Right Nothing

  -- Process artwork: direct upload only (no staged upload for images)
  artworkResult <- case eefArtworkFile editForm of
    Nothing -> pure $ Right Nothing
    Just artworkFile
      | isEmptyUpload artworkFile -> pure $ Right Nothing
      | otherwise -> do
          result <- FileUpload.uploadEpisodeArtwork storageBackend mAwsEnv showModel.slug (Just airDate) artworkFile
          case result of
            Left err -> do
              Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
              pure $ Left $ Text.pack $ show err
            Right Nothing -> pure $ Right Nothing -- No file selected
            Right (Just uploadResult) ->
              pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

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
  Episodes.Id ->
  Maybe Text ->
  AppM (Either Text ())
updateTracksFromJson episodeId mTracksJson = do
  -- Delete all existing tracks for this episode
  deleteResult <- execQuery (EpisodeTrack.deleteAllTracksForEpisode episodeId)
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
  Episodes.Id ->
  (Int, TrackInfo) ->
  AppM (Either Text EpisodeTrack.Id)
insertTrack episodeId (trackNum, track) = do
  let trackInsert =
        EpisodeTrack.Insert
          { EpisodeTrack.etiEpisodeId = episodeId,
            EpisodeTrack.etiTrackNumber = fromIntegral trackNum,
            EpisodeTrack.etiTitle = Sanitize.sanitizePlainText (tiTitle track),
            EpisodeTrack.etiArtist = Sanitize.sanitizePlainText (tiArtist track)
          }
  execQuery (EpisodeTrack.insertEpisodeTrack trackInsert) >>= \case
    Left err -> do
      Log.logInfo "Failed to insert track" (show err)
      pure $ Left "Failed to insert track"
    Right (Just trackId) -> pure $ Right trackId
    Right Nothing -> do
      Log.logInfo "Track insert returned Nothing" (show track)
      pure $ Left "Failed to insert track"

--------------------------------------------------------------------------------
-- Tag Parsing

-- | Parse a comma-separated tag string into a list of trimmed, non-empty tag names.
parseTagList :: Text -> [Text]
parseTagList = filter (not . Text.null) . map Text.strip . Text.splitOn ","

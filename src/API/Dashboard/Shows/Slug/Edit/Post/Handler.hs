{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..), ShowEditForm (..))
import API.Links (apiLinks, dashboardShowsLinks)
import API.Types
import Amazonka qualified as AWS
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadCatch, MonadMask)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson qualified as Aeson
import Data.Has (Has, getter)
import Data.Int (Int64)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)

--------------------------------------------------------------------------------

-- URL helpers
dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId slug = Links.linkURI $ dashboardShowsLinks.detail showId slug Nothing

dashboardShowEditGetUrl :: Slug -> Text
dashboardShowEditGetUrl slug =
  let uri = Links.linkURI $ dashboardShowsLinks.editGet slug
   in [i|/#{uri}|]

-- | Parse show status from text
parseStatus :: Text -> Maybe Shows.Status
parseStatus "active" = Just Shows.Active
parseStatus "inactive" = Just Shows.Inactive
parseStatus _ = Nothing

-- | Process logo/banner file uploads
processShowArtworkUploads ::
  ( MonadUnliftIO m,
    Log.MonadLog m,
    MonadReader env m,
    MonadMask m,
    MonadIO m,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  Slug ->
  -- | Logo file
  Maybe (FileData Mem) ->
  -- | Banner file
  Maybe (FileData Mem) ->
  m (Either Text (Maybe Text, Maybe Text)) -- (logoPath, bannerPath)
processShowArtworkUploads showSlug mLogoFile mBannerFile = do
  storageBackend <- asks getter
  mAwsEnv <- asks getter

  -- Process logo file (optional)
  logoResult <- case mLogoFile of
    Nothing ->
      pure $ Right Nothing
    Just logoFile -> do
      FileUpload.uploadShowLogo storageBackend mAwsEnv showSlug logoFile >>= \case
        Left err -> do
          Log.logInfo "Failed to upload logo file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
          pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

  -- Process banner file (optional)
  bannerResult <- case mBannerFile of
    Nothing ->
      pure $ Right Nothing
    Just bannerFile -> do
      FileUpload.uploadShowBanner storageBackend mAwsEnv showSlug bannerFile >>= \case
        Left err -> do
          Log.logInfo "Failed to upload banner file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err -- File validation failed, reject entire operation
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
          pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

  case (logoResult, bannerResult) of
    (Left logoErr, _) -> pure $ Left logoErr
    (Right logoPath, Left _bannerErr) -> pure $ Right (logoPath, Nothing)
    (Right logoPath, Right bannerPath) -> pure $ Right (logoPath, bannerPath)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadMask m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  Tracer ->
  Slug ->
  Maybe Cookie ->
  ShowEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer slug cookie editForm =
  handleRedirectErrors "Show edit" apiLinks.rootGet $ do
    -- 1. Require authentication and authorization (host of show or staff+)
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId slug userMetadata

    -- 2. Fetch the show
    showModel <- fetchShowOrNotFound slug

    -- 3. Process the edit
    updateShow userMetadata showModel editForm

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
fetchShowOrNotFound slug =
  execQuerySpan (Shows.getShowBySlug slug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

updateShow ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
    MonadMask m,
    MonadIO m,
    Has HSQL.Pool.Pool env,
    Has StorageBackend env,
    Has (Maybe AWS.Env) env
  ) =>
  UserMetadata.Model ->
  Shows.Model ->
  ShowEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateShow userMetadata showModel editForm = do
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      editUrl = dashboardShowEditGetUrl showModel.slug

  -- Parse and validate form data
  case parseStatus (sefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in show edit form" (sefStatus editForm)
      let banner = BannerParams Error "Validation Error" "Invalid show status value."
      pure (Servant.noHeader (redirectWithBanner editUrl banner))
    Just parsedStatus -> do
      -- If not staff, preserve original schedule/settings values
      let finalStatus = if isStaff then parsedStatus else showModel.status

          -- Generate slug from title
          generatedSlug = Slug.mkSlug (sefTitle editForm)

      -- Process file uploads
      uploadResults <- processShowArtworkUploads generatedSlug (sefLogoFile editForm) (sefBannerFile editForm)

      case uploadResults of
        Left uploadErr -> do
          Log.logInfo "Failed to upload show artwork" uploadErr
          let banner = BannerParams Error "Upload Error" ("File upload error: " <> uploadErr)
          pure (Servant.noHeader (redirectWithBanner editUrl banner))
        Right (mLogoPath, mBannerPath) -> do
          -- Determine final URLs based on: new upload > explicit clear > keep existing
          let finalLogoUrl = case (mLogoPath, sefLogoClear editForm) of
                (Just path, _) -> Just path -- New file uploaded
                (Nothing, True) -> Nothing -- User explicitly cleared
                (Nothing, False) -> showModel.logoUrl -- Keep existing
              finalBannerUrl = case (mBannerPath, sefBannerClear editForm) of
                (Just path, _) -> Just path -- New file uploaded
                (Nothing, True) -> Nothing -- User explicitly cleared
                (Nothing, False) -> showModel.bannerUrl -- Keep existing
              updateData =
                Shows.Insert
                  { siTitle = sefTitle editForm,
                    siSlug = generatedSlug,
                    siDescription = sefDescription editForm,
                    siLogoUrl = finalLogoUrl,
                    siBannerUrl = finalBannerUrl,
                    siStatus = finalStatus
                  }

          -- Update the show
          execQuerySpan (Shows.updateShow showModel.id updateData) >>= \case
            Left err -> do
              Log.logInfo "Failed to update show" (showModel.id, show err)
              let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
              pure (Servant.noHeader (redirectWithBanner editUrl banner))
            Right Nothing -> do
              Log.logInfo "Show update returned Nothing" showModel.id
              let banner = BannerParams Error "Update Failed" "Failed to update show. Please try again."
              pure (Servant.noHeader (redirectWithBanner editUrl banner))
            Right (Just _updatedId) -> do
              Log.logInfo "Successfully updated show" showModel.id
              -- Process tags: clear existing and add new ones
              processShowTags showModel.id (sefTags editForm)
              -- Process schedule updates if staff
              if isStaff
                then case parseSchedules (sefSchedulesJson editForm) of
                  Left err -> do
                    Log.logInfo "Schedule validation failed" err
                    let banner = BannerParams Error "Schedule Error" err
                    pure (Servant.noHeader (redirectWithBanner editUrl banner))
                  Right schedules -> do
                    -- Check for conflicts with other shows
                    conflictCheck <- checkScheduleConflicts showModel.id schedules
                    case conflictCheck of
                      Left conflictErr -> do
                        Log.logInfo "Schedule conflict with other show" conflictErr
                        let banner = BannerParams Error "Schedule Conflict" conflictErr
                        pure (Servant.noHeader (redirectWithBanner editUrl banner))
                      Right () -> do
                        -- Update schedules
                        updateSchedulesForShow showModel.id schedules
                        -- Update hosts
                        updateHostsForShow showModel.id (sefHosts editForm)
                        redirectToShowPage showModel.id generatedSlug
                else redirectToShowPage showModel.id generatedSlug
  where
    -- Redirect to the dashboard show detail page with a success banner
    redirectToShowPage :: (MonadIO m) => Shows.Id -> Slug -> m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    redirectToShowPage showId newSlug = do
      let showUrl = [i|/#{dashboardShowDetailUrl showId newSlug}|] :: Text
          bannerParams =
            BannerParams
              { bpType = Success,
                bpTitle = "Show Updated",
                bpMessage = "Your show has been updated successfully."
              }
          -- Build the full URL with banner params for the HX-Redirect header
          redirectUrl = buildRedirectUrl showUrl bannerParams
      pure $ Servant.addHeader redirectUrl (redirectWithBanner showUrl bannerParams)

--------------------------------------------------------------------------------
-- Schedule Update Helpers

-- | Parse schedules JSON from form data and validate for overlaps
parseSchedules :: Maybe Text -> Either Text [ScheduleSlotInfo]
parseSchedules Nothing = Right []
parseSchedules (Just schedulesJson)
  | Text.null (Text.strip schedulesJson) = Right []
  | schedulesJson == "[]" = Right []
  | otherwise = case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
      Left err -> Left $ "Invalid schedules JSON: " <> Text.pack err
      Right slots -> validateNoOverlaps slots

-- | Validate that schedule slots don't overlap on the same day
--
-- Two slots overlap if they're on the same day, share at least one week of the month,
-- and their time ranges intersect.
validateNoOverlaps :: [ScheduleSlotInfo] -> Either Text [ScheduleSlotInfo]
validateNoOverlaps slots =
  case findOverlap slots of
    Just (slot1, slot2) ->
      Left $
        "Schedule conflict: "
          <> dayOfWeek slot1
          <> " "
          <> startTime slot1
          <> "-"
          <> endTime slot1
          <> " overlaps with "
          <> dayOfWeek slot2
          <> " "
          <> startTime slot2
          <> "-"
          <> endTime slot2
    Nothing -> Right slots

-- | Find the first pair of overlapping slots, if any
findOverlap :: [ScheduleSlotInfo] -> Maybe (ScheduleSlotInfo, ScheduleSlotInfo)
findOverlap [] = Nothing
findOverlap (x : xs) =
  case filter (slotsOverlap x) xs of
    (y : _) -> Just (x, y)
    [] -> findOverlap xs

-- | Check if two schedule slots overlap
slotsOverlap :: ScheduleSlotInfo -> ScheduleSlotInfo -> Bool
slotsOverlap slot1 slot2 =
  -- Must be same day of week
  dayOfWeek slot1 == dayOfWeek slot2
    -- Must share at least one week of the month
    && weeksOverlap (weeksOfMonth slot1) (weeksOfMonth slot2)
    -- Time ranges must intersect
    && timesOverlap slot1 slot2

-- | Check if two lists of weeks share any common weeks
weeksOverlap :: [Int64] -> [Int64] -> Bool
weeksOverlap weeks1 weeks2 = any (`elem` weeks2) weeks1

-- | Check if time ranges overlap
--
-- Handles overnight shows (where end time < start time, e.g., 23:00-01:00)
timesOverlap :: ScheduleSlotInfo -> ScheduleSlotInfo -> Bool
timesOverlap slot1 slot2 =
  case (parseTimeOfDay (startTime slot1), parseTimeOfDay (endTime slot1), parseTimeOfDay (startTime slot2), parseTimeOfDay (endTime slot2)) of
    (Just start1, Just end1, Just start2, Just end2) ->
      let -- Normalize overnight shows by treating them as two ranges
          -- For simplicity, we check if ranges overlap
          -- Range 1: [start1, end1) - if end1 <= start1, it's overnight
          -- Range 2: [start2, end2) - if end2 <= start2, it's overnight
          isOvernight1 = end1 <= start1
          isOvernight2 = end2 <= start2
       in case (isOvernight1, isOvernight2) of
            -- Neither is overnight: simple range overlap
            (False, False) ->
              start1 < end2 && start2 < end1
            -- Slot1 is overnight (e.g., 23:00-01:00): ranges are [start1, midnight) and [midnight, end1)
            (True, False) ->
              -- Slot2 overlaps if it touches [start1, 24:00) or [00:00, end1)
              start2 < end1 || end2 > start1
            -- Slot2 is overnight
            (False, True) ->
              start1 < end2 || end1 > start2
            -- Both overnight: they definitely overlap (both span midnight)
            (True, True) ->
              True
    -- If we can't parse times, assume no overlap (validation will catch invalid times later)
    _ -> False

-- | Parse day of week from text
parseDayOfWeek :: Text -> Maybe DayOfWeek
parseDayOfWeek "sunday" = Just Sunday
parseDayOfWeek "monday" = Just Monday
parseDayOfWeek "tuesday" = Just Tuesday
parseDayOfWeek "wednesday" = Just Wednesday
parseDayOfWeek "thursday" = Just Thursday
parseDayOfWeek "friday" = Just Friday
parseDayOfWeek "saturday" = Just Saturday
parseDayOfWeek _ = Nothing

-- | Parse time of day from "HH:MM" format
parseTimeOfDay :: Text -> Maybe TimeOfDay
parseTimeOfDay t = parseTimeM True defaultTimeLocale "%H:%M" (Text.unpack t)

-- | Check for schedule conflicts with other shows
checkScheduleConflicts ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    MonadUnliftIO m,
    Has Tracer env
  ) =>
  Shows.Id ->
  [ScheduleSlotInfo] ->
  m (Either Text ())
checkScheduleConflicts showId = go
  where
    go [] = pure (Right ())
    go (slot : rest) =
      case (parseDayOfWeek (dayOfWeek slot), parseTimeOfDay (startTime slot), parseTimeOfDay (endTime slot)) of
        (Just dow, Just start, Just end) -> do
          let weeks = map fromIntegral (weeksOfMonth slot)
          execQuerySpan (ShowSchedule.checkTimeSlotConflict showId dow weeks start end) >>= \case
            Left err -> do
              Log.logInfo "Failed to check schedule conflict" (Text.pack $ show err)
              pure (Right ()) -- Don't block on DB errors, let it through
            Right (Just conflictingShow) ->
              pure (Left $ "Schedule conflict: " <> dayOfWeek slot <> " " <> startTime slot <> "-" <> endTime slot <> " overlaps with \"" <> conflictingShow <> "\"")
            Right Nothing -> go rest
        _ -> go rest -- Skip invalid slots, they'll fail later anyway

-- | Update schedules for a show
--
-- Closes out existing active schedule templates by setting their validity end date,
-- then creates new templates effective from today. This preserves historical schedule
-- data for tracking purposes.
updateSchedulesForShow ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Shows.Id ->
  [ScheduleSlotInfo] ->
  m ()
updateSchedulesForShow showId newSchedules = do
  today <- liftIO $ utctDay <$> getCurrentTime

  -- Close out all currently active schedule templates for this show
  activeTemplates <-
    execQuerySpan (ShowSchedule.getActiveScheduleTemplatesForShow showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch active schedules" (Text.pack $ show err)
        pure []
      Right templates -> pure templates

  -- For each active template, end its active validity periods
  forM_ activeTemplates $ \template -> do
    activeValidities <-
      execQuerySpan (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId) >>= \case
        Left err -> do
          Log.logInfo "Failed to fetch validity periods" (Text.pack $ show err)
          pure []
        Right validities -> pure validities

    -- End each active validity period by setting effective_until to today
    forM_ activeValidities $ \validity -> do
      _ <- execQuerySpan (ShowSchedule.endValidity validity.stvId today)
      Log.logInfo "Closed out schedule validity" (show template.stId, show validity.stvId)

  -- Create new schedule templates
  forM_ newSchedules $ \slot -> do
    case ( parseDayOfWeek (dayOfWeek slot),
           parseTimeOfDay (startTime slot),
           parseTimeOfDay (endTime slot)
         ) of
      (Just dow, Just start, Just end) -> do
        -- Create new schedule template
        let templateInsert =
              ShowSchedule.ScheduleTemplateInsert
                { ShowSchedule.stiShowId = showId,
                  ShowSchedule.stiDayOfWeek = Just dow,
                  ShowSchedule.stiWeeksOfMonth = Just (map fromIntegral (weeksOfMonth slot)),
                  ShowSchedule.stiStartTime = start,
                  ShowSchedule.stiEndTime = end,
                  ShowSchedule.stiTimezone = "America/Los_Angeles"
                }

        templateResult <- execQuerySpan (ShowSchedule.insertScheduleTemplate templateInsert)
        case templateResult of
          Left err ->
            Log.logInfo "Failed to insert schedule template" (Text.pack $ show err)
          Right templateId -> do
            -- Create validity record (effective from today, no end date)
            let validityInsert =
                  ShowSchedule.ValidityInsert
                    { ShowSchedule.viTemplateId = templateId,
                      ShowSchedule.viEffectiveFrom = today,
                      ShowSchedule.viEffectiveUntil = Nothing
                    }
            validityResult <- execQuerySpan (ShowSchedule.insertValidity validityInsert)
            case validityResult of
              Left err ->
                Log.logInfo "Failed to insert validity" (Text.pack $ show err)
              Right _ ->
                Log.logInfo "Created new schedule for show" (show showId, show dow)
      _ ->
        Log.logInfo "Invalid schedule slot data - skipping" (show slot)

--------------------------------------------------------------------------------
-- Host Update Helpers

-- | Update hosts for a show
--
-- Compares the new host list with the current hosts and:
-- 1. Removes hosts that are no longer in the list
-- 2. Adds hosts that are new to the list
-- 3. Promotes users to Host role if they aren't already Host or higher
updateHostsForShow ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Shows.Id ->
  [User.Id] ->
  m ()
updateHostsForShow showId newHostIds = do
  let newHostSet = Set.fromList newHostIds

  -- Get current hosts
  currentHosts <-
    execQuerySpan (ShowHost.getShowHosts showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch current hosts" (show err)
        pure []
      Right hosts -> pure hosts

  let currentHostSet = Set.fromList $ map (.shmUserId) currentHosts

  -- Find hosts to remove (in current but not in new)
  let hostsToRemove = Set.toList $ Set.difference currentHostSet newHostSet

  -- Find hosts to add (in new but not in current)
  let hostsToAdd = Set.toList $ Set.difference newHostSet currentHostSet

  -- Remove hosts that are no longer assigned
  forM_ hostsToRemove $ \hostId -> do
    _ <- execQuerySpan (ShowHost.removeShowHost showId hostId)
    Log.logInfo "Removed host from show" (show showId, show hostId)

  -- Add new hosts
  forM_ hostsToAdd $ \hostId -> do
    -- Add host to show
    _ <- execQuerySpan (ShowHost.addHostToShow showId hostId)
    Log.logInfo "Added host to show" (show showId, show hostId)

    -- Promote user to Host role if they're currently just a User
    execQuerySpan (UserMetadata.getUserMetadata hostId) >>= \case
      Left err ->
        Log.logInfo "Failed to fetch user metadata for role promotion" (show err)
      Right Nothing ->
        Log.logInfo "User not found for role promotion" (show hostId)
      Right (Just userMeta) ->
        when (userMeta.mUserRole == UserMetadata.User) $ do
          _ <- execQuerySpan (UserMetadata.updateUserRole hostId UserMetadata.Host)
          Log.logInfo "Promoted user to Host role" (show hostId)

  Log.logInfo "Host update complete" (show showId, "removed" :: Text, length hostsToRemove, "added" :: Text, length hostsToAdd)

--------------------------------------------------------------------------------
-- Tag Processing Helpers

-- | Process tags for a show
--
-- Clears all existing tags and re-adds tags from the comma-separated input.
-- Uses a create-or-reuse pattern: if a tag exists, it's reused; otherwise created.
processShowTags ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadDB m
  ) =>
  Shows.Id ->
  Maybe Text ->
  m ()
processShowTags showId mTagsText = do
  -- First, remove all existing tags from this show
  _ <- execQuerySpan (Shows.removeAllTagsFromShow showId)
  Log.logInfo "Cleared existing tags for show" (show showId)

  -- Then add new tags
  case mTagsText of
    Nothing -> pure ()
    Just tagsText -> do
      let tagNames = filter (not . Text.null) $ map Text.strip $ Text.splitOn "," tagsText
      forM_ tagNames $ \tagName -> do
        -- Check if tag already exists
        execQuerySpan (ShowTags.getShowTagByName tagName) >>= \case
          Right (Just existingTag) -> do
            -- Tag exists, just associate it with the show
            _ <- execQuerySpan (Shows.addTagToShow showId (ShowTags.stId existingTag))
            Log.logInfo "Associated existing tag with show" (show showId, tagName)
          _ -> do
            -- Tag doesn't exist, create it and associate
            execQuerySpan (ShowTags.insertShowTag (ShowTags.Insert tagName)) >>= \case
              Right newTagId -> do
                _ <- execQuerySpan (Shows.addTagToShow showId newTagId)
                Log.logInfo "Created and associated new tag with show" (show showId, tagName)
              Left err ->
                Log.logInfo "Failed to create tag" (tagName, show err)

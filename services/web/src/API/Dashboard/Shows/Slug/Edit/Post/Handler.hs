{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Edit.Post.Handler
  ( handler,
    ParsedScheduleSlot (..),
    normalizeTemplate,
    parseScheduleSlot,
    schedulesMatch,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..), ShowEditForm (..))
import API.Links (apiLinks, dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (handleRedirectErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (forM_, when)
import Data.Function ((&))
import Control.Monad.Reader (asks)
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Int (Int64)
import Data.List (sort)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (Day, DayOfWeek (..), TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (LocalTime (..), utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Effects.HostNotifications qualified as HostNotifications
import Log qualified
import Lucid qualified
import Rel8 (Result)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)

--------------------------------------------------------------------------------

-- | A schedule slot with all fields parsed and validated.
--
-- Produced by 'parseScheduleSlot'; all downstream functions operate on this
-- type instead of the raw 'ScheduleSlotInfo' form data.
data ParsedScheduleSlot = ParsedScheduleSlot
  { pssDay :: DayOfWeek,
    pssWeeks :: [Int64],
    pssStart :: TimeOfDay,
    pssEnd :: TimeOfDay
  }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- URL helpers
dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId slug = Links.linkURI $ dashboardShowsLinks.detail showId slug Nothing

dashboardShowEditGetUrl :: Slug -> Text
dashboardShowEditGetUrl slug =
  let uri = Links.linkURI $ dashboardShowsLinks.editGet slug
   in [i|/#{uri}|]

-- | Process logo file upload
processShowArtworkUploads ::
  Slug ->
  -- | Logo file
  Maybe (FileData Mem) ->
  AppM (Either Text (Maybe Text))
processShowArtworkUploads showSlug mLogoFile = do
  storageBackend <- asks getter
  mAwsEnv <- asks getter

  -- Process logo file (optional)
  case mLogoFile of
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

--------------------------------------------------------------------------------

handler ::
  Slug ->
  Maybe Cookie ->
  ShowEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler slug cookie editForm =
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
  Slug ->
  AppM Shows.Model
fetchShowOrNotFound slug =
  execQuery (Shows.getShowBySlug slug) >>= \case
    Left err -> throwDatabaseError err
    Right Nothing -> throwNotFound "Show"
    Right (Just showModel) -> pure showModel

updateShow ::
  UserMetadata.Model ->
  Shows.Model ->
  ShowEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateShow userMetadata showModel editForm = do
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
      editUrl = dashboardShowEditGetUrl showModel.slug

  -- Parse and validate form data
  case Shows.decodeStatus (sefStatus editForm) of
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
      uploadResults <- processShowArtworkUploads generatedSlug (sefLogoFile editForm)

      case uploadResults of
        Left uploadErr -> do
          Log.logInfo "Failed to upload show artwork" uploadErr
          let banner = BannerParams Error "Upload Error" ("File upload error: " <> uploadErr)
          pure (Servant.noHeader (redirectWithBanner editUrl banner))
        Right mLogoPath -> do
          -- Determine final URL based on: new upload > explicit clear > keep existing
          let finalLogoUrl = case (mLogoPath, sefLogoClear editForm) of
                (Just path, _) -> Just path -- New file uploaded
                (Nothing, True) -> Nothing -- User explicitly cleared
                (Nothing, False) -> showModel.logoUrl -- Keep existing

              -- Treat empty description as Nothing
              mDescription =
                let desc = sefDescription editForm
                 in if Text.null (Text.strip desc) then Nothing else Just desc

              updateData =
                Shows.Insert
                  { siTitle = sefTitle editForm,
                    siSlug = generatedSlug,
                    siDescription = mDescription,
                    siLogoUrl = finalLogoUrl,
                    siStatus = finalStatus
                  }

          -- Update the show
          execQuery (Shows.updateShow showModel.id updateData) >>= \case
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
                        -- Update hosts and get newly added ones
                        newlyAddedHosts <- updateHostsForShow showModel.id (sefHosts editForm)
                        -- Send notification emails to newly added hosts
                        let mTimeslot = buildTimeslotDescription schedules
                        HostNotifications.sendHostAssignmentNotifications showModel mTimeslot newlyAddedHosts
                        redirectToShowPage showModel.id generatedSlug
                else redirectToShowPage showModel.id generatedSlug
  where
    -- Redirect to the dashboard show detail page with a success banner
    redirectToShowPage :: Shows.Id -> Slug -> AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
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

-- | Parse schedules JSON from form data, validate all fields, and check for overlaps.
--
-- This is the single parse boundary: downstream functions receive 'ParsedScheduleSlot'
-- values with typed fields and never re-parse from text.
parseSchedules :: Maybe Text -> Either Text [ParsedScheduleSlot]
parseSchedules Nothing = Right []
parseSchedules (Just schedulesJson)
  | Text.null (Text.strip schedulesJson) = Right []
  | schedulesJson == "[]" = Right []
  | otherwise = case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
      Left err -> Left $ "Invalid schedules JSON: " <> Text.pack err
      Right slots -> do
        parsed <- traverse parseScheduleSlot slots
        validateNoOverlaps parsed

-- | Parse and validate a single schedule slot from form data.
parseScheduleSlot :: ScheduleSlotInfo -> Either Text ParsedScheduleSlot
parseScheduleSlot slot = do
  dow <- maybe (Left $ "Invalid day of week: " <> dayOfWeek slot) Right (parseDayOfWeek (dayOfWeek slot))
  start <- maybe (Left $ "Invalid start time: " <> startTime slot) Right (parseTimeOfDay (startTime slot))
  end <- maybe (Left $ "Invalid end time: " <> endTime slot) Right (parseTimeOfDay (endTime slot))
  Right $
    ParsedScheduleSlot
      { pssDay = dow,
        pssWeeks = sort (weeksOfMonth slot),
        pssStart = start,
        pssEnd = end
      }

-- | Validate that schedule slots don't overlap on the same day.
--
-- Two slots overlap if they're on the same day, share at least one week of the month,
-- and their time ranges intersect.
validateNoOverlaps :: [ParsedScheduleSlot] -> Either Text [ParsedScheduleSlot]
validateNoOverlaps slots =
  case findOverlap slots of
    Just (slot1, slot2) ->
      Left $
        "Schedule conflict: "
          <> Text.pack (show (pssDay slot1))
          <> " "
          <> formatTimeHHMM (pssStart slot1)
          <> "-"
          <> formatTimeHHMM (pssEnd slot1)
          <> " overlaps with "
          <> Text.pack (show (pssDay slot2))
          <> " "
          <> formatTimeHHMM (pssStart slot2)
          <> "-"
          <> formatTimeHHMM (pssEnd slot2)
    Nothing -> Right slots

-- | Find the first pair of overlapping slots, if any.
findOverlap :: [ParsedScheduleSlot] -> Maybe (ParsedScheduleSlot, ParsedScheduleSlot)
findOverlap [] = Nothing
findOverlap (x : xs) =
  case filter (slotsOverlap x) xs of
    (y : _) -> Just (x, y)
    [] -> findOverlap xs

-- | Check if two schedule slots overlap.
slotsOverlap :: ParsedScheduleSlot -> ParsedScheduleSlot -> Bool
slotsOverlap slot1 slot2 =
  pssDay slot1 == pssDay slot2
    && weeksOverlap (pssWeeks slot1) (pssWeeks slot2)
    && timesOverlap slot1 slot2

-- | Check if two lists of weeks share any common weeks.
weeksOverlap :: [Int64] -> [Int64] -> Bool
weeksOverlap weeks1 weeks2 = any (`elem` weeks2) weeks1

-- | Check if time ranges overlap.
--
-- Handles overnight shows (where end time < start time, e.g., 23:00-01:00).
timesOverlap :: ParsedScheduleSlot -> ParsedScheduleSlot -> Bool
timesOverlap slot1 slot2 =
  let start1 = pssStart slot1
      end1 = pssEnd slot1
      start2 = pssStart slot2
      end2 = pssEnd slot2
      isOvernight1 = end1 <= start1
      isOvernight2 = end2 <= start2
   in case (isOvernight1, isOvernight2) of
        (False, False) ->
          start1 < end2 && start2 < end1
        (True, False) ->
          start2 < end1 || end2 > start1
        (False, True) ->
          start1 < end2 || end1 > start2
        (True, True) ->
          True

-- | Parse day of week from text.
parseDayOfWeek :: Text -> Maybe DayOfWeek
parseDayOfWeek "sunday" = Just Sunday
parseDayOfWeek "monday" = Just Monday
parseDayOfWeek "tuesday" = Just Tuesday
parseDayOfWeek "wednesday" = Just Wednesday
parseDayOfWeek "thursday" = Just Thursday
parseDayOfWeek "friday" = Just Friday
parseDayOfWeek "saturday" = Just Saturday
parseDayOfWeek _ = Nothing

-- | Parse time of day from "HH:MM" format.
parseTimeOfDay :: Text -> Maybe TimeOfDay
parseTimeOfDay t = parseTimeM True defaultTimeLocale "%H:%M" (Text.unpack t)

-- | Format a TimeOfDay as "HH:MM" for error messages.
formatTimeHHMM :: TimeOfDay -> Text
formatTimeHHMM = Text.pack . formatTime defaultTimeLocale "%H:%M"

--------------------------------------------------------------------------------
-- Schedule Diff Helpers

-- | Normalize a DB template to a 'ParsedScheduleSlot' for comparison.
--
-- Returns 'Nothing' for templates with no day of week (shouldn't occur in
-- practice, but the DB column is nullable).
normalizeTemplate :: ShowSchedule.ScheduleTemplate Result -> Maybe ParsedScheduleSlot
normalizeTemplate t = case t.stDayOfWeek of
  Just dow ->
    Just $
      ParsedScheduleSlot
        { pssDay = dow,
          pssWeeks = sort (fromMaybe [] t.stWeeksOfMonth),
          pssStart = t.stStartTime,
          pssEnd = t.stEndTime
        }
  Nothing -> Nothing

-- | Check if parsed form schedule matches current DB schedule.
--
-- Both sides are compared as sets of 'ParsedScheduleSlot'. DB templates with
-- no day of week are excluded (they can't match any valid form slot).
schedulesMatch :: [ShowSchedule.ScheduleTemplate Result] -> [ParsedScheduleSlot] -> Bool
schedulesMatch dbTemplates parsedSlots =
  let dbSet = Set.fromList $ mapMaybe normalizeTemplate dbTemplates
      formSet = Set.fromList parsedSlots
   in dbSet == formSet

--------------------------------------------------------------------------------

-- | Check for schedule conflicts with other shows.
checkScheduleConflicts ::
  Shows.Id ->
  [ParsedScheduleSlot] ->
  AppM (Either Text ())
checkScheduleConflicts showId = go
  where
    go [] = pure (Right ())
    go (slot : rest) = do
      let weeks = map fromIntegral (pssWeeks slot)
      execQuery (ShowSchedule.checkTimeSlotConflict showId (pssDay slot) weeks (pssStart slot) (pssEnd slot)) >>= \case
        Left err -> do
          Log.logInfo "Failed to check schedule conflict" (Text.pack $ show err)
          pure (Right ()) -- Don't block on DB errors, let it through
        Right (Just conflictingShow) ->
          pure (Left $ "Schedule conflict: " <> Text.pack (show (pssDay slot)) <> " " <> formatTimeHHMM (pssStart slot) <> "-" <> formatTimeHHMM (pssEnd slot) <> " overlaps with \"" <> conflictingShow <> "\"")
        Right Nothing -> go rest

-- | Update schedules for a show.
--
-- Compares the incoming form schedule against the current DB schedule. If they
-- match, skips the terminate-and-recreate cycle. This prevents orphaning episodes
-- that are linked to the existing schedule templates.
updateSchedulesForShow ::
  Shows.Id ->
  [ParsedScheduleSlot] ->
  AppM ()
updateSchedulesForShow showId newSchedules = do
  -- Use Pacific time for "today" to match the schedule display
  nowUtc <- currentSystemTime
  let today = localDay (utcToPacific nowUtc)

  -- Fetch currently active schedule templates for this show
  activeTemplates <-
    execQuery (ShowSchedule.getActiveScheduleTemplatesForShow showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch active schedules" (Text.pack $ show err)
        pure []
      Right templates -> pure templates

  -- Skip the terminate-and-recreate cycle if the schedule hasn't changed
  if schedulesMatch activeTemplates newSchedules
    then Log.logInfo "Schedule unchanged, skipping update" (show showId)
    else do
      Log.logInfo "Schedule changed, updating" (show showId)
      updateScheduleTemplates showId activeTemplates newSchedules today

-- | Apply slot-level diff: terminate removed slots, create added slots, leave unchanged alone.
--
-- Instead of nuking all existing templates and recreating from scratch, we compare
-- the current DB state against the incoming form data as sets of 'ParsedScheduleSlot':
--
--   removed   = dbSet \\ formSet   (slots the user deleted from the form)
--   added     = formSet \\ dbSet   (slots the user added in the form)
--   unchanged = dbSet ∩ formSet    (implicitly left alone — no DB writes)
--
-- This preserves the template IDs and validity periods of unchanged slots, which is
-- critical because episodes are linked to templates via schedule_template_id. Destroying
-- and recreating a template with identical times orphans any episodes uploaded against
-- the old template, since the episode's foreign key still points to the terminated one.
updateScheduleTemplates ::
  Shows.Id ->
  [ShowSchedule.ScheduleTemplate Result] ->
  [ParsedScheduleSlot] ->
  Day ->
  AppM ()
updateScheduleTemplates showId activeTemplates parsedSlots today = do
  let -- Normalize each DB template into a ParsedScheduleSlot for comparison, and
      -- build a reverse lookup so we can find the original template(s) to terminate.
      -- Templates with no day of week (shouldn't happen in practice) are dropped.
      templateMap :: Map.Map ParsedScheduleSlot [ShowSchedule.ScheduleTemplate Result]
      templateMap =
        foldMap
          ( \t -> normalizeTemplate t & \case
              Just slot -> Map.singleton slot [t]
              Nothing -> Map.empty
          )
          activeTemplates

      -- Compare as sets to compute the three-way partition
      dbSet = Map.keysSet templateMap
      formSet = Set.fromList parsedSlots

      -- Slots in DB but not in form — user removed these
      removed = Set.difference dbSet formSet
      -- Slots in form but not in DB — user added these
      added = Set.difference formSet dbSet

  -- For each removed slot, look up the original DB template(s) and end their
  -- active validity periods by setting effective_until to today.
  forM_ (Set.toList removed) $ \slot ->
    forM_ (Map.findWithDefault [] slot templateMap) $ \template -> do
      activeValidities <-
        execQuery (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId) >>= \case
          Left err -> do
            Log.logInfo "Failed to fetch validity periods" (Text.pack $ show err)
            pure []
          Right validities -> pure validities

      forM_ activeValidities $ \validity -> do
        _ <- execQuery (ShowSchedule.endValidity validity.stvId today)
        Log.logInfo "Closed out schedule validity" (show template.stId, show validity.stvId)

  -- For each added slot, create a fresh template and an open-ended validity
  -- period starting from today.
  forM_ (Set.toList added) $ \slot -> do
    let templateInsert =
          ShowSchedule.ScheduleTemplateInsert
            { ShowSchedule.stiShowId = showId,
              ShowSchedule.stiDayOfWeek = Just (pssDay slot),
              ShowSchedule.stiWeeksOfMonth = Just (pssWeeks slot),
              ShowSchedule.stiStartTime = pssStart slot,
              ShowSchedule.stiEndTime = pssEnd slot,
              ShowSchedule.stiTimezone = "America/Los_Angeles",
              ShowSchedule.stiAirsTwiceDaily = True
            }

    templateResult <- execQuery (ShowSchedule.insertScheduleTemplate templateInsert)
    case templateResult of
      Left err ->
        Log.logInfo "Failed to insert schedule template" (Text.pack $ show err)
      Right templateId -> do
        -- Open-ended validity: effective from today, no end date
        let validityInsert =
              ShowSchedule.ValidityInsert
                { ShowSchedule.viTemplateId = templateId,
                  ShowSchedule.viEffectiveFrom = today,
                  ShowSchedule.viEffectiveUntil = Nothing
                }
        validityResult <- execQuery (ShowSchedule.insertValidity validityInsert)
        case validityResult of
          Left err ->
            Log.logInfo "Failed to insert validity" (Text.pack $ show err)
          Right (Just _) ->
            Log.logInfo "Created new schedule for show" (show showId, show (pssDay slot))
          Right Nothing ->
            Log.logInfo "insertValidity returned Nothing" (show showId, show (pssDay slot))

--------------------------------------------------------------------------------
-- Host Update Helpers

-- | Update hosts for a show
--
-- Compares the new host list with the current hosts and:
-- 1. Removes hosts that are no longer in the list
-- 2. Adds hosts that are new to the list
-- 3. Promotes users to Host role if they aren't already Host or higher
--
-- Returns the list of newly added host IDs (for sending notification emails).
updateHostsForShow ::
  Shows.Id ->
  [User.Id] ->
  AppM [User.Id]
updateHostsForShow showId newHostIds = do
  let newHostSet = Set.fromList newHostIds

  -- Get current hosts
  currentHosts <-
    execQuery (ShowHost.getShowHosts showId) >>= \case
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
    _ <- execQuery (ShowHost.removeShowHost showId hostId)
    Log.logInfo "Removed host from show" (show showId, show hostId)

  -- Add new hosts
  forM_ hostsToAdd $ \hostId -> do
    -- Add host to show
    _ <- execQuery (ShowHost.addHostToShow showId hostId)
    Log.logInfo "Added host to show" (show showId, show hostId)

    -- Promote user to Host role if they're currently just a User
    execQuery (UserMetadata.getUserMetadata hostId) >>= \case
      Left err ->
        Log.logInfo "Failed to fetch user metadata for role promotion" (show err)
      Right Nothing ->
        Log.logInfo "User not found for role promotion" (show hostId)
      Right (Just userMeta) ->
        when (userMeta.mUserRole == UserMetadata.User) $ do
          _ <- execQuery (UserMetadata.updateUserRole hostId UserMetadata.Host)
          Log.logInfo "Promoted user to Host role" (show hostId)

  Log.logInfo "Host update complete" (show showId, "removed" :: Text, length hostsToRemove, "added" :: Text, length hostsToAdd)

  -- Return newly added hosts for notification
  pure hostsToAdd

--------------------------------------------------------------------------------
-- Tag Processing Helpers

-- | Process tags for a show
--
-- Clears all existing tags and re-adds tags from the comma-separated input.
-- Uses a create-or-reuse pattern: if a tag exists, it's reused; otherwise created.
processShowTags ::
  Shows.Id ->
  Maybe Text ->
  AppM ()
processShowTags showId mTagsText = do
  -- First, remove all existing tags from this show
  _ <- execQuery (Shows.removeAllTagsFromShow showId)
  Log.logInfo "Cleared existing tags for show" (show showId)

  -- Then add new tags
  case mTagsText of
    Nothing -> pure ()
    Just tagsText -> do
      let tagNames = filter (not . Text.null) $ map Text.strip $ Text.splitOn "," tagsText
      forM_ tagNames $ \tagName -> do
        -- Check if tag already exists
        execQuery (ShowTags.getShowTagByName tagName) >>= \case
          Right (Just existingTag) -> do
            -- Tag exists, just associate it with the show
            _ <- execQuery (Shows.addTagToShow showId (ShowTags.stId existingTag))
            Log.logInfo "Associated existing tag with show" (show showId, tagName)
          _ -> do
            -- Tag doesn't exist, create it and associate
            execQuery (ShowTags.insertShowTag (ShowTags.Insert tagName)) >>= \case
              Right (Just newTagId) -> do
                _ <- execQuery (Shows.addTagToShow showId newTagId)
                Log.logInfo "Created and associated new tag with show" (show showId, tagName)
              Right Nothing ->
                Log.logInfo "Tag insert returned Nothing" tagName
              Left err ->
                Log.logInfo "Failed to create tag" (tagName, show err)

--------------------------------------------------------------------------------
-- Notification Helpers

-- | Build a human-readable timeslot description from schedule slots.
--
-- Returns Nothing if no schedules, otherwise returns a formatted string
-- like "Fridays 8:00 PM - 10:00 PM PT"
buildTimeslotDescription :: [ParsedScheduleSlot] -> Maybe Text
buildTimeslotDescription [] = Nothing
buildTimeslotDescription (slot : _) =
  Just $ HostNotifications.formatTimeslotDescription (pssDay slot) (pssStart slot) (pssEnd slot)

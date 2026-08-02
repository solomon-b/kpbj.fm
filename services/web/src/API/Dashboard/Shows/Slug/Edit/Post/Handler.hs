{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Edit.Post.Handler
  ( handler,
    action,
    ParsedScheduleSlot (..),
    normalizeTemplate,
    parseScheduleSlot,
    schedulesMatch,
    validateNoOverlaps,
    checkScheduleConflicts,
    removedTemplates,
    scheduleUpdateFlash,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..), ShowEditForm (..))
import API.Links (dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (forM_, unless, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson qualified as Aeson
import Data.Function ((&))
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
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (LocalTime (..), addMinutesToTimeOfDay, minutesFromMidnight, parseDateYMD, parseTimeHHMM, slotDurationMins, utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization (sanitizeTitle)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Effects.HostNotifications qualified as HostNotifications
import Log qualified
import OrphanInstances.DayOfWeek (dayOfWeekFromText)
import Rel8 (Result)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | A schedule slot with all fields parsed and validated.
--
-- Produced by 'parseScheduleSlot'; all downstream functions operate on this
-- type instead of the raw 'ScheduleSlotInfo' form data.
data ParsedScheduleSlot = ParsedScheduleSlot
  { pssDay :: DayOfWeek,
    pssWeeks :: [Int64],
    pssStart :: TimeOfDay,
    pssEnd :: TimeOfDay,
    pssReplayStartTime :: Maybe TimeOfDay
  }
  deriving stock (Eq, Ord, Show)

--------------------------------------------------------------------------------

-- URL helpers
dashboardShowDetailUrl :: Shows.Id -> Slug -> Links.URI
dashboardShowDetailUrl showId slug = Links.linkURI $ dashboardShowsLinks.detail showId slug Nothing

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
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler slug cookie editForm =
  handleRedirectErrors "Show edit" (dashboardShowsLinks.editGet slug) $ do
    (user, userMetadata) <- requireAuth cookie
    requireShowHostOrStaff user.mId slug userMetadata
    (showId, newSlug, unscheduledEpisodes) <- action userMetadata slug editForm
    let showUrl = [i|/#{dashboardShowDetailUrl showId newSlug}|] :: Text
        flash = scheduleUpdateFlash unscheduledEpisodes
    pure $ Servant.addHeader showUrl $ Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

--------------------------------------------------------------------------------

-- | Business logic: fetch show, validate, update.
--
-- Returns the show ID, the (potentially updated) slug for the redirect, and any
-- upcoming episodes the schedule change unscheduled so the caller can report them.
action ::
  UserMetadata.Model ->
  Slug ->
  ShowEditForm ->
  ExceptT HandlerError AppM (Shows.Id, Slug, [Episodes.UpcomingEpisodeRef])
action userMetadata slug editForm = do
  -- 1. Fetch the show
  showModel <- fetchShowOrNotFound slug

  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- 2. Validate status
  parsedStatus <- case Shows.decodeStatus (sefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in show edit form" (sefStatus editForm)
      throwValidationError "Invalid show status value."
    Just s -> pure s
  let finalStatus = if isStaff then parsedStatus else showModel.status

  -- 3. Generate slug from title
  let generatedSlug = Slug.mkSlug (sefTitle editForm)

  -- 4. Process file uploads
  uploadResult <- lift $ processShowArtworkUploads generatedSlug (sefLogoFile editForm)
  mLogoPath <- case uploadResult of
    Left uploadErr -> do
      Log.logInfo "Failed to upload show artwork" uploadErr
      throwValidationError ("File upload error: " <> uploadErr)
    Right path -> pure path

  -- 5. Determine final logo URL: new upload > explicit clear > keep existing
  let finalLogoUrl = case (mLogoPath, sefLogoClear editForm) of
        (Just path, _) -> Just path
        (Nothing, True) -> Nothing
        (Nothing, False) -> showModel.logoUrl

      mDescription =
        let desc = sefDescription editForm
         in if Text.null (Text.strip desc) then Nothing else Just desc

      updateData =
        Shows.Insert
          { siTitle = sanitizeTitle (sefTitle editForm),
            siSlug = generatedSlug,
            siDescription = mDescription,
            siLogoUrl = finalLogoUrl,
            siStatus = finalStatus
          }

  -- 6. Update the show
  updateResult <- execQuery (Shows.updateShow showModel.id updateData)
  case updateResult of
    Left err -> do
      Log.logInfo "Failed to update show" (showModel.id, show err)
      throwDatabaseError err
    Right Nothing -> do
      Log.logInfo "Show update returned Nothing" showModel.id
      throwHandlerFailure "Failed to update show."
    Right (Just _updatedId) ->
      Log.logInfo "Successfully updated show" showModel.id

  -- 7. Process tags (fire and forget)
  lift $ processShowTags showModel.id (sefTags editForm)

  -- 8. Process schedule and host updates if staff
  unscheduledEpisodes <-
    if isStaff
      then do
        schedules <- case parseSchedules (sefSchedulesJson editForm) of
          Left err -> do
            Log.logInfo "Schedule validation failed" err
            throwValidationError err
          Right s -> pure s
        today <- localDay . utcToPacific <$> lift currentSystemTime
        mStartDate <- case sefScheduleStartDate editForm of
          Nothing -> pure Nothing
          Just dateText -> case parseDateYMD dateText of
            Nothing -> do
              Log.logInfo "Invalid schedule start date" dateText
              throwValidationError "Invalid schedule start date."
            Just d ->
              if d < today
                then do
                  Log.logInfo "Schedule start date in the past" (Text.pack (show d))
                  throwValidationError "The schedule start date can't be in the past."
                else pure (Just d)
        -- Date the submitted schedule takes effect. Deferred edits are checked
        -- against that future date, not today.
        let startDate = fromMaybe today mStartDate
        -- Only conflict-check a schedule that actually changed. The edit form always
        -- re-posts the show's current slots, so checking on every edit would reject
        -- unrelated changes (title, logo, hosts) whenever another show legitimately
        -- holds the same slot in a validity window that doesn't overlap this show's.
        -- This re-reads the active templates that 'updateSchedulesForShow' fetches
        -- again below. The duplicate query keeps the change small.
        scheduleUnchanged <-
          lift $
            execQuery (ShowSchedule.getActiveScheduleTemplatesForShow showModel.id) >>= \case
              Left err -> do
                -- Fail safe. Without the current schedule we can't tell whether it
                -- changed, so run the check and accept a possible false conflict.
                Log.logAttention "Failed to fetch active schedules for conflict check" (Text.pack $ show err)
                pure False
              Right templates -> pure (schedulesMatch templates schedules)
        unless scheduleUnchanged $ do
          conflictCheck <- lift $ checkScheduleConflicts showModel.id schedules startDate
          case conflictCheck of
            Left conflictErr -> do
              Log.logInfo "Schedule conflict with other show" conflictErr
              throwValidationError conflictErr
            Right () -> pure ()
        lift $ do
          unscheduled <- updateSchedulesForShow showModel.id schedules mStartDate
          newlyAddedHosts <- updateHostsForShow showModel.id (sefHosts editForm)
          let mTimeslot = buildTimeslotDescription schedules
          HostNotifications.sendHostAssignmentNotifications showModel mTimeslot newlyAddedHosts
          pure unscheduled
      else pure []

  -- 9. If the show is now inactive, close its schedule windows.
  --
  -- This runs after step 8 on purpose. Step 8 diffs the submitted form against the
  -- show's active templates and recreates any slot it does not find. If the close
  -- ran first, step 8 would see no active templates and write the slots back.
  --
  -- An inactive show must not hold a slot. The conflict check skips inactive shows,
  -- so a slot that stays claimed lets a later reactivation put two shows on one
  -- slot. Closing the windows here means a reactivated show has no schedule and
  -- must be booked again.
  deactivated <-
    if finalStatus == Shows.Inactive && showModel.status /= Shows.Inactive
      then lift $ closeSchedulesOnDeactivate showModel.id
      else pure []

  pure (showModel.id, generatedSlug, unscheduledEpisodes <> deactivated)

-- | Close a show's schedule windows and detach its upcoming episodes.
--
-- Returns the detached episodes so the caller can name them in the flash message.
-- A database error is logged and reported as no detached episodes, because the
-- show status has already been written and this action must not fail the save.
closeSchedulesOnDeactivate :: Shows.Id -> AppM [Episodes.UpcomingEpisodeRef]
closeSchedulesOnDeactivate showId = do
  today <- localDay . utcToPacific <$> currentSystemTime
  execQuery (Episodes.closeSchedulesAndDetachEpisodes showId today) >>= \case
    Left err -> do
      Log.logAttention "Failed to close schedules for deactivated show" (Text.pack $ show err)
      pure []
    Right detached -> do
      Log.logInfo "Closed schedules for deactivated show" (show showId, length detached)
      pure detached

-- | Fetch show by slug or throw NotFound
fetchShowOrNotFound ::
  Slug ->
  ExceptT HandlerError AppM Shows.Model
fetchShowOrNotFound slug =
  fromMaybeM (throwNotFound "Show") $
    fromRightM throwDatabaseError $
      execQuery (Shows.getShowBySlug slug)

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
  dow <- maybe (Left $ "Invalid day of week: " <> dayOfWeek slot) Right (dayOfWeekFromText (dayOfWeek slot))
  start <- maybe (Left $ "Invalid start time: " <> startTime slot) Right (parseTimeHHMM (startTime slot))
  let dur = duration slot
  if dur `notElem` [30, 60, 120]
    then Left $ "Invalid duration: " <> Text.pack (show dur) <> " (must be 30, 60, or 120)"
    else do
      let end = addMinutesToTimeOfDay start dur
      mReplay <- case replayTime slot of
        Nothing -> Right Nothing
        Just rt
          | Text.null (Text.strip rt) -> Right Nothing
          | otherwise -> case parseTimeHHMM rt of
              Nothing -> Left $ "Invalid replay time: " <> rt
              Just replayTod -> Right (Just replayTod)
      Right $
        ParsedScheduleSlot
          { pssDay = dow,
            pssWeeks = sort (weeksOfMonth slot),
            pssStart = start,
            pssEnd = end,
            pssReplayStartTime = mReplay
          }

-- | Validate that the slots of one submission do not overlap each other.
--
-- This is the check within a single show. 'checkScheduleConflicts' makes the
-- check against the other shows in the database.
--
-- Each slot expands into its primary range and, when it has one, its replay
-- range. Every pair then goes through 'slotsOverlap', which covers a slot that
-- crosses midnight onto the next day.
validateNoOverlaps :: [ParsedScheduleSlot] -> Either Text [ParsedScheduleSlot]
validateNoOverlaps slots =
  let -- Expand each slot into primary + optional replay virtual slot
      expandSlot s =
        let primary = s {pssReplayStartTime = Nothing}
            replay = case pssReplayStartTime s of
              Nothing -> []
              Just rt ->
                let dur = slotDurationMins (pssStart s) (pssEnd s)
                 in [ s
                        { pssStart = rt,
                          pssEnd = addMinutesToTimeOfDay rt dur,
                          pssReplayStartTime = Nothing
                        }
                    ]
         in primary : replay
      allVirtual = concatMap expandSlot slots
   in case findOverlap allVirtual of
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
--
-- Each slot becomes a half-open range of minutes from midnight of its own day.
-- See 'slotMinutes'. A slot that crosses midnight takes @[start, 1440)@ on its
-- own day and @[0, end - 1440)@ on the next day.
--
-- Two slots can meet on three days, and this checks all three. They can share a
-- day. The first can cross midnight onto the second's day. The second can cross
-- midnight onto the first's day.
--
-- 'ShowSchedule.checkTimeSlotConflict' applies the same three comparisons
-- against the other shows in the database.
slotsOverlap :: ParsedScheduleSlot -> ParsedScheduleSlot -> Bool
slotsOverlap slot1 slot2 = sameDay || tail1HitsSlot2 || tail2HitsSlot1
  where
    (start1, end1) = slotMinutes slot1
    (start2, end2) = slotMinutes slot2

    sameDay =
      pssDay slot1 == pssDay slot2
        && weeksOverlap (pssWeeks slot1) (pssWeeks slot2)
        && start1 < min end2 1440
        && min end1 1440 > start2

    tail1HitsSlot2 =
      end1 > 1440
        && pssDay slot2 == nextDayOfWeek (pssDay slot1)
        && weeksMeetAcrossMidnight (pssWeeks slot1) (pssWeeks slot2)
        && start2 < end1 - 1440

    tail2HitsSlot1 =
      end2 > 1440
        && pssDay slot1 == nextDayOfWeek (pssDay slot2)
        && weeksMeetAcrossMidnight (pssWeeks slot2) (pssWeeks slot1)
        && start1 < end2 - 1440

-- | A slot as a half-open range of minutes from midnight of the day it starts on.
--
-- The end goes above 1440 when the slot crosses midnight. A slot that stops at
-- midnight gets an end of exactly 1440 and does not cross.
slotMinutes :: ParsedScheduleSlot -> (Int, Int)
slotMinutes slot =
  let start = minutesFromMidnight (pssStart slot)
      end = minutesFromMidnight (pssEnd slot)
   in (start, if end > start then end else end + 1440)

-- | The next day of the week. Saturday gives Sunday.
nextDayOfWeek :: DayOfWeek -> DayOfWeek
nextDayOfWeek d = toEnum (fromEnum d + 1)

-- | Check if two lists of weeks share any common weeks.
weeksOverlap :: [Int64] -> [Int64] -> Bool
weeksOverlap weeks1 weeks2 = any (`elem` weeks2) weeks1

-- | The weeks of the month a date can fall in, given the week of the day before it.
--
-- Week @w@ covers the days @7(w - 1) + 1@ to @7w@, so the next day is in week
-- @w@ or week @w + 1@. The day after the last day of a month is in week 1, and
-- a month can end in week 4 (February) or week 5.
nextWeeks :: Int64 -> [Int64]
nextWeeks w = [w, w + 1] <> [1 | w >= 4]

-- | Check if a slot on the @earlier@ weeks can cross midnight onto a slot on the
-- @later@ weeks.
--
-- This can report a meeting that a concrete calendar would not produce. It never
-- misses one.
weeksMeetAcrossMidnight :: [Int64] -> [Int64] -> Bool
weeksMeetAcrossMidnight earlier later =
  any (any (`elem` later) . nextWeeks) earlier

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
          pssWeeks = sort (fromMaybe [1, 2, 3, 4, 5] t.stWeeksOfMonth),
          pssStart = t.stStartTime,
          pssEnd = t.stEndTime,
          pssReplayStartTime = t.stReplayStartTime
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

-- | Active templates whose slot signature is absent from the submitted form.
--
-- These are the templates 'updateScheduleTemplates' will terminate and detach
-- episodes from. That means any active template whose normalized slot is not
-- present in the form, either deleted outright or re-keyed to a different
-- signature. Templates with no day of week (which cannot normalize) are dropped,
-- matching 'normalizeTemplate'.
removedTemplates ::
  [ShowSchedule.ScheduleTemplate Result] ->
  [ParsedScheduleSlot] ->
  [ShowSchedule.ScheduleTemplate Result]
removedTemplates activeTemplates parsedSlots =
  let templateMap :: Map.Map ParsedScheduleSlot [ShowSchedule.ScheduleTemplate Result]
      templateMap =
        foldMap
          ( \t ->
              normalizeTemplate t & \case
                Just slot -> Map.singleton slot [t]
                Nothing -> Map.empty
          )
          activeTemplates
      dbSet = Map.keysSet templateMap
      formSet = Set.fromList parsedSlots
      removed = Set.difference dbSet formSet
   in concatMap (\slot -> Map.findWithDefault [] slot templateMap) (Set.toList removed)

-- | Warning-flash body listing the upcoming episodes that a schedule edit just
-- unscheduled. They keep their audio and show up flagged "UNSCHEDULED" in the
-- dashboard. Staff assign each a new slot from the episode edit page.
renderUnscheduledNotice :: [Episodes.UpcomingEpisodeRef] -> Text
renderUnscheduledNotice eps =
  let n = length eps
      describeEp ep =
        let epNum = Episodes.unEpisodeNumber ep.uerEpisodeNumber
            scheduledText =
              Text.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M" (utcToPacific ep.uerScheduledAt)
         in [i|Episode \##{epNum} (was #{scheduledText})|] :: Text
      listing = Text.intercalate ", " (map describeEp eps)
   in [i|Your show was updated, but #{n} upcoming episode(s) were unscheduled because their time slot changed: #{listing}. They now show as UNSCHEDULED in the dashboard and need a new slot.|]

-- | Success flash for a show edit, downgraded to a Warning that names the
-- episodes when the schedule change unscheduled any upcoming episodes.
scheduleUpdateFlash :: [Episodes.UpcomingEpisodeRef] -> FlashMessage
scheduleUpdateFlash [] =
  FlashMessage Success "Show Updated" "Your show has been updated successfully."
scheduleUpdateFlash eps =
  FlashMessage Warning "Show Updated" (renderUnscheduledNotice eps)

--------------------------------------------------------------------------------

-- | Check for schedule conflicts with other shows.
--
-- Checks both primary and replay time ranges against the database. @fromDate@ is
-- the date the proposed schedule takes effect. Other shows' validity windows that
-- have already closed by then are not conflicts.
checkScheduleConflicts ::
  Shows.Id ->
  [ParsedScheduleSlot] ->
  Day ->
  AppM (Either Text ())
checkScheduleConflicts showId slots fromDate = go slots
  where
    go [] = pure (Right ())
    go (slot : rest) = do
      let weeks = map fromIntegral (pssWeeks slot)
      -- Check primary slot
      execQuery (ShowSchedule.checkTimeSlotConflict showId (pssDay slot) weeks (pssStart slot) (pssEnd slot) fromDate) >>= \case
        Left err -> do
          Log.logAttention "Failed to check schedule conflict" (Text.pack $ show err)
          pure (Left "Unable to verify schedule availability. Please try again.")
        Right (Just conflictingShow) ->
          pure (Left $ "Schedule conflict: " <> Text.pack (show (pssDay slot)) <> " " <> formatTimeHHMM (pssStart slot) <> "-" <> formatTimeHHMM (pssEnd slot) <> " overlaps with \"" <> conflictingShow <> "\"")
        Right Nothing ->
          -- Check replay slot if set
          case pssReplayStartTime slot of
            Nothing -> go rest
            Just replayStart -> do
              let dur = slotDurationMins (pssStart slot) (pssEnd slot)
                  replayEnd = addMinutesToTimeOfDay replayStart dur
              execQuery (ShowSchedule.checkTimeSlotConflict showId (pssDay slot) weeks replayStart replayEnd fromDate) >>= \case
                Left err -> do
                  Log.logAttention "Failed to check replay conflict" (Text.pack $ show err)
                  pure (Left "Unable to verify schedule availability. Please try again.")
                Right (Just conflictingShow) ->
                  pure (Left $ "Replay conflict: " <> Text.pack (show (pssDay slot)) <> " " <> formatTimeHHMM replayStart <> "-" <> formatTimeHHMM replayEnd <> " overlaps with \"" <> conflictingShow <> "\"")
                Right Nothing -> go rest

-- | Update schedules for a show.
--
-- Compares the incoming form schedule against the current DB schedule. If they
-- match, skips the terminate-and-recreate cycle. This prevents orphaning episodes
-- that are linked to the existing schedule templates.
--
-- \"Current\" means the pending templates when a pending schedule exists, and the
-- active ones otherwise, mirroring which set the edit form was populated from. The
-- start date is part of the comparison because 'schedulesMatch' only looks at slot
-- signatures.
--
-- Only once the submitted schedule actually differs is a pending schedule cancelled
-- (validity terminated, its episodes detached, active templates restored to
-- open-ended). The diff itself is then applied against the active templates.
--
-- When @mStartDate@ is provided it is used as the @effective_from@ date for any
-- newly inserted validity records. When absent the current Pacific date is used.
updateSchedulesForShow ::
  Shows.Id ->
  [ParsedScheduleSlot] ->
  Maybe Day ->
  AppM [Episodes.UpcomingEpisodeRef]
updateSchedulesForShow showId newSchedules mStartDate = do
  -- Use Pacific time as default start date when none provided
  nowUtc <- currentSystemTime
  let startDate = fromMaybe (localDay (utcToPacific nowUtc)) mStartDate

  activeTemplates <-
    execQuery (ShowSchedule.getActiveScheduleTemplatesForShow showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch active schedules" (Text.pack $ show err)
        pure []
      Right templates -> pure templates

  pendingTemplates <-
    execQuery (ShowSchedule.getPendingScheduleTemplatesForShow showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch pending schedules" (Text.pack $ show err)
        pure []
      Right templates -> pure templates

  -- The edit form is populated from the pending schedule when one exists (see
  -- 'API.Dashboard.Shows.Slug.Edit.Get.Handler'), so an unrelated save re-posts the
  -- pending slots verbatim. Compare against whatever the form was filled from, not
  -- always the active templates, or a title-only edit reads as a schedule change and
  -- destroys the pending below.
  let currentTemplates = if null pendingTemplates then activeTemplates else pendingTemplates
  startDateUnchanged <- pendingStartDateMatches pendingTemplates mStartDate

  if schedulesMatch currentTemplates newSchedules && startDateUnchanged
    then do
      Log.logInfo "Schedule unchanged, skipping update" (show showId)
      pure []
    else do
      Log.logInfo "Schedule changed, updating" (show showId)
      -- Cancel any pending schedule so the diff below runs against a clean active
      -- state. Only reached when the submitted schedule actually differs.
      unless (null pendingTemplates) $
        cancelPendingSchedule pendingTemplates activeTemplates
      let removedIds = map (.stId) (removedTemplates activeTemplates newSchedules)
      unscheduled <-
        if null removedIds
          then pure []
          else
            execQuery (Episodes.getUpcomingEpisodesForTemplates removedIds startDate) >>= \case
              Left err -> do
                Log.logInfo "Failed to fetch episodes to be unscheduled" (Text.pack $ show err)
                pure []
              Right eps -> pure eps
      updateScheduleTemplates showId activeTemplates newSchedules startDate
      pure unscheduled

-- | Whether the submitted start date matches the pending schedule's existing one.
--
-- 'schedulesMatch' compares slot signatures only (day, weeks, times), so without this
-- a save that moves a pending schedule's start date and changes nothing else would
-- read as unchanged and be silently dropped.
--
-- Vacuously 'True' when there is no pending schedule, since there is no date to move.
-- Fails open: if the validity periods can't be read we report a change, which falls
-- through to the existing update path rather than discarding the edit.
pendingStartDateMatches ::
  [ShowSchedule.ScheduleTemplate Result] ->
  Maybe Day ->
  AppM Bool
pendingStartDateMatches [] _ = pure True
pendingStartDateMatches (template : _) mStartDate =
  execQuery (ShowSchedule.getValidityPeriodsForTemplate template.stId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch pending validity for start-date comparison" (Text.pack $ show err)
      pure False
    Right validities -> case map (.stvEffectiveFrom) validities of
      [] -> pure False
      froms -> pure (mStartDate == Just (minimum froms))

-- | Cancel a pending schedule, restoring active templates to open-ended.
--
-- 1. Terminates each pending template's validity by setting effective_until = effective_from
-- 2. Detaches any episode already uploaded against the pending slot
-- 3. Restores active templates' validity to open-ended (clears effective_until)
--
-- Step 2 matters because a cancelled pending's validity becomes the empty window
-- @[from, from)@, which no date satisfies. An episode left pointing at one is
-- invisible to 'Episodes.getCurrentlyAiringEpisode' and airs as silence with no
-- warning. Pending slots are bookable (they appear in
-- 'ShowSchedule.getUpcomingUnscheduledShowDates'), so episodes really do accumulate
-- on them. Detaching leaves the episode UNSCHEDULED instead, which keeps its audio,
-- flags it in the dashboard, and lets staff reassign it.
cancelPendingSchedule ::
  [ShowSchedule.ScheduleTemplate Result] ->
  [ShowSchedule.ScheduleTemplate Result] ->
  AppM ()
cancelPendingSchedule pendingTemplates activeTemplates = do
  -- Cancel pending validity periods
  forM_ pendingTemplates $ \template -> do
    validities <-
      execQuery (ShowSchedule.getValidityPeriodsForTemplate template.stId) >>= \case
        Left err -> do
          Log.logInfo "Failed to fetch pending validity periods" (Text.pack $ show err)
          pure []
        Right vs -> pure vs
    forM_ validities $ \validity -> do
      _ <- execQuery (ShowSchedule.endValidity validity.stvId validity.stvEffectiveFrom)
      Log.logInfo "Cancelled pending schedule validity" (show template.stId, show validity.stvId)

    -- Detach episodes booked against the slot being cancelled. A pending template's
    -- episodes all fall on or after its effective_from, so clearing from the earliest
    -- one covers them.
    case map (.stvEffectiveFrom) validities of
      [] -> pure ()
      froms ->
        execQuery (Episodes.clearTemplateForUpcomingEpisodes template.stId (minimum froms)) >>= \case
          Left err -> Log.logAttention "Failed to clear template from cancelled pending's episodes" (show err)
          Right ids -> Log.logInfo "Detached episodes from cancelled pending" (show template.stId, length ids)

  -- Restore active validity periods to open-ended
  forM_ activeTemplates $ \template -> do
    activeValidities <-
      execQuery (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId) >>= \case
        Left err -> do
          Log.logInfo "Failed to fetch active validity periods" (Text.pack $ show err)
          pure []
        Right vs -> pure vs
    forM_ activeValidities $ \validity ->
      case validity.stvEffectiveUntil of
        Just _ -> do
          _ <- execQuery (ShowSchedule.restoreValidity validity.stvId)
          Log.logInfo "Restored active validity to open-ended" (show template.stId, show validity.stvId)
        Nothing -> pure ()

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
updateScheduleTemplates showId activeTemplates parsedSlots startDate = do
  let -- Normalize each DB template into a ParsedScheduleSlot so we can compute the
      -- set of newly added slots (those in the form but absent from the DB).
      -- Templates with no day of week (shouldn't happen in practice) are dropped.
      dbSet = Set.fromList (mapMaybe normalizeTemplate activeTemplates)
      formSet = Set.fromList parsedSlots

      -- Slots in form but not in DB — user added these
      added = Set.difference formSet dbSet

  -- For each removed (or re-keyed) template, end its active validity periods by
  -- setting effective_until to startDate, then detach its upcoming episodes.
  forM_ (removedTemplates activeTemplates parsedSlots) $ \template -> do
    activeValidities <-
      execQuery (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId) >>= \case
        Left err -> do
          Log.logInfo "Failed to fetch validity periods" (Text.pack $ show err)
          pure []
        Right validities -> pure validities

    forM_ activeValidities $ \validity -> do
      _ <- execQuery (ShowSchedule.endValidity validity.stvId startDate)
      Log.logInfo "Closed out schedule validity" (show template.stId, show validity.stvId)

    -- Detach upcoming episodes from this expired template, but only those airing
    -- on or after the change date so interim episodes keep their slot.
    execQuery (Episodes.clearTemplateForUpcomingEpisodes template.stId startDate) >>= \case
      Left err -> Log.logAttention "Failed to clear template from episodes" (show err)
      Right ids -> Log.logInfo "Detached episodes from expired template" (show template.stId, length ids)

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
              ShowSchedule.stiReplayStartTime = pssReplayStartTime slot
            }

    templateResult <- execQuery (ShowSchedule.insertScheduleTemplate templateInsert)
    case templateResult of
      Left err ->
        Log.logInfo "Failed to insert schedule template" (Text.pack $ show err)
      Right templateId -> do
        -- Open-ended validity: effective from startDate, no end date
        let validityInsert =
              ShowSchedule.ValidityInsert
                { ShowSchedule.viTemplateId = templateId,
                  ShowSchedule.viEffectiveFrom = startDate,
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

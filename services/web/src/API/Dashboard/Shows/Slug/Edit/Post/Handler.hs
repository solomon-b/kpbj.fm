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
    validateSingleSlot,
    checkScheduleConflicts,
    removedTemplates,
    scheduleUpdateFlash,
    insertScheduleSlot,
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
import Control.Monad (forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (Day, TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Recurrence (Recurrence, editorCanShow, parseWeeks, recurrenceDay, recurrenceFromRow, recurring, weekNumbers, weeksLabel)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (LocalTime (..), addMinutesToTimeOfDay, minutesFromMidnight, parseDateYMD, parseTimeHHMM, slotDurationMins, utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization (sanitizeTitle)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Effects.HostNotifications qualified as HostNotifications
import Hasql.Pool (UsageError)
import Hasql.Transaction qualified as HT
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
--
-- The recurrence keeps its weeks sorted and free of duplicates, so 'Eq' and 'Ord'
-- decide whether a schedule changed.
data ParsedScheduleSlot = ParsedScheduleSlot
  { pssRecurrence :: Recurrence,
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
        -- One transaction. A failure here leaves the show's schedule and its
        -- episodes exactly as they were, and reaches the user as an error banner.
        update <-
          lift (updateSchedulesForShow showModel.id schedules mStartDate today) >>= \case
            Left (ScheduleDbError dbErr) -> do
              Log.logAttention "Schedule update rolled back" (Aeson.object ["show.id" .= showModel.id, "error" .= Text.pack (show dbErr)])
              throwDatabaseError dbErr
            Left (ScheduleInvariant msg) -> do
              Log.logAttention "Schedule update condemned" (Aeson.object ["show.id" .= showModel.id, "error" .= msg])
              throwHandlerFailure msg
            Right update -> pure update
        lift $ do
          Log.logInfo "Schedule update committed" (scheduleUpdateLog showModel.id update)
          newlyAddedHosts <- updateHostsForShow showModel.id (sefHosts editForm)
          let mTimeslot = buildTimeslotDescription schedules
          HostNotifications.sendHostAssignmentNotifications showModel mTimeslot newlyAddedHosts
          pure update.suUnscheduled
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
parseSchedules :: Maybe Text -> Either Text (Maybe ParsedScheduleSlot)
parseSchedules Nothing = Right Nothing
parseSchedules (Just schedulesJson)
  | Text.null (Text.strip schedulesJson) = Right Nothing
  | schedulesJson == "[]" = Right Nothing
  | otherwise = case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
      Left err -> Left $ "Invalid schedules JSON: " <> Text.pack err
      Right slots -> do
        parsed <- traverse parseScheduleSlot slots
        validateSingleSlot parsed

-- | Parse and validate a single schedule slot from form data.
--
-- The field order decides which message a submission with several bad fields gets,
-- so the weeks are parsed through 'parseWeeks' rather than 'parseRecurrence' to keep
-- the day, then start time, then weeks ordering.
parseScheduleSlot :: ScheduleSlotInfo -> Either Text ParsedScheduleSlot
parseScheduleSlot slot = do
  dow <- maybe (Left $ "Invalid day of week: " <> dayOfWeek slot) Right (dayOfWeekFromText (dayOfWeek slot))
  start <- maybe (Left $ "Invalid start time: " <> startTime slot) Right (parseTimeHHMM (startTime slot))
  weeks <- parseWeeks (weeksOfMonth slot)
  validateEditorCanShow (recurring dow weeks)
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
          { pssRecurrence = recurring dow weeks,
            pssStart = start,
            pssEnd = end,
            pssReplayStartTime = mReplay
          }

-- | Reject a week set the schedule editor cannot show.
--
-- @weeks_of_month@ holds any non-empty subset of 1 to 5. The editor produces seven of
-- those 31, so it can render the other 24 only as a frequency button with no week
-- button beside it. A member of staff who clicks one to fill that gap rewrites the
-- show's schedule, which is the shape of defect this check exists to stop.
--
-- The column keeps the full range, so giving the editor controls for the remaining 24
-- means widening 'editorWeekSets' and nothing else. Until then this is the gate, and
-- it sits at the parse boundary all three write paths share.
--
-- The schedule editor cannot trip this. It takes a hand-written POST.
validateEditorCanShow :: Recurrence -> Either Text ()
validateEditorCanShow recurrence
  | editorCanShow recurrence = Right ()
  | otherwise =
      Left $
        "The schedule form cannot show "
          <> fromMaybe "that set of weeks" (weeksLabel recurrence)
          <> ". Pick every week, the 1st and 3rd, the 2nd and 4th, or one week from the 1st to the 4th."

-- | Reduce a submission to the one slot a show may hold.
--
-- @one_active_slot_per_show@ makes a second concurrent slot unrepresentable, so a
-- form carrying two is rejected here with a message naming the problem rather than by
-- the database with an exclusion violation. The editor emits at most one.
--
-- The surviving slot still has to clear its own replay. Those two ranges share a day
-- and a recurrence, so 'checkScheduleConflicts' cannot see a collision between them.
-- It asks the database about other shows only.
validateSingleSlot :: [ParsedScheduleSlot] -> Either Text (Maybe ParsedScheduleSlot)
validateSingleSlot [] = Right Nothing
validateSingleSlot [slot] = Just slot <$ validateReplayGap slot
validateSingleSlot slots =
  Left $
    "A show holds one time slot, but this form sent "
      <> Text.pack (show (length slots))
      <> ". Remove the extra slots and try again."

-- | Reject a replay that runs over the airing it replays.
--
-- Both ranges start on the same weekday. Two same-day ranges that each cross
-- midnight must both cover the minute before midnight, so checking the shared day is
-- enough; there is no separate case for the tails.
validateReplayGap :: ParsedScheduleSlot -> Either Text ()
validateReplayGap slot = case pssReplayStartTime slot of
  Nothing -> Right ()
  Just replayStart ->
    let dur = slotDurationMins (pssStart slot) (pssEnd slot)
        replayEnd = addMinutesToTimeOfDay replayStart dur
        (primaryFrom, primaryTo) = minuteRange (pssStart slot) (pssEnd slot)
        (replayFrom, replayTo) = minuteRange replayStart replayEnd
     in if primaryFrom < min replayTo 1440 && min primaryTo 1440 > replayFrom
          then
            Left $
              "Schedule conflict: the replay at "
                <> formatTimeHHMM replayStart
                <> "-"
                <> formatTimeHHMM replayEnd
                <> " overlaps the airing at "
                <> formatTimeHHMM (pssStart slot)
                <> "-"
                <> formatTimeHHMM (pssEnd slot)
          else Right ()

-- | A time range as half-open minutes from midnight of the day it starts on.
--
-- The end goes above 1440 when the range crosses midnight. A range that stops at
-- midnight gets an end of exactly 1440 and does not cross.
minuteRange :: TimeOfDay -> TimeOfDay -> (Int, Int)
minuteRange start end =
  let s = minutesFromMidnight start
      e = minutesFromMidnight end
   in (s, if e > s then e else e + 1440)

-- | Format a TimeOfDay as "HH:MM" for error messages.
formatTimeHHMM :: TimeOfDay -> Text
formatTimeHHMM = Text.pack . formatTime defaultTimeLocale "%H:%M"

--------------------------------------------------------------------------------
-- Schedule Diff Helpers

-- | Normalize a DB template to a 'ParsedScheduleSlot' for comparison.
--
-- Total, because every template carries a day and a non-empty week set. So every
-- active template reaches both 'schedulesMatch' and 'removedTemplates', and a
-- schedule change closes all of them.
normalizeTemplate :: ShowSchedule.ScheduleTemplate Result -> ParsedScheduleSlot
normalizeTemplate t =
  ParsedScheduleSlot
    { pssRecurrence = recurrenceFromRow t.stDayOfWeek t.stWeeksOfMonth,
      pssStart = t.stStartTime,
      pssEnd = t.stEndTime,
      pssReplayStartTime = t.stReplayStartTime
    }

-- | Check if parsed form schedule matches current DB schedule.
--
-- Both sides are compared as sets of 'ParsedScheduleSlot'. A show holds one slot, so
-- the sets hold at most one element each. Taking a list of templates keeps the
-- comparison correct for a show that somehow carries more.
schedulesMatch :: [ShowSchedule.ScheduleTemplate Result] -> Maybe ParsedScheduleSlot -> Bool
schedulesMatch dbTemplates parsedSlot =
  let dbSet = Set.fromList (map normalizeTemplate dbTemplates)
      formSet = maybe Set.empty Set.singleton parsedSlot
   in dbSet == formSet

-- | The part of a slot that decides when an episode airs.
--
-- 'Effects.Database.Tables.Episodes.getCurrentlyAiringEpisodes' builds the primary
-- window from the recurrence, the start time, and the end time. A template that
-- keeps all three still airs every episode that it aired before.
--
-- The replay start time is absent on purpose. A replay is a second window on the
-- same recurrence, and a move of that window changes no episode's airing. The diff
-- therefore treats a replay change as an edit of the template, not as a removal and
-- an addition. See 'updateScheduleTemplates'.
type SlotIdentity = (Recurrence, TimeOfDay, TimeOfDay)

slotIdentity :: ParsedScheduleSlot -> SlotIdentity
slotIdentity slot = (pssRecurrence slot, pssStart slot, pssEnd slot)

-- | Active templates whose slot identity is absent from the submitted form.
--
-- These are the templates 'updateScheduleTemplates' terminates and detaches episodes
-- from. The form either dropped the slot or moved it to a different day or time.
removedTemplates ::
  [ShowSchedule.ScheduleTemplate Result] ->
  Maybe ParsedScheduleSlot ->
  [ShowSchedule.ScheduleTemplate Result]
removedTemplates activeTemplates parsedSlot =
  filter (\t -> Just (slotIdentity (normalizeTemplate t)) /= fmap slotIdentity parsedSlot) activeTemplates

-- | Active templates that hold the submitted slot identity.
--
-- A show holds one slot, so this list holds at most one template. A template here
-- keeps its episodes. Only its replay time can differ from the form.
keptTemplates ::
  [ShowSchedule.ScheduleTemplate Result] ->
  Maybe ParsedScheduleSlot ->
  [ShowSchedule.ScheduleTemplate Result]
keptTemplates activeTemplates parsedSlot =
  filter (\t -> Just (slotIdentity (normalizeTemplate t)) == fmap slotIdentity parsedSlot) activeTemplates

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
  Maybe ParsedScheduleSlot ->
  Day ->
  AppM (Either Text ())
checkScheduleConflicts _ Nothing _ = pure (Right ())
checkScheduleConflicts showId (Just slot) fromDate = do
  let day = recurrenceDay (pssRecurrence slot)
      weeks = weekNumbers (pssRecurrence slot)
      dayText = Text.pack (show day)
  -- Check primary slot
  execQuery (ShowSchedule.checkTimeSlotConflict showId day weeks (pssStart slot) (pssEnd slot) fromDate) >>= \case
    Left err -> do
      Log.logAttention "Failed to check schedule conflict" (Text.pack $ show err)
      pure (Left "Unable to verify schedule availability. Please try again.")
    Right (Just conflictingShow) ->
      pure (Left $ "Schedule conflict: " <> dayText <> " " <> formatTimeHHMM (pssStart slot) <> "-" <> formatTimeHHMM (pssEnd slot) <> " overlaps with \"" <> conflictingShow <> "\"")
    Right Nothing ->
      -- Check replay slot if set
      case pssReplayStartTime slot of
        Nothing -> pure (Right ())
        Just replayStart -> do
          let dur = slotDurationMins (pssStart slot) (pssEnd slot)
              replayEnd = addMinutesToTimeOfDay replayStart dur
          execQuery (ShowSchedule.checkTimeSlotConflict showId day weeks replayStart replayEnd fromDate) >>= \case
            Left err -> do
              Log.logAttention "Failed to check replay conflict" (Text.pack $ show err)
              pure (Left "Unable to verify schedule availability. Please try again.")
            Right (Just conflictingShow) ->
              pure (Left $ "Replay conflict: " <> dayText <> " " <> formatTimeHHMM replayStart <> "-" <> formatTimeHHMM replayEnd <> " overlaps with \"" <> conflictingShow <> "\"")
            Right Nothing -> pure (Right ())

-- | What one schedule update changed.
--
-- A 'HT.Transaction' has no 'MonadIO', so the transaction cannot log. It reports
-- what it did instead, and the caller writes one line after the commit.
data ScheduleUpdate = ScheduleUpdate
  { -- | Upcoming episodes the change detached. Named in the flash message.
    suUnscheduled :: [Episodes.UpcomingEpisodeRef],
    -- | Pending templates whose validity was cancelled.
    suCancelledPending :: [ShowSchedule.TemplateId],
    -- | Active templates the diff removed, so their validity was end-dated and
    -- their upcoming episodes detached.
    suClosed :: [ShowSchedule.TemplateId],
    -- | Templates created for the newly added slots.
    suCreated :: [ShowSchedule.TemplateId],
    -- | Upcoming episodes moved onto a new template, which kept their air times.
    -- A deferred replay change produces these instead of detached episodes.
    suMigrated :: [Episodes.Id],
    -- | 'False' when the submitted schedule matched the stored one, so nothing ran.
    suChanged :: Bool
  }
  deriving stock (Show, Eq)

-- | Why a schedule update failed. Either way the transaction wrote nothing.
data ScheduleUpdateError
  = -- | A statement failed and the transaction rolled back.
    ScheduleDbError UsageError
  | -- | The transaction reached a state it cannot proceed from, so it condemned itself.
    ScheduleInvariant Text

-- | An update that ran no statements, because the submitted schedule already matched.
noScheduleChange :: ScheduleUpdate
noScheduleChange = ScheduleUpdate [] [] [] [] [] False

-- | JSON body for the single log line a committed schedule update writes.
scheduleUpdateLog :: Shows.Id -> ScheduleUpdate -> Aeson.Value
scheduleUpdateLog showId update =
  Aeson.object
    [ "show.id" .= showId,
      "changed" .= update.suChanged,
      "cancelled_pending" .= update.suCancelledPending,
      "closed" .= update.suClosed,
      "created" .= update.suCreated,
      "migrated" .= update.suMigrated,
      "unscheduled" .= map (.uerId) update.suUnscheduled
    ]

-- | Update schedules for a show, in one transaction.
--
-- Every statement below runs in a single 'execTransaction'. The removals end a
-- validity period and clear @scheduled_at@ from the upcoming episodes, so a later
-- failure that committed on its own would destroy a show's schedule and leave no
-- way to recover the episodes' air times.
--
-- The reads are inside the transaction too. They feed the diff that decides which
-- templates get destroyed, so a concurrent edit landing between the read and the
-- write is the same class of fault.
--
-- 'HT.Transaction' has no 'MonadIO', and
-- 'Hasql.Transaction.Sessions.transaction' retries the body on a serialization
-- conflict. So @today@ arrives as an argument rather than from the clock, and the
-- caller does the logging.
updateSchedulesForShow ::
  Shows.Id ->
  Maybe ParsedScheduleSlot ->
  Maybe Day ->
  -- | Today in Pacific. Used as the effective date when the form gives none.
  Day ->
  AppM (Either ScheduleUpdateError ScheduleUpdate)
updateSchedulesForShow showId newSchedules mStartDate today =
  execTransaction (runExceptT (scheduleUpdateTx showId newSchedules mStartDate today)) >>= \case
    Left dbErr -> pure $ Left (ScheduleDbError dbErr)
    Right (Left msg) -> pure $ Left (ScheduleInvariant msg)
    Right (Right update) -> pure $ Right update

-- | The body of a schedule update.
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
-- newly inserted validity records. When absent @today@ is used.
scheduleUpdateTx ::
  Shows.Id ->
  Maybe ParsedScheduleSlot ->
  Maybe Day ->
  Day ->
  ExceptT Text HT.Transaction ScheduleUpdate
scheduleUpdateTx showId newSchedules mStartDate today = do
  let startDate = fromMaybe today mStartDate

  activeTemplates <- lift $ HT.statement () (ShowSchedule.getActiveScheduleTemplatesForShow showId)
  pendingTemplates <- lift $ HT.statement () (ShowSchedule.getPendingScheduleTemplatesForShow showId)

  -- The edit form is populated from the pending schedule when one exists (see
  -- 'API.Dashboard.Shows.Slug.Edit.Get.Handler'), so an unrelated save re-posts the
  -- pending slots verbatim. Compare against whatever the form was filled from, not
  -- always the active templates, or a title-only edit reads as a schedule change and
  -- destroys the pending below.
  let currentTemplates = if null pendingTemplates then activeTemplates else pendingTemplates
  startDateUnchanged <- lift $ pendingStartDateMatches pendingTemplates mStartDate

  if schedulesMatch currentTemplates newSchedules && startDateUnchanged
    then pure noScheduleChange
    else do
      -- Cancel any pending schedule so the diff below runs against a clean active
      -- state. Only reached when the submitted schedule actually differs.
      cancelled <-
        if null pendingTemplates
          then pure []
          else lift $ cancelPendingSchedule pendingTemplates activeTemplates

      -- Read the episodes about to be detached before the detach runs.
      let removedIds = map (.stId) (removedTemplates activeTemplates newSchedules)
      unscheduled <-
        if null removedIds
          then pure []
          else lift $ HT.statement () (Episodes.getUpcomingEpisodesForTemplates removedIds startDate)

      (closed, created, migrated) <- updateScheduleTemplates showId activeTemplates newSchedules startDate today
      pure
        ScheduleUpdate
          { suUnscheduled = unscheduled,
            suCancelledPending = cancelled,
            suClosed = closed,
            suCreated = created,
            suMigrated = migrated,
            suChanged = True
          }

-- | Whether the submitted start date matches the pending schedule's existing one.
--
-- 'schedulesMatch' compares slot signatures only (day, weeks, times), so without this
-- a save that moves a pending schedule's start date and changes nothing else would
-- read as unchanged and be silently dropped.
--
-- Vacuously 'True' when there is no pending schedule, since there is no date to move.
-- A pending template with no validity period reports a change, which falls through to
-- the update path rather than discarding the edit.
pendingStartDateMatches ::
  [ShowSchedule.ScheduleTemplate Result] ->
  Maybe Day ->
  HT.Transaction Bool
pendingStartDateMatches [] _ = pure True
pendingStartDateMatches (template : _) mStartDate = do
  validities <- HT.statement () (ShowSchedule.getValidityPeriodsForTemplate template.stId)
  pure $ case map (.stvEffectiveFrom) validities of
    [] -> False
    froms -> mStartDate == Just (minimum froms)

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
--
-- Returns the pending templates it cancelled.
cancelPendingSchedule ::
  [ShowSchedule.ScheduleTemplate Result] ->
  [ShowSchedule.ScheduleTemplate Result] ->
  HT.Transaction [ShowSchedule.TemplateId]
cancelPendingSchedule pendingTemplates activeTemplates = do
  -- Cancel pending validity periods
  forM_ pendingTemplates $ \template -> do
    validities <- HT.statement () (ShowSchedule.getValidityPeriodsForTemplate template.stId)
    forM_ validities $ \validity ->
      void $ HT.statement () (ShowSchedule.endValidity validity.stvId validity.stvEffectiveFrom)

    -- Detach episodes booked against the slot being cancelled. A pending template's
    -- episodes all fall on or after its effective_from, so clearing from the earliest
    -- one covers them.
    case map (.stvEffectiveFrom) validities of
      [] -> pure ()
      froms ->
        void $ HT.statement () (Episodes.clearTemplateForUpcomingEpisodes template.stId (minimum froms))

  -- Restore active validity periods to open-ended
  forM_ activeTemplates $ \template -> do
    activeValidities <- HT.statement () (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId)
    forM_ activeValidities $ \validity ->
      case validity.stvEffectiveUntil of
        Just _ -> void $ HT.statement () (ShowSchedule.restoreValidity validity.stvId)
        Nothing -> pure ()

  pure (map (.stId) pendingTemplates)

-- | Apply the slot diff. End removed slots, create added slots, keep the rest.
--
-- The diff compares slot identities rather than whole slots. See 'slotIdentity'.
--
--   removed = the active templates whose identity the form does not hold
--   added   = the form's slot, when no active template holds its identity
--   kept    = the active template that holds it, if there is one
--
-- A kept template keeps its id and its validity window, and its episodes keep their
-- foreign key. Only the replay time can differ, and 'retimeReplay' handles that.
--
-- Returns the templates it closed, the templates it created, and the episodes it
-- moved between templates.
updateScheduleTemplates ::
  Shows.Id ->
  [ShowSchedule.ScheduleTemplate Result] ->
  Maybe ParsedScheduleSlot ->
  -- | The date the change takes effect
  Day ->
  -- | Today in Pacific
  Day ->
  ExceptT Text HT.Transaction ([ShowSchedule.TemplateId], [ShowSchedule.TemplateId], [Episodes.Id])
updateScheduleTemplates showId activeTemplates parsedSlot startDate today = do
  let dbIdentities = Set.fromList (map (slotIdentity . normalizeTemplate) activeTemplates)

      -- The form's slot, when no stored template holds its identity.
      added = filter (\slot -> not (Set.member (slotIdentity slot) dbIdentities)) (maybe [] pure parsedSlot)

      removed = removedTemplates activeTemplates parsedSlot

  case (parsedSlot, keptTemplates activeTemplates parsedSlot) of
    (Just slot, [template]) -> retimeReplay showId slot template startDate today
    _ -> do
      -- For each removed (or re-keyed) template, end its active validity periods by
      -- setting effective_until to startDate, then detach its upcoming episodes.
      lift $ forM_ removed $ \template -> do
        activeValidities <- HT.statement () (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId)
        forM_ activeValidities $ \validity ->
          void $ HT.statement () (ShowSchedule.endValidity validity.stvId startDate)

        -- Detach upcoming episodes from this expired template, but only those airing
        -- on or after the change date so interim episodes keep their slot.
        void $ HT.statement () (Episodes.clearTemplateForUpcomingEpisodes template.stId startDate)

      created <- traverse (insertScheduleSlot showId startDate) added
      pure (map (.stId) removed, created, [])

-- | Write a new replay time onto a template that keeps its slot identity.
--
-- The primary window does not move, so no episode loses its airing. The old code
-- read this edit as a removal and an addition, which detached every upcoming
-- episode of the show for no reason.
--
-- An immediate change edits the row. A change with a future date needs two rows,
-- because the old replay time still runs until that date. The second row is a full
-- template, so the episodes on or after the date move onto it and keep their air
-- times. The order matters. @one_active_slot_per_show@ compares the open windows of
-- a show, so the old window closes before the new one opens.
retimeReplay ::
  Shows.Id ->
  ParsedScheduleSlot ->
  ShowSchedule.ScheduleTemplate Result ->
  -- | The date the change takes effect
  Day ->
  -- | Today in Pacific
  Day ->
  ExceptT Text HT.Transaction ([ShowSchedule.TemplateId], [ShowSchedule.TemplateId], [Episodes.Id])
retimeReplay showId slot template startDate today
  | template.stReplayStartTime == pssReplayStartTime slot =
      -- The whole slot matches. A start date move reaches here, and it changes no
      -- template.
      pure ([], [], [])
  | startDate <= today = do
      lift $ void $ HT.statement () (ShowSchedule.updateReplayStartTime template.stId (pssReplayStartTime slot))
      pure ([], [], [])
  | otherwise = do
      lift $ do
        activeValidities <- HT.statement () (ShowSchedule.getActiveValidityPeriodsForTemplate template.stId)
        forM_ activeValidities $ \validity ->
          void $ HT.statement () (ShowSchedule.endValidity validity.stvId startDate)
      newTemplateId <- insertScheduleSlot showId startDate slot
      migrated <- lift $ HT.statement () (Episodes.migrateUpcomingEpisodes template.stId newTemplateId startDate)
      pure ([template.stId], [newTemplateId], migrated)

-- | Create one schedule template and its open-ended validity period.
--
-- A template with no validity period never airs, so a missing validity row condemns
-- the transaction rather than leaving the show holding a slot it cannot broadcast.
insertScheduleSlot ::
  Shows.Id ->
  Day ->
  ParsedScheduleSlot ->
  ExceptT Text HT.Transaction ShowSchedule.TemplateId
insertScheduleSlot showId startDate slot = do
  let templateInsert =
        ShowSchedule.ScheduleTemplateInsert
          { ShowSchedule.stiShowId = showId,
            ShowSchedule.stiDayOfWeek = recurrenceDay (pssRecurrence slot),
            ShowSchedule.stiWeeksOfMonth = weekNumbers (pssRecurrence slot),
            ShowSchedule.stiStartTime = pssStart slot,
            ShowSchedule.stiEndTime = pssEnd slot,
            ShowSchedule.stiTimezone = "America/Los_Angeles",
            ShowSchedule.stiReplayStartTime = pssReplayStartTime slot
          }
  templateId <- lift $ HT.statement () (ShowSchedule.insertScheduleTemplate templateInsert)

  -- Open-ended validity: effective from startDate, no end date
  let validityInsert =
        ShowSchedule.ValidityInsert
          { ShowSchedule.viTemplateId = templateId,
            ShowSchedule.viEffectiveFrom = startDate,
            ShowSchedule.viEffectiveUntil = Nothing
          }
  lift (HT.statement () (ShowSchedule.insertValidity validityInsert)) >>= \case
    Just _ -> pure templateId
    Nothing -> do
      lift HT.condemn
      throwE "Could not save the schedule. Please try again."

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
buildTimeslotDescription :: Maybe ParsedScheduleSlot -> Maybe Text
buildTimeslotDescription =
  fmap $ \slot ->
    HostNotifications.formatTimeslotDescription (recurrenceDay (pssRecurrence slot)) (pssStart slot) (pssEnd slot)

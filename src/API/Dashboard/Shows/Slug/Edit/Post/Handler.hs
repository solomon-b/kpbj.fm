{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Slug.Edit.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Get.Templates.Form qualified as EditForm
import API.Dashboard.Shows.Slug.Edit.Post.Route (ScheduleSlotInfo (..), ShowEditForm (..))
import API.Links (apiLinks, dashboardShowsLinks, showsLinks, userLinks)
import API.Types
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Applicative ((<|>))
import Control.Monad (forM_, when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector qualified as Vector
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot)
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
rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

-- | Parse show status from text
parseStatus :: Text -> Maybe Shows.Status
parseStatus "active" = Just Shows.Active
parseStatus "inactive" = Just Shows.Inactive
parseStatus _ = Nothing

-- | Process logo/banner file uploads
processShowArtworkUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Slug ->
  -- | Logo file
  Maybe (FileData Mem) ->
  -- | Banner file
  Maybe (FileData Mem) ->
  m (Either Text (Maybe Text, Maybe Text)) -- (logoPath, bannerPath)
processShowArtworkUploads showSlug mLogoFile mBannerFile = do
  -- Process logo file (optional)
  logoResult <- case mLogoFile of
    Nothing ->
      pure $ Right Nothing
    Just logoFile -> do
      FileUpload.uploadShowLogo showSlug logoFile >>= \case
        Left err -> do
          Log.logInfo "Failed to upload logo file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  -- Process banner file (optional)
  bannerResult <- case mBannerFile of
    Nothing ->
      pure $ Right Nothing
    Just bannerFile -> do
      FileUpload.uploadShowBanner showSlug bannerFile >>= \case
        Left err -> do
          Log.logInfo "Failed to upload banner file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err -- File validation failed, reject entire operation
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

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
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  ShowEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized show edit attempt" slug
      let banner = BannerParams Error "Login Required" "You must be logged in to edit a show."
      pure (Servant.noHeader (redirectWithBanner [i|/#{userLoginGetUrl}|] banner))
    Just (_user, userMetadata)
      | isSuspended userMetadata -> do
          let banner = BannerParams Error "Account Suspended" "Your account has been suspended."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
    Just (user, userMetadata) -> do
      execQuerySpan (ShowHost.isUserHostOfShowSlug user.mId slug) >>= \case
        Left err -> do
          Log.logAttention "isUserHostOfShow execution error" (show err)
          let banner = BannerParams Error "Error" "An error occurred. Please try again."
          pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
        Right isHost -> do
          let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
          if not (isHost || isStaff)
            then do
              Log.logInfo "User attempted to edit show they don't host" slug
              let banner = BannerParams Error "Access Denied" "You don't have permission to edit this show."
              pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
            else do
              execQuerySpan (Shows.getShowBySlug slug) >>= \case
                Left err -> do
                  Log.logAttention "getShowBySlug execution error" (show err)
                  let banner = BannerParams Error "Error" "Failed to load show. Please try again."
                  pure (Servant.noHeader (redirectWithBanner [i|/#{rootGetUrl}|] banner))
                Right Nothing -> do
                  Log.logInfo_ $ "No show with slug: '" <> display slug <> "'"
                  let banner = BannerParams Warning "Not Found" "The show you're trying to update doesn't exist."
                  pure (Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner))
                Right (Just showModel) -> do
                  -- Fetch sidebar shows for dashboard rendering on errors
                  sidebarShowsResult <-
                    if UserMetadata.isAdmin userMetadata.mUserRole
                      then execQuerySpan Shows.getAllActiveShows
                      else execQuerySpan (Shows.getShowsForUser user.mId)
                  let sidebarShows = fromRight [] sidebarShowsResult
                      selectedShow = Just showModel
                  updateShow hxRequest user userMetadata showModel sidebarShows selectedShow editForm

updateShow ::
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
  Shows.Model ->
  [Shows.Model] ->
  Maybe Shows.Model ->
  ShowEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateShow hxRequest _user userMetadata showModel sidebarShows selectedShow editForm = do
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- Parse and validate form data
  case parseStatus (sefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in show edit form" (sefStatus editForm)
      let banner = BannerParams Error "Validation Error" "Invalid show status value."
      pure (Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner))
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
          pure (Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner))
        Right (mLogoPath, mBannerPath) -> do
          -- Use new uploaded files if provided, otherwise keep existing values
          let finalLogoUrl = mLogoPath <|> showModel.logoUrl
              finalBannerUrl = mBannerPath <|> showModel.bannerUrl

              updateData =
                Shows.Insert
                  { siTitle = sefTitle editForm,
                    siSlug = generatedSlug,
                    siDescription = sefDescription editForm,
                    siGenre = sefGenre editForm,
                    siLogoUrl = finalLogoUrl,
                    siBannerUrl = finalBannerUrl,
                    siStatus = finalStatus
                  }

          -- Update the show
          execQuerySpan (Shows.updateShow showModel.id updateData) >>= \case
            Left err -> do
              Log.logInfo "Failed to update show" (showModel.id, show err)
              let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
              pure (Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner))
            Right Nothing -> do
              Log.logInfo "Show update returned Nothing" showModel.id
              let banner = BannerParams Error "Update Failed" "Failed to update show. Please try again."
              pure (Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner))
            Right (Just _updatedId) -> do
              Log.logInfo "Successfully updated show" showModel.id
              -- Process schedule updates if staff
              if isStaff
                then case parseSchedules (sefSchedulesJson editForm) of
                  Left err -> do
                    Log.logInfo "Schedule validation failed" err
                    -- Re-render form with error banner, preserving user's submitted schedules
                    let updatedShowModel = showModel {Shows.title = sefTitle editForm, Shows.description = sefDescription editForm, Shows.genre = sefGenre editForm, Shows.slug = generatedSlug, Shows.status = finalStatus, Shows.logoUrl = finalLogoUrl, Shows.bannerUrl = finalBannerUrl}
                        -- Use the submitted JSON so user sees what they tried to submit
                        submittedSchedulesJson = fromMaybe "[]" (sefSchedulesJson editForm)
                        submittedHostIds = Set.fromList (sefHosts editForm)
                        banner = BannerParams Error "Schedule Error" err
                    -- Refetch eligible hosts for the form
                    eligibleHosts <- fetchEligibleHosts
                    let editTemplate = EditForm.template updatedShowModel userMetadata True submittedSchedulesJson eligibleHosts submittedHostIds
                    html <- renderDashboardTemplate hxRequest userMetadata sidebarShows selectedShow NavShows Nothing Nothing (renderBannerHtml banner <> editTemplate)
                    pure $ Servant.noHeader html
                  Right schedules -> do
                    -- Check for conflicts with other shows
                    conflictCheck <- checkScheduleConflicts showModel.id schedules
                    case conflictCheck of
                      Left conflictErr -> do
                        Log.logInfo "Schedule conflict with other show" conflictErr
                        -- Re-render form with error banner, preserving user's submitted schedules
                        let updatedShowModel = showModel {Shows.title = sefTitle editForm, Shows.description = sefDescription editForm, Shows.genre = sefGenre editForm, Shows.slug = generatedSlug, Shows.status = finalStatus, Shows.logoUrl = finalLogoUrl, Shows.bannerUrl = finalBannerUrl}
                            -- Use the submitted JSON so user sees what they tried to submit
                            submittedSchedulesJson = fromMaybe "[]" (sefSchedulesJson editForm)
                            submittedHostIds = Set.fromList (sefHosts editForm)
                            banner = BannerParams Error "Schedule Conflict" conflictErr
                        -- Refetch eligible hosts for the form
                        eligibleHosts <- fetchEligibleHosts
                        let editTemplate = EditForm.template updatedShowModel userMetadata True submittedSchedulesJson eligibleHosts submittedHostIds
                        html <- renderDashboardTemplate hxRequest userMetadata sidebarShows selectedShow NavShows Nothing Nothing (renderBannerHtml banner <> editTemplate)
                        pure $ Servant.noHeader html
                      Right () -> do
                        -- Update schedules
                        updateSchedulesForShow showModel.id schedules
                        -- Update hosts
                        updateHostsForShow showModel.id (sefHosts editForm)
                        redirectToShowPage generatedSlug
                else redirectToShowPage generatedSlug
  where
    -- Render banner as HTML
    renderBannerHtml :: BannerParams -> Lucid.Html ()
    renderBannerHtml BannerParams {..} =
      let bgColor = case bpType of
            Success -> "bg-green-100 border-green-600 text-green-800"
            Error -> "bg-red-100 border-red-600 text-red-800"
            Warning -> "bg-yellow-100 border-yellow-600 text-yellow-800"
            Info -> "bg-blue-100 border-blue-600 text-blue-800"
       in Lucid.div_ [Lucid.class_ ("border-2 p-4 mb-4 " <> bgColor)] $ do
            Lucid.strong_ $ Lucid.toHtml bpTitle
            Lucid.span_ [Lucid.class_ "ml-2"] $ Lucid.toHtml bpMessage

    -- Redirect to the show page with a success banner
    redirectToShowPage :: (MonadIO m) => Slug -> m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    redirectToShowPage newSlug = do
      let showUrl = [i|/#{showGetUrl newSlug}|] :: Text
          bannerParams =
            BannerParams
              { bpType = Success,
                bpTitle = "Show Updated",
                bpMessage = "Your show has been updated successfully."
              }
          -- Build the full URL with banner params for the HX-Redirect header
          redirectUrl = buildRedirectUrl showUrl bannerParams
      pure $ Servant.addHeader redirectUrl (redirectWithBanner showUrl bannerParams)

    -- Build URL with banner query params
    buildRedirectUrl :: Text -> BannerParams -> Text
    buildRedirectUrl baseUrl BannerParams {..} =
      let bannerTypeParam :: Text
          bannerTypeParam = case bpType of
            Success -> "success"
            Error -> "error"
            Warning -> "warning"
            Info -> "info"
       in [i|#{baseUrl}?_banner=#{bannerTypeParam}&_title=#{bpTitle}&_msg=#{bpMessage}|]

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
          let weeks = Vector.fromList (weeksOfMonth slot)
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
      execQuerySpan (ShowSchedule.getActiveValidityPeriodsForTemplate template.id) >>= \case
        Left err -> do
          Log.logInfo "Failed to fetch validity periods" (Text.pack $ show err)
          pure []
        Right validities -> pure validities

    -- End each active validity period by setting effective_until to today
    forM_ activeValidities $ \validity -> do
      _ <- execQuerySpan (ShowSchedule.endValidity validity.id today)
      Log.logInfo "Closed out schedule validity" (show template.id, show validity.id)

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
                  ShowSchedule.stiWeeksOfMonth = Just (Vector.fromList (weeksOfMonth slot)),
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

-- | Fetch all eligible hosts for the form (all users)
fetchEligibleHosts ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    MonadUnliftIO m,
    Has Tracer env
  ) =>
  m [UserMetadata.UserWithMetadata]
fetchEligibleHosts =
  execQuerySpan (UserMetadata.getAllUsersWithPagination 1000 0) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch eligible hosts" (show err)
      pure []
    Right hosts -> pure hosts

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

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, showGetLink)
import API.Shows.Slug.Edit.Get.Templates.Form qualified as EditForm
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Applicative ((<|>))
import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
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
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot)
import Effects.FileUpload qualified as FileUpload
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:slug/edit"
    ( "shows"
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem ShowEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Form data for show editing
data ShowEditForm = ShowEditForm
  { sefTitle :: Text,
    sefDescription :: Text,
    sefGenre :: Maybe Text,
    sefLogoFile :: Maybe (FileData Mem),
    sefBannerFile :: Maybe (FileData Mem),
    sefStatus :: Text,
    sefSchedulesJson :: Maybe Text
  }
  deriving (Show)

-- | Schedule slot info parsed from JSON form data
data ScheduleSlotInfo = ScheduleSlotInfo
  { dayOfWeek :: Text,
    weeksOfMonth :: [Int64],
    startTime :: Text,
    endTime :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance FromMultipart Mem ShowEditForm where
  fromMultipart multipartData =
    ShowEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "genre" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "logo_file" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "banner_file" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "schedules_json" multipartData))
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing (Just t) | Text.null (Text.strip t) = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

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

-- | Error templates
unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit shows."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Show Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The show you're trying to update doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

forbiddenTemplate :: Lucid.Html ()
forbiddenTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit shows you host or have staff permissions."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Update Failed"
    Lucid.p_ [Lucid.class_ "mb-6"] $ Lucid.toHtml errorMsg
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

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
      Servant.noHeader <$> renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the show to verify it exists and check authorization
      execQuerySpan (Shows.getShowBySlug slug) >>= \case
        Left err -> do
          Log.logAttention "getShowBySlug execution error" (show err)
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No show with slug: '" <> display slug <> "'"
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just showModel) -> do
          -- Check authorization - user must be a host of the show or staff+
          execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id) >>= \case
            Left err -> do
              Log.logAttention "isUserHostOfShow execution error" (show err)
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) forbiddenTemplate
            Right True -> updateShow hxRequest user userMetadata showModel editForm
            Right False ->
              if UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then updateShow hxRequest user userMetadata showModel editForm
                else do
                  Log.logInfo "User attempted to edit show they don't host" showModel.id
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

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
  ShowEditForm ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
updateShow hxRequest _user userMetadata showModel editForm = do
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- Parse and validate form data
  case parseStatus (sefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in show edit form" (sefStatus editForm)
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid show status value.")
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
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "File upload error: " <> uploadErr)
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
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
            Right Nothing -> do
              Log.logInfo "Show update returned Nothing" showModel.id
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update show. Please try again.")
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
                        banner = renderBanner Error "Schedule Error" err
                        editTemplate = EditForm.template updatedShowModel userMetadata True submittedSchedulesJson
                    Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (banner <> editTemplate)
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
                            banner = renderBanner Error "Schedule Conflict" conflictErr
                            editTemplate = EditForm.template updatedShowModel userMetadata True submittedSchedulesJson
                        Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (banner <> editTemplate)
                      Right () -> do
                        updateSchedulesForShow showModel.id schedules
                        redirectToShowPage generatedSlug
                else redirectToShowPage generatedSlug
  where
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
checkScheduleConflicts showId slots = go slots
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

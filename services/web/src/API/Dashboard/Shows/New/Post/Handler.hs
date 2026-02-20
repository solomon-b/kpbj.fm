{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Post.Route (NewShowForm (..), ScheduleSlotInfo (..))
import API.Links (dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Time.LocalTime (LocalTime (..), hoursToTimeZone, utcToLocalTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
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
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)

--------------------------------------------------------------------------------

-- URL helpers
dashboardShowDetailUrl :: Shows.Id -> Slug.Slug -> Links.URI
dashboardShowDetailUrl showId slug = Links.linkURI $ dashboardShowsLinks.detail showId slug Nothing

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  NewShowForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler cookie form =
  handleRedirectErrors "Show creation" dashboardShowsLinks.newGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only Admin users can create shows." userMetadata
    createdShow <- action form
    let showId = createdShow.id
        showSlug = createdShow.slug
        showTitle = createdShow.title
        detailUrl = [i|/#{dashboardShowDetailUrl showId showSlug}|] :: Text
        banner = BannerParams Success "Show Created" [i|"#{showTitle}" has been created successfully.|]
        redirectUrl = buildRedirectUrl detailUrl banner
    pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)

--------------------------------------------------------------------------------

-- | Business logic: validate form, create show with all side effects.
--
-- Returns the created 'Shows.Model' so the handler can build the redirect.
action ::
  NewShowForm ->
  ExceptT HandlerError AppM Shows.Model
action form = do
  -- 1. Validate form data
  showData <- case validateNewShow form of
    Left validationError -> do
      Log.logInfo "Show creation failed validation" (Aeson.object ["error" .= validationError])
      throwValidationError validationError
    Right sd -> pure sd

  -- 2. Parse and validate schedules
  schedules <- case parseSchedules (nsfSchedulesJson form) of
    Left err -> do
      Log.logInfo "Failed to parse schedules" (Aeson.object ["error" .= err])
      throwValidationError ("Invalid schedule data: " <> err)
    Right s -> pure s

  -- 3. Check schedule conflicts (Shows.Id 0 means check against ALL active shows)
  conflictResult <- lift $ checkScheduleConflicts (Shows.Id 0) schedules
  case conflictResult of
    Left conflictErr -> do
      Log.logInfo "Schedule conflict detected" (Aeson.object ["error" .= conflictErr])
      throwValidationError conflictErr
    Right () -> pure ()

  -- 4. Process file uploads
  uploadResult <- lift $ processShowArtworkUploads showData.siSlug (nsfLogoFile form)
  mLogoPath <- case uploadResult of
    Left uploadErr -> do
      Log.logInfo "Failed to upload show artwork" uploadErr
      throwValidationError ("File upload error: " <> uploadErr)
    Right path -> pure path

  -- 5. Check slug uniqueness
  existingShow <- execQuery (Shows.getShowBySlug showData.siSlug)
  case existingShow of
    Left dbErr -> throwDatabaseError dbErr
    Right (Just _) -> throwValidationError "A show with this URL already exists. Try a different title."
    Right Nothing -> pure ()

  -- 6. Insert show
  let finalShowData = showData {Shows.siLogoUrl = mLogoPath}
  insertResult <- execQuery (Shows.insertShow finalShowData)
  showId <- case insertResult of
    Left dbError -> do
      Log.logInfo "Database error creating show" (Aeson.object ["error" .= Text.pack (show dbError)])
      throwDatabaseError dbError
    Right Nothing -> do
      Log.logInfo_ "Show insert returned Nothing"
      throwHandlerFailure "Failed to create show."
    Right (Just sid) -> pure sid

  -- 7. Post-creation side effects (fire and forget)
  lift $ do
    assignHostsToShow showId (nsfHosts form)
    processShowTags showId (nsfTags form)
    createSchedulesForShow showId schedules

  -- 8. Fetch created show
  fetchResult <- execQuery (Shows.getShowById showId)
  createdShow <- case fetchResult of
    Right (Just s) -> pure s
    _ -> do
      Log.logInfo_ "Created show but failed to retrieve it"
      throwHandlerFailure "Show was created but there was an error loading it."

  -- 9. Send host notification emails
  let mTimeslot = buildTimeslotDescription schedules
  lift $ HostNotifications.sendHostAssignmentNotifications createdShow mTimeslot (nsfHosts form)

  Log.logInfo "Successfully created show" (Aeson.object ["title" .= createdShow.title, "id" .= show showId])
  pure createdShow

-- | Validate and convert form data to show insert data (without file paths yet)
validateNewShow :: NewShowForm -> Either Text Shows.Insert
validateNewShow form = do
  let slug = Slug.mkSlug (nsfTitle form)

      -- Sanitize user input
      sanitizedTitle = Sanitize.sanitizeTitle (nsfTitle form)
      sanitizedDescription = Sanitize.sanitizeUserContent (nsfDescription form)

      -- Treat empty description as Nothing
      mDescription =
        if Text.null (Text.strip sanitizedDescription)
          then Nothing
          else Just sanitizedDescription

      status = case nsfStatus form of
        "active" -> Shows.Active
        "inactive" -> Shows.Inactive
        _ -> Shows.Active

  -- Basic validation
  if Text.null (Text.strip sanitizedTitle)
    then Left "Title is required"
    else
      Right $
        Shows.Insert
          { Shows.siTitle = sanitizedTitle,
            Shows.siSlug = slug,
            Shows.siDescription = mDescription,
            Shows.siLogoUrl = Nothing, -- Will be set after file upload
            Shows.siStatus = status
          }

-- | Process logo file upload
processShowArtworkUploads ::
  Slug.Slug ->
  Maybe (FileData Mem) ->
  AppM (Either Text (Maybe Text))
processShowArtworkUploads showSlug mLogoFile = do
  -- TODO: Why is the aws env separate from the storage backend?
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

-- | Assign hosts to a show and auto-promote regular users to Host role
assignHostsToShow ::
  Shows.Id ->
  [User.Id] ->
  AppM ()
assignHostsToShow showId hostIds = do
  forM_ hostIds $ \userId -> do
    -- Check if user needs to be promoted to Host role
    promoteUserToHostIfNeeded userId

    let hostInsert =
          ShowHost.Insert
            { ShowHost.shiId = showId,
              ShowHost.shiUserId = userId,
              ShowHost.shiRole = ShowHost.Host
            }
    result <- execQuery (ShowHost.insertShowHost hostInsert)
    case result of
      Left dbError ->
        Log.logInfo "Failed to assign host to show" (Aeson.object ["userId" .= show userId, "error" .= Text.pack (show dbError)])
      Right () ->
        Log.logInfo "Assigned host to show" (Aeson.object ["userId" .= show userId])

-- | Promote a regular User to Host role if they are not already Host/Staff/Admin
promoteUserToHostIfNeeded ::
  User.Id ->
  AppM ()
promoteUserToHostIfNeeded userId = do
  execQuery (UserMetadata.getUserMetadata userId) >>= \case
    Left dbError ->
      Log.logInfo "Failed to fetch user metadata for promotion check" (Aeson.object ["userId" .= show userId, "error" .= Text.pack (show dbError)])
    Right Nothing ->
      Log.logInfo "User metadata not found for promotion check" (Aeson.object ["userId" .= show userId])
    Right (Just metadata) ->
      case metadata.mUserRole of
        UserMetadata.User -> do
          -- User is a regular user, promote them to Host
          result <- execQuery (UserMetadata.updateUserRole userId UserMetadata.Host)
          case result of
            Left dbError ->
              Log.logInfo "Failed to promote user to Host" (Aeson.object ["userId" .= show userId, "error" .= Text.pack (show dbError)])
            Right Nothing ->
              Log.logInfo "User role update returned no result" (Aeson.object ["userId" .= show userId])
            Right (Just _) ->
              Log.logInfo "Promoted user to Host role" (Aeson.object ["userId" .= show userId])
        _ ->
          -- User already has Host, Staff, or Admin role - no promotion needed
          pure ()

-- | Process comma-separated tags and associate them with a show
processShowTags ::
  Shows.Id ->
  Maybe Text ->
  AppM ()
processShowTags _ Nothing = pure ()
processShowTags showId (Just tagsText) = do
  let tagNames = filter (not . Text.null) $ map Text.strip $ Text.splitOn "," tagsText
  forM_ tagNames $ \tagName -> do
    -- Get or create the tag
    execQuery (ShowTags.getShowTagByName tagName) >>= \case
      Right (Just existingTag) -> do
        -- Tag exists, associate it with the show
        void $ execQuery (Shows.addTagToShow showId (ShowTags.stId existingTag))
        Log.logInfo "Associated existing tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
      _ -> do
        -- Tag doesn't exist, create it and associate
        execQuery (ShowTags.insertShowTag (ShowTags.Insert tagName)) >>= \case
          Right (Just newTagId) -> do
            void $ execQuery (Shows.addTagToShow showId newTagId)
            Log.logInfo "Created and associated new tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
          Right Nothing ->
            Log.logInfo "Tag insert returned Nothing" (Aeson.object ["tag" .= tagName])
          Left dbError ->
            Log.logInfo "Failed to create tag" (Aeson.object ["tag" .= tagName, "error" .= Text.pack (show dbError)])

--------------------------------------------------------------------------------
-- Schedule Creation Helpers

-- | Parse schedules JSON from form data
parseSchedules :: Maybe Text -> Either Text [ScheduleSlotInfo]
parseSchedules Nothing = Right []
parseSchedules (Just schedulesJson)
  | Text.null (Text.strip schedulesJson) = Right []
  | schedulesJson == "[]" = Right []
  | otherwise = case Aeson.eitherDecodeStrict (Text.encodeUtf8 schedulesJson) of
      Left err -> Left $ "Invalid schedules JSON: " <> Text.pack err
      Right slots -> Right slots

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
--
-- For new show creation, pass Shows.Id 0 to check against ALL active shows.
checkScheduleConflicts ::
  Shows.Id ->
  [ScheduleSlotInfo] ->
  AppM (Either Text ())
checkScheduleConflicts showId = go
  where
    go [] = pure (Right ())
    go (slot : rest) =
      case (parseDayOfWeek (dayOfWeek slot), parseTimeOfDay (startTime slot), parseTimeOfDay (endTime slot)) of
        (Just dow, Just start, Just end) -> do
          let weeks = map fromIntegral (weeksOfMonth slot)
          execQuery (ShowSchedule.checkTimeSlotConflict showId dow weeks start end) >>= \case
            Left err -> do
              Log.logInfo "Failed to check schedule conflict" (Text.pack $ show err)
              pure (Right ()) -- Don't block on DB errors, let it through
            Right (Just conflictingShow) ->
              pure (Left $ "Schedule conflict: " <> dayOfWeek slot <> " " <> startTime slot <> "-" <> endTime slot <> " overlaps with \"" <> conflictingShow <> "\"")
            Right Nothing -> go rest
        _ -> go rest -- Skip invalid slots, they'll fail later anyway

-- | Create schedules for a newly created show
createSchedulesForShow ::
  Shows.Id ->
  [ScheduleSlotInfo] ->
  AppM ()
createSchedulesForShow showId slots = do
  -- Use Pacific time for "today" to match the schedule display
  nowUtc <- liftIO getCurrentTime
  let pacificTz = hoursToTimeZone (-8) -- PST is UTC-8
      nowPacific = utcToLocalTime pacificTz nowUtc
      today = localDay nowPacific

  forM_ slots $ \slot -> do
    case ( parseDayOfWeek (dayOfWeek slot),
           parseTimeOfDay (startTime slot),
           parseTimeOfDay (endTime slot)
         ) of
      (Just dow, Just start, Just end) -> do
        -- Create schedule template
        let templateInsert =
              ShowSchedule.ScheduleTemplateInsert
                { ShowSchedule.stiShowId = showId,
                  ShowSchedule.stiDayOfWeek = Just dow,
                  ShowSchedule.stiWeeksOfMonth = Just (map fromIntegral (weeksOfMonth slot)),
                  ShowSchedule.stiStartTime = start,
                  ShowSchedule.stiEndTime = end,
                  ShowSchedule.stiTimezone = "America/Los_Angeles",
                  ShowSchedule.stiAirsTwiceDaily = True
                }

        templateResult <- execQuery (ShowSchedule.insertScheduleTemplate templateInsert)
        case templateResult of
          Left err ->
            Log.logInfo "Failed to insert schedule template" (Aeson.object ["error" .= Text.pack (show err)])
          Right templateId -> do
            -- Create validity record (effective immediately, no end date)
            let validityInsert =
                  ShowSchedule.ValidityInsert
                    { ShowSchedule.viTemplateId = templateId,
                      ShowSchedule.viEffectiveFrom = today,
                      ShowSchedule.viEffectiveUntil = Nothing
                    }
            validityResult <- execQuery (ShowSchedule.insertValidity validityInsert)
            case validityResult of
              Left err ->
                Log.logInfo "Failed to insert validity" (Aeson.object ["error" .= Text.pack (show err)])
              Right (Just _) ->
                Log.logInfo "Created schedule for show" (Aeson.object ["showId" .= show showId, "day" .= show dow])
              Right Nothing ->
                Log.logInfo "insertValidity returned Nothing" (Aeson.object ["showId" .= show showId, "day" .= show dow])
      _ ->
        Log.logInfo "Invalid schedule slot data - skipping" (Aeson.object ["slot" .= Aeson.toJSON slot])

--------------------------------------------------------------------------------
-- Helper Functions

-- | Build a human-readable timeslot description from schedule slots.
--
-- Returns Nothing if no valid schedules, otherwise returns a formatted string
-- like "Fridays 8:00 PM - 10:00 PM PT"
buildTimeslotDescription :: [ScheduleSlotInfo] -> Maybe Text
buildTimeslotDescription [] = Nothing
buildTimeslotDescription (slot : _) =
  -- Just use the first schedule slot for the email
  case ( parseDayOfWeek (dayOfWeek slot),
         parseTimeOfDay (startTime slot),
         parseTimeOfDay (endTime slot)
       ) of
    (Just dow, Just start, Just end) ->
      Just $ HostNotifications.formatTimeslotDescription dow start end
    _ -> Nothing

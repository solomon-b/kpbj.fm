{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Post.Route (NewShowForm (..), ScheduleSlotInfo (..))
import API.Links (apiLinks, dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleRedirectErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)

--------------------------------------------------------------------------------

-- URL helpers
dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing

dashboardShowsNewGetUrl :: Links.URI
dashboardShowsNewGetUrl = Links.linkURI dashboardShowsLinks.newGet

dashboardShowDetailUrl :: Shows.Id -> Slug.Slug -> Links.URI
dashboardShowDetailUrl showId slug = Links.linkURI $ dashboardShowsLinks.detail showId slug Nothing

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  NewShowForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer cookie form =
  handleRedirectErrors "Show creation" apiLinks.rootGet $ do
    -- 1. Require authentication and admin role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only Admin users can create shows." userMetadata

    -- 2. Validate form data
    case validateNewShow form of
      Left validationError -> do
        Log.logInfo "Show creation failed validation" (Aeson.object ["error" .= validationError])
        let banner = BannerParams Error "Validation Error" validationError
        pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsNewGetUrl}|] banner)
      Right showData ->
        handleShowCreation showData form

-- | Validate and convert form data to show insert data (without file paths yet)
validateNewShow :: NewShowForm -> Either Text Shows.Insert
validateNewShow form = do
  let slug = Slug.mkSlug (nsfTitle form)

      -- Sanitize user input
      sanitizedTitle = Sanitize.sanitizeTitle (nsfTitle form)
      sanitizedDescription = Sanitize.sanitizeUserContent (nsfDescription form)

      status = case nsfStatus form of
        "active" -> Shows.Active
        "inactive" -> Shows.Inactive
        _ -> Shows.Active

  -- Basic validation
  if Text.null (Text.strip sanitizedTitle)
    then Left "Title is required"
    else
      if Text.null (Text.strip sanitizedDescription)
        then Left "Description is required"
        else
          Right $
            Shows.Insert
              { Shows.siTitle = sanitizedTitle,
                Shows.siSlug = slug,
                Shows.siDescription = sanitizedDescription,
                Shows.siLogoUrl = Nothing, -- Will be set after file upload
                Shows.siBannerUrl = Nothing, -- Will be set after file upload
                Shows.siStatus = status
              }

-- | Process logo/banner file uploads
processShowArtworkUploads ::
  Slug.Slug ->
  Maybe (FileData Mem) ->
  Maybe (FileData Mem) ->
  AppM (Either Text (Maybe Text, Maybe Text))
processShowArtworkUploads showSlug mLogoFile mBannerFile = do
  -- TODO: Why is the aws env separate from the storage backend?
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
          pure $ Left $ Text.pack $ show err
        Right Nothing -> pure $ Right Nothing -- No file selected
        Right (Just uploadResult) ->
          pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

  case (logoResult, bannerResult) of
    (Left logoErr, _) -> pure $ Left logoErr
    (Right _logoPath, Left bannerErr) -> pure $ Left bannerErr
    (Right mLogoPath, Right mBannerPath) -> pure $ Right (mLogoPath, mBannerPath)

-- | Handle show creation after validation passes
handleShowCreation ::
  Shows.Insert ->
  NewShowForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handleShowCreation showData form = do
  -- Parse schedules first to check for conflicts before creating the show
  case parseSchedules (nsfSchedulesJson form) of
    Left err -> do
      Log.logInfo "Failed to parse schedules" (Aeson.object ["error" .= err])
      let banner = BannerParams Error "Invalid Schedule" ("Invalid schedule data: " <> err)
      pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsNewGetUrl}|] banner)
    Right schedules -> do
      -- Check for schedule conflicts before creating the show
      -- Use Shows.Id 0 as placeholder since the show doesn't exist yet
      -- This means we check against ALL active shows (no exclusion)
      conflictResult <- checkScheduleConflicts (Shows.Id 0) schedules
      case conflictResult of
        Left conflictErr -> do
          Log.logInfo "Schedule conflict detected" (Aeson.object ["error" .= conflictErr])
          let banner = BannerParams Error "Schedule Conflict" conflictErr
          pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsNewGetUrl}|] banner)
        Right () -> do
          -- No conflicts, proceed with file uploads
          uploadResults <- processShowArtworkUploads showData.siSlug (nsfLogoFile form) (nsfBannerFile form)

          case uploadResults of
            Left uploadErr -> do
              Log.logInfo "Failed to upload show artwork" uploadErr
              let banner = BannerParams Error "Upload Error" ("File upload error: " <> uploadErr)
              pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsNewGetUrl}|] banner)
            Right (mLogoPath, mBannerPath) -> do
              -- Update show data with file paths
              let finalShowData =
                    showData
                      { Shows.siLogoUrl = mLogoPath,
                        Shows.siBannerUrl = mBannerPath
                      }

              execQuerySpan (Shows.insertShow finalShowData) >>= \case
                Left dbError -> do
                  Log.logInfo "Database error creating show" (Aeson.object ["error" .= Text.pack (show dbError)])
                  let banner = BannerParams Error "Database Error" "Database error occurred. Please try again."
                  pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsNewGetUrl}|] banner)
                Right showId -> do
                  -- Assign hosts to the show
                  assignHostsToShow showId (nsfHosts form)

                  -- Process tags
                  processShowTags showId (nsfTags form)

                  -- Create schedules (already validated for conflicts)
                  createSchedulesForShow showId schedules

                  -- Fetch the created show
                  execQuerySpan (Shows.getShowById showId) >>= \case
                    Right (Just createdShow) -> do
                      Log.logInfo "Successfully created show" (Aeson.object ["title" .= Shows.siTitle finalShowData, "id" .= show showId])
                      let showSlug = createdShow.slug
                          showTitle = createdShow.title
                          detailLink = dashboardShowDetailUrl showId showSlug
                          detailUrl = [i|/#{detailLink}|] :: Text
                          banner = BannerParams Success "Show Created" [i|"#{showTitle}" has been created successfully.|]
                          redirectUrl = buildRedirectUrl detailUrl banner
                      pure $ Servant.addHeader redirectUrl (redirectWithBanner detailUrl banner)
                    _ -> do
                      Log.logInfo_ "Created show but failed to retrieve it"
                      let banner = BannerParams Error "Error" "Show was created but there was an error displaying the confirmation."
                      pure $ Servant.noHeader (redirectWithBanner [i|/#{dashboardShowsGetUrl}|] banner)

-- | Assign hosts to a show and auto-promote regular users to Host role
assignHostsToShow ::
  Shows.Id ->
  [User.Id] ->
  AppM ()
assignHostsToShow showId hostIds = do
  -- Make the first host the primary host
  forM_ (zip hostIds [0 :: Int ..]) $ \(userId, idx) -> do
    -- Check if user needs to be promoted to Host role
    promoteUserToHostIfNeeded userId

    let isPrimary = idx == 0
        hostInsert =
          ShowHost.Insert
            { ShowHost.shiId = showId,
              ShowHost.shiUserId = userId,
              ShowHost.shiRole = ShowHost.Host,
              ShowHost.shiIsPrimary = isPrimary
            }
    result <- execQuerySpan (ShowHost.insertShowHost hostInsert)
    case result of
      Left dbError ->
        Log.logInfo "Failed to assign host to show" (Aeson.object ["userId" .= show userId, "error" .= Text.pack (show dbError)])
      Right () ->
        Log.logInfo "Assigned host to show" (Aeson.object ["userId" .= show userId, "isPrimary" .= isPrimary])

-- | Promote a regular User to Host role if they are not already Host/Staff/Admin
promoteUserToHostIfNeeded ::
  User.Id ->
  AppM ()
promoteUserToHostIfNeeded userId = do
  execQuerySpan (UserMetadata.getUserMetadata userId) >>= \case
    Left dbError ->
      Log.logInfo "Failed to fetch user metadata for promotion check" (Aeson.object ["userId" .= show userId, "error" .= Text.pack (show dbError)])
    Right Nothing ->
      Log.logInfo "User metadata not found for promotion check" (Aeson.object ["userId" .= show userId])
    Right (Just metadata) ->
      case metadata.mUserRole of
        UserMetadata.User -> do
          -- User is a regular user, promote them to Host
          result <- execQuerySpan (UserMetadata.updateUserRole userId UserMetadata.Host)
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
    execQuerySpan (ShowTags.getShowTagByName tagName) >>= \case
      Right (Just existingTag) -> do
        -- Tag exists, associate it with the show
        void $ execQuerySpan (Shows.addTagToShow showId (ShowTags.stId existingTag))
        Log.logInfo "Associated existing tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
      _ -> do
        -- Tag doesn't exist, create it and associate
        execQuerySpan (ShowTags.insertShowTag (ShowTags.Insert tagName)) >>= \case
          Right newTagId -> do
            void $ execQuerySpan (Shows.addTagToShow showId newTagId)
            Log.logInfo "Created and associated new tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
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
          execQuerySpan (ShowSchedule.checkTimeSlotConflict showId dow weeks start end) >>= \case
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
  today <- liftIO $ utctDay <$> getCurrentTime

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
                  ShowSchedule.stiTimezone = "America/Los_Angeles"
                }

        templateResult <- execQuerySpan (ShowSchedule.insertScheduleTemplate templateInsert)
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
            validityResult <- execQuerySpan (ShowSchedule.insertValidity validityInsert)
            case validityResult of
              Left err ->
                Log.logInfo "Failed to insert validity" (Aeson.object ["error" .= Text.pack (show err)])
              Right _ ->
                Log.logInfo "Created schedule for show" (Aeson.object ["showId" .= show showId, "day" .= show dow])
      _ ->
        Log.logInfo "Invalid schedule slot data - skipping" (Aeson.object ["slot" .= Aeson.toJSON slot])

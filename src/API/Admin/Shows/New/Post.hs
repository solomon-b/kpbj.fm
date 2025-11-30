{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Shows.New.Post (Route, handler) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminShowsGetLink, adminShowsNewGetLink, rootGetLink, showGetLink, userLoginGetLink)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (forM_)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Text.Read qualified as Text.Read
import Data.Time (DayOfWeek (..), TimeOfDay, getCurrentTime, utctDay)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Data.Vector qualified as Vector
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
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
import Servant.Multipart (FileData, FromMultipart, Input (..), Mem, MultipartData (..), MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
adminShowsGetUrl :: Links.URI
adminShowsGetUrl = Links.linkURI $ adminShowsGetLink Nothing Nothing Nothing

adminShowsNewGetUrl :: Links.URI
adminShowsNewGetUrl = Links.linkURI adminShowsNewGetLink

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI rootGetLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /admin/shows/new"
    ( "admin"
        :> "shows"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem NewShowForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------

-- | Form data for creating a new show
data NewShowForm = NewShowForm
  { nsfTitle :: Text,
    nsfDescription :: Text,
    nsfGenre :: Maybe Text,
    nsfLogoFile :: Maybe (FileData Mem),
    nsfBannerFile :: Maybe (FileData Mem),
    nsfStatus :: Text,
    nsfHosts :: [User.Id],
    nsfSchedulesJson :: Maybe Text
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

instance FromMultipart Mem NewShowForm where
  fromMultipart multipartData =
    NewShowForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "genre" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "logo_file" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "banner_file" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (parseHosts $ fromRight [] (lookupInputs "hosts" multipartData))
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "schedules_json" multipartData))
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing (Just t) | Text.null (Text.strip t) = Nothing
      emptyToNothing x = x

      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

      parseHosts :: [Text] -> [User.Id]
      parseHosts = mapMaybe parseUserId

      parseUserId :: Text -> Maybe User.Id
      parseUserId t = case Text.Read.decimal t of
        Right (n, "") -> Just (User.Id n)
        _ -> Nothing

      -- Helper to lookup all values for a given input name (for multi-select)
      lookupInputs :: Text -> MultipartData Mem -> Either String [Text]
      lookupInputs name multipart =
        Right [iValue input | input <- inputs multipart, iName input == name]

--------------------------------------------------------------------------------

-- | Error template
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Error Creating Show"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{adminShowsNewGetUrl}|],
          hxGet_ [i|/#{adminShowsNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
        ]
        "TRY AGAIN"
      Lucid.a_
        [ Lucid.href_ [i|/#{adminShowsGetUrl}|],
          hxGet_ [i|/#{adminShowsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "BACK TO SHOWS"

-- | Login required template
loginRequiredTemplate :: Lucid.Html ()
loginRequiredTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-4"] "You must be logged in to create shows."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "LOGIN"

-- | Permission denied template
permissionDeniedTemplate :: Lucid.Html ()
permissionDeniedTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ [Lucid.class_ "mb-4"] "Only Admin users can create shows."
    Lucid.a_
      [ Lucid.href_ [i|/#{rootGetUrl}|],
        hxGet_ [i|/#{rootGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "BACK TO HOME"

-- | Success template showing the created show
successTemplate :: Shows.Model -> Lucid.Html ()
successTemplate theShow = do
  let showTitle = theShow.title
      showDescription = theShow.description
      showDetailUrl = Links.linkURI $ showGetLink theShow.slug
  renderBanner Success "Show Created" [i|"#{showTitle}" has been created successfully.|]

  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] $ Lucid.toHtml showTitle
    Lucid.p_ [Lucid.class_ "text-gray-600 mb-4"] $ Lucid.toHtml showDescription

    Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showDetailUrl}|],
          hxGet_ [i|/#{showDetailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "VIEW SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{adminShowsGetUrl}|],
          hxGet_ [i|/#{adminShowsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-300 text-gray-800 px-6 py-3 font-bold hover:bg-gray-400"
        ]
        "BACK TO SHOWS"

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
  Maybe Cookie ->
  Maybe HxRequest ->
  NewShowForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing ->
      Servant.noHeader <$> renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (_user, userMetadata) ->
      if not (UserMetadata.isAdmin userMetadata.mUserRole) || isSuspended userMetadata
        then Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) permissionDeniedTemplate
        else case validateNewShow form of
          Left validationError -> do
            Log.logInfo "Show creation failed validation" (Aeson.object ["error" .= validationError])
            Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate validationError)
          Right showData ->
            handleShowCreation hxRequest userMetadata showData form

-- | Validate and convert form data to show insert data (without file paths yet)
validateNewShow :: NewShowForm -> Either Text Shows.Insert
validateNewShow form = do
  let slug = Slug.mkSlug (nsfTitle form)

      -- Sanitize user input
      sanitizedTitle = Sanitize.sanitizeTitle (nsfTitle form)
      sanitizedDescription = Sanitize.sanitizeUserContent (nsfDescription form)
      sanitizedGenre = Sanitize.sanitizeTitle <$> nsfGenre form

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
                Shows.siGenre = sanitizedGenre,
                Shows.siLogoUrl = Nothing, -- Will be set after file upload
                Shows.siBannerUrl = Nothing, -- Will be set after file upload
                Shows.siStatus = status
              }

-- | Process logo/banner file uploads
processShowArtworkUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Slug.Slug ->
  Maybe (FileData Mem) ->
  Maybe (FileData Mem) ->
  m (Either Text (Maybe Text, Maybe Text))
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
          pure $ Left $ Text.pack $ show err
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (logoResult, bannerResult) of
    (Left logoErr, _) -> pure $ Left logoErr
    (Right _logoPath, Left bannerErr) -> pure $ Left bannerErr
    (Right mLogoPath, Right mBannerPath) -> pure $ Right (mLogoPath, mBannerPath)

-- | Handle show creation after validation passes
handleShowCreation ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  UserMetadata.Model ->
  Shows.Insert ->
  NewShowForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handleShowCreation hxRequest userMetadata showData form = do
  -- Parse schedules first to check for conflicts before creating the show
  case parseSchedules (nsfSchedulesJson form) of
    Left err -> do
      Log.logInfo "Failed to parse schedules" (Aeson.object ["error" .= err])
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "Invalid schedule data: " <> err)
    Right schedules -> do
      -- Check for schedule conflicts before creating the show
      -- Use Shows.Id 0 as placeholder since the show doesn't exist yet
      -- This means we check against ALL active shows (no exclusion)
      conflictResult <- checkScheduleConflicts (Shows.Id 0) schedules
      case conflictResult of
        Left conflictErr -> do
          Log.logInfo "Schedule conflict detected" (Aeson.object ["error" .= conflictErr])
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate conflictErr)
        Right () -> do
          -- No conflicts, proceed with file uploads
          uploadResults <- processShowArtworkUploads showData.siSlug (nsfLogoFile form) (nsfBannerFile form)

          case uploadResults of
            Left uploadErr -> do
              Log.logInfo "Failed to upload show artwork" uploadErr
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "File upload error: " <> uploadErr)
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
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
                Right showId -> do
                  -- Assign hosts to the show
                  assignHostsToShow showId (nsfHosts form)

                  -- Create schedules (already validated for conflicts)
                  createSchedulesForShow showId schedules

                  -- Fetch the created show
                  execQuerySpan (Shows.getShowById showId) >>= \case
                    Right (Just createdShow) -> do
                      Log.logInfo "Successfully created show" (Aeson.object ["title" .= Shows.siTitle finalShowData, "id" .= show showId])
                      let detailUrl = Links.linkURI $ showGetLink createdShow.slug
                      html <- renderTemplate hxRequest (Just userMetadata) (successTemplate createdShow)
                      pure $ Servant.addHeader [i|/#{detailUrl}|] html
                    _ -> do
                      Log.logInfo_ "Created show but failed to retrieve it"
                      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Show was created but there was an error displaying the confirmation.")

-- | Assign hosts to a show and auto-promote regular users to Host role
assignHostsToShow ::
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
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  User.Id ->
  m ()
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

-- | Create schedules for a newly created show
createSchedulesForShow ::
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
                  ShowSchedule.stiWeeksOfMonth = Just (Vector.fromList (weeksOfMonth slot)),
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

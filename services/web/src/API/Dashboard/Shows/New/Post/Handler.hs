{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.New.Post.Route (NewShowForm (..))
import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), checkScheduleConflicts, insertScheduleSlot, parseScheduleSlot, validateSingleSlot)
import API.Links (dashboardShowsLinks)
import API.Types
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (forM, forM_, void)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Maybe (catMaybes, fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (Day)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Recurrence (recurrenceDay)
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (LocalTime (..), parseDateYMD, utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload qualified as FileUpload
import Effects.HostNotifications qualified as HostNotifications
import Hasql.Transaction qualified as HT
import Log qualified
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
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler cookie form =
  handleRedirectErrors "Show creation" dashboardShowsLinks.newGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only Admin users can create shows." userMetadata
    createdShow <- action form
    let showId = createdShow.id
        showSlug = createdShow.slug
        showTitle = createdShow.title
        detailUrl = [i|/#{dashboardShowDetailUrl showId showSlug}|] :: Text
        flash = FlashMessage Success "Show Created" [i|"#{showTitle}" has been created successfully.|]
    pure $ Servant.addHeader detailUrl $ Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

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

  -- 3. Parse optional schedule start date
  mStartDate <- case nsfScheduleStartDate form of
    Nothing -> pure Nothing
    Just dateText -> case parseDateYMD dateText of
      Nothing -> do
        Log.logInfo "Invalid schedule start date" dateText
        throwValidationError "Invalid schedule start date."
      Just d -> pure (Just d)

  -- 4. Check schedule conflicts (Shows.Id 0 means check against ALL active shows)
  --
  -- This flow accepts past start dates, so the conflict check clamps to today. A
  -- past date would re-admit long-dead validity windows and report conflicts with
  -- shows that vacated the slot months ago. A past overlap is historical, not
  -- bookable, so only the window from today forward can actually collide.
  today <- localDay . utcToPacific <$> lift currentSystemTime
  let conflictFromDate = maybe today (max today) mStartDate
  conflictResult <- lift $ checkScheduleConflicts (Shows.Id 0) schedules conflictFromDate
  case conflictResult of
    Left conflictErr -> do
      Log.logInfo "Schedule conflict detected" (Aeson.object ["error" .= conflictErr])
      throwValidationError conflictErr
    Right () -> pure ()

  -- 5. Process file uploads
  uploadResult <- lift $ processShowArtworkUploads showData.siSlug (nsfLogoFile form)
  mLogoPath <- case uploadResult of
    Left uploadErr -> do
      Log.logInfo "Failed to upload show artwork" uploadErr
      throwValidationError ("File upload error: " <> uploadErr)
    Right path -> pure path

  -- 6. Check slug uniqueness
  existingShow <- execQuery (Shows.getShowBySlug showData.siSlug)
  case existingShow of
    Left dbErr -> throwDatabaseError dbErr
    Right (Just _) -> throwValidationError "A show with this URL already exists. Try a different title."
    Right Nothing -> pure ()

  -- 7. Create the show, its hosts, its tags, and its schedule in one transaction.
  --
  -- The show row has to be inside. If it committed on its own and a later statement
  -- failed, staff would be left with a show that has no schedule, and the retry would
  -- be rejected by the slug uniqueness check in step 6.
  --
  -- The unclamped start date is used here so the stored effective_from is exactly
  -- what staff asked for.
  let finalShowData = showData {Shows.siLogoUrl = mLogoPath}
      startDate = fromMaybe today mStartDate
  creation <-
    lift (execTransaction (runExceptT (createShowTx finalShowData (nsfHosts form) (nsfTags form) schedules startDate))) >>= \case
      Left dbError -> do
        Log.logAttention "Show creation rolled back" (Aeson.object ["slug" .= showData.siSlug, "error" .= Text.pack (show dbError)])
        throwDatabaseError dbError
      Right (Left msg) -> do
        Log.logAttention "Show creation condemned" (Aeson.object ["slug" .= showData.siSlug, "error" .= msg])
        throwHandlerFailure msg
      Right (Right c) -> pure c

  let showId = creation.scShowId

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

  -- The transaction cannot log, so everything it wrote is reported here.
  Log.logInfo
    "Successfully created show"
    ( Aeson.object
        [ "show.id" .= showId,
          "title" .= createdShow.title,
          "promoted_hosts" .= creation.scPromotedHosts,
          "created_templates" .= creation.scCreatedTemplates
        ]
    )
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

-- | What creating a show wrote.
--
-- A 'HT.Transaction' has no 'MonadIO', so the transaction cannot log. It reports
-- what it did instead, and the caller writes one line after the commit.
data ShowCreation = ShowCreation
  { scShowId :: Shows.Id,
    -- | Hosts this creation promoted from User to Host.
    scPromotedHosts :: [User.Id],
    -- | Templates created for the submitted slots.
    scCreatedTemplates :: [ShowSchedule.TemplateId]
  }

-- | Create a show, its hosts, its tags, and its schedule, in one transaction.
--
-- @startDate@ arrives as an argument rather than from the clock, because
-- 'Hasql.Transaction.Sessions.transaction' retries the body on a serialization
-- conflict and the body must give the same answer each time.
createShowTx ::
  Shows.Insert ->
  [User.Id] ->
  -- | Comma-separated tags from the form.
  Maybe Text ->
  Maybe ParsedScheduleSlot ->
  Day ->
  ExceptT Text HT.Transaction ShowCreation
createShowTx showData hostIds mTags slot startDate = do
  showId <-
    lift (HT.statement () (Shows.insertShow showData)) >>= \case
      Just sid -> pure sid
      Nothing -> do
        lift HT.condemn
        throwE "Failed to create show."

  promoted <- lift $ assignHostsToShow showId hostIds
  lift $ processShowTags showId mTags
  templates <- createSchedulesForShow showId slot startDate

  pure
    ShowCreation
      { scShowId = showId,
        scPromotedHosts = promoted,
        scCreatedTemplates = templates
      }

-- | Assign hosts to a show and auto-promote regular users to Host role.
--
-- Returns the users it promoted.
assignHostsToShow ::
  Shows.Id ->
  [User.Id] ->
  HT.Transaction [User.Id]
assignHostsToShow showId hostIds =
  fmap catMaybes $
    forM hostIds $ \userId -> do
      promoted <- promoteUserToHostIfNeeded userId

      let hostInsert =
            ShowHost.Insert
              { ShowHost.shiId = showId,
                ShowHost.shiUserId = userId,
                ShowHost.shiRole = ShowHost.Host
              }
      HT.statement () (ShowHost.insertShowHost hostInsert)
      pure $ if promoted then Just userId else Nothing

-- | Promote a regular User to Host role if they are not already Host/Staff/Admin.
--
-- Returns whether it promoted them. A user with no metadata row is left alone.
promoteUserToHostIfNeeded ::
  User.Id ->
  HT.Transaction Bool
promoteUserToHostIfNeeded userId =
  HT.statement () (UserMetadata.getUserMetadata userId) >>= \case
    Nothing -> pure False
    Just metadata ->
      case metadata.mUserRole of
        UserMetadata.User -> do
          void $ HT.statement () (UserMetadata.updateUserRole userId UserMetadata.Host)
          pure True
        _ ->
          -- User already has Host, Staff, or Admin role - no promotion needed
          pure False

-- | Process comma-separated tags and associate them with a show.
processShowTags ::
  Shows.Id ->
  Maybe Text ->
  HT.Transaction ()
processShowTags _ Nothing = pure ()
processShowTags showId (Just tagsText) = do
  let tagNames = filter (not . Text.null) $ map Text.strip $ Text.splitOn "," tagsText
  forM_ tagNames $ \tagName -> do
    tagId <- HT.statement () (ShowTags.upsertShowTag tagName)
    HT.statement () (Shows.addTagToShow showId tagId)

--------------------------------------------------------------------------------
-- Schedule Creation Helpers

-- | Parse schedules JSON from form data, validate all fields, and check for overlaps.
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

-- | Create the show's schedule slot, if the form carried one.
--
-- @startDate@ becomes the @effective_from@ date of the new validity record. The slot
-- goes through 'insertScheduleSlot', the same helper the edit path uses, so both
-- paths write a template and its validity period the same way.
createSchedulesForShow ::
  Shows.Id ->
  Maybe ParsedScheduleSlot ->
  Day ->
  ExceptT Text HT.Transaction [ShowSchedule.TemplateId]
createSchedulesForShow showId slot startDate =
  traverse (insertScheduleSlot showId startDate) (maybe [] pure slot)

--------------------------------------------------------------------------------
-- Helper Functions

-- | Build a human-readable timeslot description from schedule slots.
--
-- Returns Nothing if no valid schedules, otherwise returns a formatted string
-- like "Fridays 8:00 PM - 10:00 PM PT"
buildTimeslotDescription :: Maybe ParsedScheduleSlot -> Maybe Text
buildTimeslotDescription =
  fmap $ \slot ->
    HostNotifications.formatTimeslotDescription (recurrenceDay (pssRecurrence slot)) (pssStart slot) (pssEnd slot)

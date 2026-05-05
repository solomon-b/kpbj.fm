{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Invite.Token.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (ParsedScheduleSlot (..), checkScheduleConflicts, parseScheduleSlot)
import API.Invite.Token.Post.Route (InviteOnboardingForm (..))
import API.Links (dashboardShowsLinks, userLinks)
import API.Types (DashboardShowsRoutes (..), UserRoutes (..))
import App.Auth qualified as Auth
import App.Config (Environment)
import App.Domains qualified as Domains
import App.Handler.Error (HandlerError (..), logHandlerError, throwDatabaseError, throwHandlerFailure, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (forM_, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Maybe (isJust)
import Data.Password.Argon2 (Argon2, PasswordHash, hashPassword, mkPassword)
import Data.Password.Validate qualified as PW.Validate
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.EmailAddress (EmailAddress)
import Domain.Types.EmailAddress qualified as EmailAddress
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Domain.Types.Timezone (LocalTime (..), utcToPacific)
import Effects.Clock (currentSystemTime)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execQueryThrow)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.ShowTags qualified as ShowTags
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.EmailVerification qualified as EmailVerification
import Effects.FileUpload qualified as FileUpload
import Hasql.Interpolate (OneRow (..))
import Log qualified
import Lucid qualified
import Network.Socket (SockAddr)
import OrphanInstances.OneRow ()
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, Mem)
import Web.HttpApiData qualified as Http

--------------------------------------------------------------------------------

-- | Result of a successful onboarding action.
data OnboardResult = OnboardResult
  { orUserId :: User.Id,
    orShowId :: Shows.Id,
    orShowSlug :: Slug
  }

-- | Error message for display in OOB banner.
errorMessage :: HandlerError -> Text
errorMessage = \case
  ValidationError msg -> msg
  DatabaseError _ -> "A database error occurred. Please try again."
  HandlerFailure msg -> msg
  NotFound resource -> resource <> " not found."
  _ -> "An unexpected error occurred. Please try again."

--------------------------------------------------------------------------------

-- | Handler for POST /invite/:token.
--
-- Creates a new user account and show from the onboarding form, then logs
-- the user in and redirects to their show dashboard. Uses the two-header
-- pattern (Set-Cookie + HX-Redirect) like Login and Register.
handler ::
  HostInvitation.Token ->
  Maybe Cookie ->
  SockAddr ->
  Maybe Text ->
  InviteOnboardingForm ->
  AppM
    ( Servant.Headers
        '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text]
        (Lucid.Html ())
    )
handler token _cookie remoteHost mUserAgent form = do
  result <- runExceptT $ action token form
  case result of
    Left err -> do
      logHandlerError "Host Onboarding" err
      pure $ Servant.noHeader $ Servant.noHeader $ renderBanner Error "Error" (errorMessage err)
    Right onboardResult -> do
      env <- asks (getter @Environment)
      sessionResult <- Auth.login onboardResult.orUserId remoteHost mUserAgent
      case sessionResult of
        Left loginErr -> do
          -- Session creation failed -- redirect to login without cookie
          Log.logInfo "Session creation failed after onboarding" (Text.pack $ show loginErr)
          let redirectUrl = "/" <> Http.toUrlPiece (userLinks.loginGet Nothing Nothing)
          pure $ Servant.noHeader $ Servant.addHeader redirectUrl $ renderBanner Success "Account Created" "Your account was created. Please log in."
        Right loginResult -> do
          -- Success -- set cookie and redirect to the new show dashboard
          let sessionId = Auth.loginResultSessionId loginResult
              cookieHeader = Auth.mkCookieSession env (Domains.cookieDomainMaybe env) sessionId
              showDetailUrl = Links.linkURI $ dashboardShowsLinks.detail onboardResult.orShowId onboardResult.orShowSlug Nothing
              redirectUrl = [i|/#{showDetailUrl}|] :: Text
          pure $ Servant.addHeader cookieHeader $ Servant.addHeader redirectUrl $ pure ()

--------------------------------------------------------------------------------

-- | Core onboarding business logic.
--
-- Validates all form fields, creates the user with Host role, creates the
-- show, assigns the host, creates schedule templates from the invitation
-- data, processes the logo upload, marks the invitation as claimed, and
-- sends a verification email.
action ::
  HostInvitation.Token ->
  InviteOnboardingForm ->
  ExceptT HandlerError AppM OnboardResult
action token form = do
  -- 1. Validate token
  invitation <- lookupInvitation token

  -- 2. Validate account fields
  validEmail <- validateEmail (iofEmail form)
  hashedPw <- validateAndHashPassword (iofPassword form)
  displayName <- validateDisplayName (iofDisplayName form)
  fullName <- validateFullName (iofFullName form)

  -- 3. Validate show fields
  let sanitizedTitle = Sanitize.sanitizeTitle (iofShowTitle form)
  when (Text.null (Text.strip sanitizedTitle)) $
    throwValidationError "Show title is required."
  let showSlug = Slug.mkSlug sanitizedTitle
      sanitizedDescription = Sanitize.sanitizeUserContent (iofShowDescription form)
      mDescription =
        if Text.null (Text.strip sanitizedDescription)
          then Nothing
          else Just sanitizedDescription

  -- 4. Check slug uniqueness
  existingShow <- execQuery (Shows.getShowBySlug showSlug)
  case existingShow of
    Left dbErr -> throwDatabaseError dbErr
    Right (Just _) -> throwValidationError "A show with this URL already exists. Try a different title."
    Right Nothing -> pure ()

  -- 5. Check email uniqueness
  mExisting <- execQuery (User.getUserByEmail validEmail)
  case mExisting of
    Left dbErr -> throwDatabaseError dbErr
    Right (Just _) -> throwValidationError "This email address is already registered."
    Right Nothing -> pure ()

  -- 6. Parse schedule data from invitation
  schedules <- case parseInvitationSchedules (invitation.hiScheduleData) of
    Left err -> do
      Log.logInfo "Failed to parse invitation schedule data" (Aeson.object ["error" .= err])
      throwValidationError ("Invalid schedule data: " <> err)
    Right s -> pure s

  -- 7. Check schedule conflicts
  conflictResult <- lift $ checkScheduleConflicts (Shows.Id 0) schedules
  case conflictResult of
    Left conflictErr -> do
      Log.logInfo "Schedule conflict detected during onboarding" (Aeson.object ["error" .= conflictErr])
      throwValidationError conflictErr
    Right () -> pure ()

  -- 8. Process logo upload
  uploadResult <- lift $ processShowArtworkUploads showSlug (iofShowLogo form)
  mLogoPath <- case uploadResult of
    Left uploadErr -> do
      Log.logInfo "Failed to upload show artwork during onboarding" uploadErr
      throwValidationError ("File upload error: " <> uploadErr)
    Right path -> pure path

  -- 9. Create user
  OneRow uid <- execQueryThrow $ User.insertUser (User.ModelInsert validEmail hashedPw)
  Log.logInfo "Created user for onboarding" (Aeson.object ["userId" .= show uid])

  -- 10. Create user metadata with Host role
  mMetadataId <-
    execQueryThrow $
      UserMetadata.insertUserMetadata $
        UserMetadata.Insert uid displayName fullName Nothing UserMetadata.Host UserMetadata.Automatic UserMetadata.DefaultTheme
  case mMetadataId of
    Nothing -> throwHandlerFailure "Failed to create user metadata."
    Just _ -> pure ()

  -- 11. Insert show
  insertResult <-
    execQuery $
      Shows.insertShow
        Shows.Insert
          { Shows.siTitle = sanitizedTitle,
            Shows.siSlug = showSlug,
            Shows.siDescription = mDescription,
            Shows.siLogoUrl = mLogoPath,
            Shows.siStatus = Shows.Active
          }
  showId <- case insertResult of
    Left dbErr -> do
      Log.logInfo "Database error creating show during onboarding" (Aeson.object ["error" .= Text.pack (show dbErr)])
      throwDatabaseError dbErr
    Right Nothing -> do
      Log.logInfo_ "Show insert returned Nothing during onboarding"
      throwHandlerFailure "Failed to create show."
    Right (Just sid) -> pure sid

  -- 12. Assign host to show
  hostInsertResult <-
    execQuery $
      ShowHost.insertShowHost
        ShowHost.Insert
          { ShowHost.shiId = showId,
            ShowHost.shiUserId = uid,
            ShowHost.shiRole = ShowHost.Host
          }
  case hostInsertResult of
    Left dbErr ->
      Log.logInfo "Failed to assign host to show" (Aeson.object ["userId" .= show uid, "error" .= Text.pack (show dbErr)])
    Right () ->
      Log.logInfo "Assigned host to show" (Aeson.object ["userId" .= show uid, "showId" .= show showId])

  -- 13. Process show tags
  lift $ processShowTags showId (iofShowTags form)

  -- 14. Create schedule templates
  lift $ createSchedulesForShow showId schedules

  -- 15. Mark invitation as claimed
  claimResult <- execQuery (HostInvitation.claimInvitation token uid)
  case claimResult of
    Left dbErr ->
      Log.logInfo "Failed to claim invitation" (Aeson.object ["error" .= Text.pack (show dbErr)])
    Right Nothing ->
      Log.logInfo "Invitation claim returned Nothing (may already be claimed)" token
    Right (Just _) ->
      Log.logInfo "Invitation claimed successfully" token

  -- 16. Send verification email (fire and forget)
  _ <- lift $ EmailVerification.createAndSendVerification uid validEmail

  -- 17. Subscribe to newsletter if opted in
  lift $ when (isJust (iofNewsletter form)) $ do
    Log.logInfo "User opted into newsletter during onboarding" (display validEmail)

  Log.logInfo "Host onboarding completed successfully" (Aeson.object ["userId" .= show uid, "showId" .= show showId, "slug" .= display showSlug])

  pure
    OnboardResult
      { orUserId = uid,
        orShowId = showId,
        orShowSlug = showSlug
      }

--------------------------------------------------------------------------------
-- Validation Helpers

-- | Look up a pending, non-expired invitation by token.
lookupInvitation ::
  HostInvitation.Token ->
  ExceptT HandlerError AppM HostInvitation.Model
lookupInvitation token = do
  result <- execQuery (HostInvitation.getByToken token)
  case result of
    Left dbErr -> throwDatabaseError dbErr
    Right Nothing -> throwValidationError "This invitation link is invalid, expired, or has already been used."
    Right (Just inv) -> pure inv

-- | Validate and parse an email address.
validateEmail ::
  Text ->
  ExceptT HandlerError AppM EmailAddress
validateEmail emailText = do
  let email = EmailAddress.mkEmailAddress emailText
  case EmailAddress.validate email of
    Left _ -> throwValidationError "Please enter a valid email address."
    Right validEmail -> pure validEmail

-- | Validate password policy and hash.
validateAndHashPassword ::
  Text ->
  ExceptT HandlerError AppM (PasswordHash Argon2)
validateAndHashPassword passwordText = do
  let password = mkPassword passwordText
  case PW.Validate.validatePassword PW.Validate.defaultPasswordPolicy_ password of
    PW.Validate.InvalidPassword _reasons ->
      throwValidationError "Password must be at least 8 characters."
    PW.Validate.ValidPassword ->
      lift $ hashPassword password

-- | Validate display name is non-empty.
validateDisplayName ::
  Text ->
  ExceptT HandlerError AppM DisplayName
validateDisplayName nameText =
  case DisplayName.mkDisplayName (Text.strip nameText) of
    Nothing -> throwValidationError "Display name is required."
    Just dn -> pure dn

-- | Validate full name is non-empty.
validateFullName ::
  Text ->
  ExceptT HandlerError AppM FullName
validateFullName nameText =
  case FullName.mkFullName (Text.strip nameText) of
    Nothing -> throwValidationError "Full name is required."
    Just fn -> pure fn

--------------------------------------------------------------------------------
-- Schedule Helpers

-- | Parse schedule data from invitation JSON.
--
-- The invitation stores schedule data as a JSON array of schedule slot objects.
-- This uses the same parsing logic as the show creation/edit handlers.
parseInvitationSchedules :: Aeson.Value -> Either Text [ParsedScheduleSlot]
parseInvitationSchedules scheduleJson = case scheduleJson of
  Aeson.Array arr
    | null arr -> Right []
  _ -> case Aeson.fromJSON scheduleJson of
    Aeson.Error err -> Left $ "Invalid schedule JSON: " <> Text.pack err
    Aeson.Success slots -> traverse parseScheduleSlot slots

-- | Create schedules for a newly created show.
--
-- Uses the current Pacific date as the effective_from date.
createSchedulesForShow ::
  Shows.Id ->
  [ParsedScheduleSlot] ->
  AppM ()
createSchedulesForShow showId slots = do
  nowUtc <- currentSystemTime
  let today = localDay (utcToPacific nowUtc)

  forM_ slots $ \slot -> do
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
        Log.logInfo "Failed to insert schedule template" (Aeson.object ["error" .= Text.pack (show err)])
      Right templateId -> do
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
            Log.logInfo "Created schedule for show" (Aeson.object ["showId" .= show showId, "day" .= show (pssDay slot)])
          Right Nothing ->
            Log.logInfo "insertValidity returned Nothing" (Aeson.object ["showId" .= show showId, "day" .= show (pssDay slot)])

--------------------------------------------------------------------------------
-- Logo Upload Helper

-- | Process logo file upload for the new show.
processShowArtworkUploads ::
  Slug ->
  Maybe (FileData Mem) ->
  AppM (Either Text (Maybe Text))
processShowArtworkUploads showSlug mLogoFile = do
  storageBackend <- asks getter
  mAwsEnv <- asks getter

  case mLogoFile of
    Nothing ->
      pure $ Right Nothing
    Just logoFile -> do
      FileUpload.uploadShowLogo storageBackend mAwsEnv showSlug logoFile >>= \case
        Left err -> do
          Log.logInfo "Failed to upload logo file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err
        Right Nothing -> pure $ Right Nothing
        Right (Just uploadResult) ->
          pure $ Right $ Just $ Text.pack $ uploadResultStoragePath uploadResult

--------------------------------------------------------------------------------
-- Tag Helpers

-- | Process comma-separated tags and associate them with a show.
processShowTags ::
  Shows.Id ->
  Maybe Text ->
  AppM ()
processShowTags _ Nothing = pure ()
processShowTags showId (Just tagsText) = do
  let tagNames = filter (not . Text.null) $ map Text.strip $ Text.splitOn "," tagsText
  forM_ tagNames $ \tagName -> do
    execQuery (ShowTags.getShowTagByName tagName) >>= \case
      Right (Just existingTag) -> do
        _ <- execQuery (Shows.addTagToShow showId (ShowTags.stId existingTag))
        Log.logInfo "Associated existing tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
      _ -> do
        execQuery (ShowTags.insertShowTag (ShowTags.Insert tagName)) >>= \case
          Right (Just newTagId) -> do
            _ <- execQuery (Shows.addTagToShow showId newTagId)
            Log.logInfo "Created and associated new tag with show" (Aeson.object ["tag" .= tagName, "showId" .= show showId])
          Right Nothing ->
            Log.logInfo "Tag insert returned Nothing" (Aeson.object ["tag" .= tagName])
          Left dbError ->
            Log.logInfo "Failed to create tag" (Aeson.object ["tag" .= tagName, "error" .= Text.pack (show dbError)])

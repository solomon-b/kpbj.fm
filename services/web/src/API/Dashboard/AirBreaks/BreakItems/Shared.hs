{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- | The handlers behind the staff break item dashboard sections.
--
-- PSAs, underwriting announcements, and station announcements are one table.
-- They are separate dashboard sections, and they differ only in the values a
-- 'Section' holds. Every handler here takes one and is otherwise shared.
--
-- Every handler here requires Staff in its type. The @RequireRole@ in the six
-- @Route.hs@ files of each section must also say @'Staff@, or the server does
-- not compile.
--
-- Show promos are not a section, because hosts submit them and staff review
-- them. Their pages reuse the form parsing and the audio claim from here.
module API.Dashboard.AirBreaks.BreakItems.Shared
  ( -- * Sections
    Section (..),
    psaSection,
    underwritingSection,
    announcementSection,

    -- * Handlers
    listHandler,
    newGetHandler,
    newPostHandler,
    newPostAction,
    editGetHandler,
    editPostHandler,
    deleteHandler,

    -- * Shared Steps
    parseBreakItemForm,
    ParsedBreakItem (..),
    checkItemLength,
    claimBreakItemAudio,
    ClaimedAudio (..),
    redirectTo,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm (..))
import API.Links
  ( apiLinks,
    dashboardAnnouncementsLinks,
    dashboardPsasLinks,
    dashboardUnderwritingLinks,
    rootLink,
  )
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (breakItemUploadUrl)
import App.Handler.Error
  ( HandlerError,
    handleBannerErrors,
    handleHtmlErrors,
    handleRedirectErrors,
    throwDatabaseError,
    throwHandlerFailure,
    throwNotFound,
    throwValidationError,
  )
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Component.BreakItems qualified as BreakItemsUI
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, getCurrentTime)
import Domain.BreakPlanner (categoryMaxSeconds)
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Domain.Types.Timezone (pacificDay, parseDateYMD)
import Effects.BreakPlan qualified as BreakPlan
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.StagedUploads (claimAndRelocateUpload)
import Log qualified
import Lucid qualified
import Servant qualified
import Servant.Links qualified as Links
import Text.Read (readMaybe)
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------
-- Sections

-- | One break item dashboard section.
data Section = Section
  { -- | Which rows this section owns.
    secCategory :: BreakItems.Category,
    -- | Which sidebar entry to highlight.
    secNav :: DashboardNav,
    -- | Everything the shared rendering needs.
    secUrls :: BreakItemsUI.SectionUrls,
    -- | Where an auth or access failure on a POST sends the user.
    secNewGetLink :: Links.Link,
    -- | The list page, which is where a successful POST lands.
    secListLink :: Links.Link,
    -- | Prefix for the error log lines this section emits.
    secLogName :: Text
  }

-- | The PSA section at @\/dashboard\/psas@.
psaSection :: Section
psaSection =
  Section
    { secCategory = BreakItems.Psa,
      secNav = NavPsas,
      secNewGetLink = dashboardPsasLinks.newGet,
      secListLink = dashboardPsasLinks.list Nothing,
      secLogName = "PSA",
      secUrls =
        BreakItemsUI.SectionUrls
          { suCategory = BreakItems.Psa,
            suNoun = "PSA",
            suNounPlural = "PSAs",
            suListUrl = Links.linkURI . dashboardPsasLinks.list . Just,
            suNewGetUrl = Links.linkURI dashboardPsasLinks.newGet,
            suNewPostUrl = Links.linkURI dashboardPsasLinks.newPost,
            suEditGetUrl = Links.linkURI . dashboardPsasLinks.editGet,
            suEditPostUrl = Links.linkURI . dashboardPsasLinks.editPost,
            suDeleteUrl = Links.linkURI . dashboardPsasLinks.delete,
            suTableBodyId = "psas-table-body",
            suUploadType = "break_item_audio"
          }
    }

-- | The underwriting section at @\/dashboard\/underwriting@.
underwritingSection :: Section
underwritingSection =
  Section
    { secCategory = BreakItems.Underwriting,
      secNav = NavUnderwriting,
      secNewGetLink = dashboardUnderwritingLinks.newGet,
      secListLink = dashboardUnderwritingLinks.list Nothing,
      secLogName = "Underwriting announcement",
      secUrls =
        BreakItemsUI.SectionUrls
          { suCategory = BreakItems.Underwriting,
            suNoun = "Underwriting Announcement",
            suNounPlural = "Underwriting",
            suListUrl = Links.linkURI . dashboardUnderwritingLinks.list . Just,
            suNewGetUrl = Links.linkURI dashboardUnderwritingLinks.newGet,
            suNewPostUrl = Links.linkURI dashboardUnderwritingLinks.newPost,
            suEditGetUrl = Links.linkURI . dashboardUnderwritingLinks.editGet,
            suEditPostUrl = Links.linkURI . dashboardUnderwritingLinks.editPost,
            suDeleteUrl = Links.linkURI . dashboardUnderwritingLinks.delete,
            suTableBodyId = "underwriting-table-body",
            suUploadType = "break_item_audio"
          }
    }

-- | The station announcement section at @\/dashboard\/announcements@.
announcementSection :: Section
announcementSection =
  Section
    { secCategory = BreakItems.Announcement,
      secNav = NavAnnouncements,
      secNewGetLink = dashboardAnnouncementsLinks.newGet,
      secListLink = dashboardAnnouncementsLinks.list Nothing,
      secLogName = "Announcement",
      secUrls =
        BreakItemsUI.SectionUrls
          { suCategory = BreakItems.Announcement,
            suNoun = "Announcement",
            suNounPlural = "Announcements",
            suListUrl = Links.linkURI . dashboardAnnouncementsLinks.list . Just,
            suNewGetUrl = Links.linkURI dashboardAnnouncementsLinks.newGet,
            suNewPostUrl = Links.linkURI dashboardAnnouncementsLinks.newPost,
            suEditGetUrl = Links.linkURI . dashboardAnnouncementsLinks.editGet,
            suEditPostUrl = Links.linkURI . dashboardAnnouncementsLinks.editPost,
            suDeleteUrl = Links.linkURI . dashboardAnnouncementsLinks.delete,
            suTableBodyId = "announcements-table-body",
            suUploadType = "break_item_audio"
          }
    }

--------------------------------------------------------------------------------
-- List

-- | @GET /dashboard/air-breaks/psas@ and @GET /dashboard/air-breaks/underwriting@.
listHandler :: Section -> Maybe PageNumber -> Satisfies 'UserMetadata.Staff RoleAuth -> Maybe FlashMessage -> Maybe HxRequest -> AppM (Lucid.Html ())
listHandler sec maybePage (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors (sec.secLogName <> " list") apiLinks.rootGet $ do
    let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
        limit = 20 :: Limit
        offset = fromIntegral ((pageNum - 1) * fromIntegral limit) :: Offset

    -- One row past the page, so the caller can tell whether another page exists
    -- without a second count query.
    fetched <-
      fromRightM throwDatabaseError $
        execQuery (BreakItems.getByCategory sec.secCategory (limit + 1) offset)
    let items = take (fromIntegral limit) fetched
        hasMore = length fetched > fromIntegral limit

    mNames <- case sec.secCategory of
      BreakItems.Psa -> pure Nothing
      BreakItems.Underwriting -> do
        underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
        pure $ Just $ Map.fromList [(u.uwId, u.uwName) | u <- underwriters]
      BreakItems.Announcement -> pure Nothing
      BreakItems.ShowPromo -> pure Nothing

    backend <- asks (Has.getter @StorageBackend)

    if hxRequest == IsHxRequest && pageNum > 1
      then pure $ BreakItemsUI.renderItemsFragment backend sec.secUrls mNames items page hasMore
      else
        lift $
          renderDashboardTemplate
            hxRequest
            mFlash
            auth.raMetadata
            NoShow
            sec.secNav
            Nothing
            (Just (BreakItemsUI.renderActionButton sec.secUrls))
            (BreakItemsUI.renderListPage backend sec.secUrls mNames items page hasMore)

--------------------------------------------------------------------------------
-- Upload

-- | @GET /dashboard/air-breaks/psas/new@ and @GET /dashboard/air-breaks/underwriting/new@.
newGetHandler :: Section -> Satisfies 'UserMetadata.Staff RoleAuth -> Maybe FlashMessage -> Maybe HxRequest -> AppM (Lucid.Html ())
newGetHandler sec (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors (sec.secLogName <> " upload form") apiLinks.rootGet $ do
    env <- asks (Has.getter @Environment)
    today <- pacificDay <$> liftIO getCurrentTime
    mUf <- sectionUnderwritingFields sec Nothing

    lift $
      renderDashboardTemplate
        hxRequest
        mFlash
        auth.raMetadata
        NoShow
        sec.secNav
        Nothing
        Nothing
        (BreakItemsUI.renderUploadForm sec.secUrls (breakItemUploadUrl env sec.secCategory) today mUf)

-- | @POST /dashboard/air-breaks/psas/new@ and the other sections' upload posts.
newPostHandler ::
  Section ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
newPostHandler sec (Satisfies _ auth) form =
  handleRedirectErrors (sec.secLogName <> " upload") sec.secNewGetLink $ do
    _ <- newPostAction sec auth.raUser form
    -- Today's break plan is built once, so an item uploaded today can air
    -- from tomorrow at the earliest.
    pure $
      redirectTo
        (rootLink sec.secListLink)
        (FlashMessage Success (sec.secLogName <> " Uploaded") "It can air from tomorrow, or from its first air date if that is later.")

-- | Parse the form, claim the audio, and save the row.
--
-- Returns the ID of the new row. Every row a section saves is approved,
-- because staff upload it.
newPostAction ::
  Section ->
  User.Model ->
  BreakItemForm ->
  ExceptT HandlerError AppM BreakItems.Id
newPostAction sec user form = do
  parsed <- parseBreakItemForm sec.secCategory form
  audio <- claimBreakItemAudio user sec.secCategory form.bifAudioToken

  newId <-
    fromMaybeM (throwHandlerFailure ("Failed to create " <> sec.secLogName <> " record.")) $
      fromRightM throwDatabaseError $
        execQuery
          ( BreakItems.insertBreakItem
              BreakItems.Insert
                { biiTitle = parsed.pbiTitle,
                  biiCategory = sec.secCategory,
                  biiUnderwriterId = parsed.pbiUnderwriterId,
                  biiShowId = Nothing,
                  biiReviewStatus = BreakItems.Approved,
                  biiAudioFilePath = audio.caStoragePath,
                  biiMimeType = audio.caMimeType,
                  biiFileSize = audio.caFileSize,
                  biiDurationSeconds = audio.caDurationSeconds,
                  biiStartsOn = parsed.pbiStartsOn,
                  biiEndsOn = parsed.pbiEndsOn,
                  biiCreatorId = User.mId user
                }
          )

  Log.logInfo (sec.secLogName <> " uploaded successfully") parsed.pbiTitle
  pure newId

--------------------------------------------------------------------------------
-- Edit

-- | @GET /dashboard/air-breaks/psas/:id/edit@ and @GET /dashboard/air-breaks/underwriting/:id/edit@.
editGetHandler ::
  Section ->
  BreakItems.Id ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe FlashMessage ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
editGetHandler sec itemId (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleRedirectErrors (sec.secLogName <> " edit form") sec.secListLink $ do
    item <- fetchInSection sec itemId
    mUf <- sectionUnderwritingFields sec (Just item)
    html <-
      lift $
        renderDashboardTemplate
          hxRequest
          mFlash
          auth.raMetadata
          NoShow
          sec.secNav
          Nothing
          Nothing
          (BreakItemsUI.renderEditForm sec.secUrls item mUf)
    pure $ Servant.noHeader html

-- | @POST /dashboard/air-breaks/psas/:id/edit@ and @POST /dashboard/air-breaks/underwriting/:id/edit@.
editPostHandler ::
  Section ->
  BreakItems.Id ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
editPostHandler sec itemId _ form =
  handleRedirectErrors (sec.secLogName <> " edit") sec.secListLink $ do
    -- Reads through the section, so an id from the other category is a 404
    -- rather than a cross-section edit.
    item <- fetchInSection sec itemId
    parsed <- parseBreakItemForm sec.secCategory form

    _ <-
      fromMaybeM (throwNotFound sec.secLogName) $
        fromRightM throwDatabaseError $
          execQuery
            ( BreakItems.updateBreakItem
                item.bimId
                parsed.pbiTitle
                parsed.pbiStartsOn
                parsed.pbiEndsOn
                parsed.pbiUnderwriterId
            )

    Log.logInfo (sec.secLogName <> " updated successfully") parsed.pbiTitle
    pure $
      redirectTo
        (rootLink sec.secListLink)
        (FlashMessage Success (sec.secLogName <> " Updated") "Your changes are live.")

--------------------------------------------------------------------------------
-- Delete

-- | @DELETE /dashboard/air-breaks/psas/:id@ and @DELETE /dashboard/air-breaks/underwriting/:id@.
deleteHandler :: Section -> BreakItems.Id -> Satisfies 'UserMetadata.Staff RoleAuth -> AppM (Lucid.Html ())
deleteHandler sec itemId _ =
  handleBannerErrors (sec.secLogName <> " delete") $ do
    item <- fetchInSection sec itemId

    result <- execQuery (BreakItems.softDeleteBreakItem item.bimId)
    case result of
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound sec.secLogName
      Right (Just _) -> Log.logInfo (sec.secLogName <> " deleted successfully") item.bimId

    pure $ do
      mempty
      renderBanner
        Success
        (sec.secLogName <> " Deleted")
        "It has stopped airing. Its past plays stay in the record."

--------------------------------------------------------------------------------
-- Form parsing

-- | A break item form after validation.
data ParsedBreakItem = ParsedBreakItem
  { pbiTitle :: Text,
    pbiStartsOn :: Day,
    pbiEndsOn :: Maybe Day,
    -- | Set for underwriting, absent for every other category.
    pbiUnderwriterId :: Maybe Underwriters.Id
  }
  deriving stock (Show, Eq)

-- | Validate a submitted break item form.
--
-- The form carries no audio length. The upload path reads it from the staged
-- upload row with 'checkItemLength'.
parseBreakItemForm :: BreakItems.Category -> BreakItemForm -> ExceptT HandlerError AppM ParsedBreakItem
parseBreakItemForm category form = do
  let title = Sanitize.sanitizePlainText form.bifTitle
  when (Text.null title) $ throwValidationError "Title is required."

  startsOn <-
    fromMaybeM (throwValidationError "A first air date is required.") $
      pure (parseDateYMD form.bifStartsOn)

  endsOn <-
    if Text.null (Text.strip form.bifEndsOn)
      then pure Nothing
      else
        fmap Just $
          fromMaybeM (throwValidationError "The last air date is not a date.") $
            pure (parseDateYMD form.bifEndsOn)

  case endsOn of
    Just end | end < startsOn -> throwValidationError "The last air date is before the first."
    _ -> pure ()

  underwriterId <- case category of
    BreakItems.Psa -> pure Nothing
    BreakItems.Underwriting ->
      fmap Just $
        fromMaybeM (throwValidationError "Choose an underwriter.") $
          pure (Underwriters.Id <$> readMaybe (Text.unpack (Text.strip form.bifUnderwriterId)))
    BreakItems.Announcement -> pure Nothing
    BreakItems.ShowPromo -> pure Nothing

  pure
    ParsedBreakItem
      { pbiTitle = title,
        pbiStartsOn = startsOn,
        pbiEndsOn = endsOn,
        pbiUnderwriterId = underwriterId
      }

-- | Check the audio length that the staged upload row holds.
--
-- The break template only fits when every item stays within its limit. The
-- length is absent when the upload handler could not measure the file. Only
-- an upload calls this, because an edit never replaces the audio.
checkItemLength :: BreakItems.Category -> Maybe Int64 -> Either Text Int64
checkItemLength category mDurationSeconds =
  case mDurationSeconds of
    Nothing ->
      Left "Could not read the audio length. Upload the file again."
    Just seconds
      | seconds > limit -> Left (tooLongMessage seconds)
      | otherwise -> Right seconds
  where
    limit = categoryMaxSeconds category

    tooLongMessage seconds =
      categoryPlural category
        <> " can run at most "
        <> Text.pack (show limit)
        <> " seconds. This file runs "
        <> Text.pack (show seconds)
        <> "."

-- | A staged audio upload after its checks and its claim.
data ClaimedAudio = ClaimedAudio
  { -- | Where the claim moved the file.
    caStoragePath :: Text,
    caMimeType :: Text,
    caFileSize :: Int64,
    caDurationSeconds :: Int64
  }
  deriving stock (Show, Eq)

-- | Check the staged audio upload that a form names, then claim it.
--
-- The upload handler measured the length and stored it on the staged row.
-- The length check runs before the claim, because the claim moves the file.
claimBreakItemAudio ::
  User.Model ->
  BreakItems.Category ->
  -- | The staged upload token from the form
  Text ->
  ExceptT HandlerError AppM ClaimedAudio
claimBreakItemAudio user category audioToken = do
  when (Text.null audioToken) $
    throwValidationError "Audio file is required."

  stagedUpload <-
    fromMaybeM (throwValidationError "Uploaded file not found or expired.") $
      fromRightM throwDatabaseError $
        execQuery (StagedUploads.getByToken (StagedUploads.Token audioToken))

  duration <-
    case checkItemLength category (StagedUploads.durationSeconds stagedUpload) of
      Left message -> throwValidationError message
      Right seconds -> pure seconds

  now <- liftIO getCurrentTime
  claimResult <-
    lift $
      claimAndRelocateUpload
        (User.mId user)
        audioToken
        StagedUploads.BreakItemAudio
        AudioBucket
        BreakItemAudio
        now
        "break-item"
  storagePath <- case claimResult of
    Left err -> do
      Log.logInfo "Failed to claim staged upload" err
      throwValidationError err
    Right path -> pure path

  pure
    ClaimedAudio
      { caStoragePath = storagePath,
        caMimeType = StagedUploads.mimeType stagedUpload,
        caFileSize = StagedUploads.fileSize stagedUpload,
        caDurationSeconds = duration
      }

-- | A category's name in the plural, for validation messages.
categoryPlural :: BreakItems.Category -> Text
categoryPlural = \case
  BreakItems.Psa -> "PSAs"
  BreakItems.Underwriting -> "Underwriting announcements"
  BreakItems.Announcement -> "Announcements"
  BreakItems.ShowPromo -> "Show promos"

--------------------------------------------------------------------------------
-- Helpers

-- | Fetch a row and confirm this section owns it.
--
-- A row of the other category reads as absent, so one section's URLs can never
-- reach the other section's rows.
fetchInSection :: Section -> BreakItems.Id -> ExceptT HandlerError AppM BreakItems.Model
fetchInSection sec itemId = do
  item <-
    fromMaybeM (throwNotFound sec.secLogName) $
      fromRightM throwDatabaseError $
        execQuery (BreakItems.getById itemId)
  when (item.bimCategory /= sec.secCategory) $ throwNotFound sec.secLogName
  pure item

-- | The underwriting form fields, for the underwriting section only.
sectionUnderwritingFields ::
  Section ->
  Maybe BreakItems.Model ->
  ExceptT HandlerError AppM (Maybe BreakItemsUI.UnderwritingFields)
sectionUnderwritingFields sec mItem = case sec.secCategory of
  BreakItems.Psa -> pure Nothing
  BreakItems.Underwriting -> Just <$> underwritingFields mItem
  BreakItems.Announcement -> pure Nothing
  BreakItems.ShowPromo -> pure Nothing

-- | The underwriter list and today's share, for the underwriting form.
underwritingFields :: Maybe BreakItems.Model -> ExceptT HandlerError AppM BreakItemsUI.UnderwritingFields
underwritingFields mItem = do
  underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
  today <- pacificDay <$> liftIO getCurrentTime
  items <- fromRightM throwDatabaseError $ execQuery (BreakItems.getActiveOnDay today)
  kinds <-
    fromRightM throwDatabaseError $
      execQuery (uncurry ShowSchedule.breakKindsBetween (BreakPlan.dayBoundaries today))
  let active = Set.size (Set.fromList (mapMaybe (.bimUnderwriterId) items))
      showBreaks = length [() | (_, ShowSchedule.ShowBreak) <- kinds]
  pure
    BreakItemsUI.UnderwritingFields
      { ufUnderwriters = underwriters,
        ufSelected = mItem >>= (.bimUnderwriterId),
        ufActiveUnderwriters = active,
        ufShowBreaksToday = showBreaks
      }

-- | Build the redirect-with-flash response of Pattern A.
redirectTo ::
  Text ->
  FlashMessage ->
  Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent
redirectTo url flash =
  Servant.addHeader url $
    Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

-- | The handlers behind both break item dashboard sections.
--
-- PSAs and underwriting announcements are one table and one playout rotation. They are
-- two dashboard sections so each can carry its own permission gate, and the two
-- differ only in the values a 'Section' holds. Every handler here takes one and
-- is otherwise shared.
--
-- To change who may manage a section, change its 'secRequire' below. Nothing
-- else has to move.
module API.Dashboard.BreakItems.Shared
  ( -- * Sections
    Section (..),
    psaSection,
    underwritingSection,

    -- * Handlers
    listHandler,
    newGetHandler,
    newPostHandler,
    editGetHandler,
    editPostHandler,
    deleteHandler,

    -- * Exported for testing
    parseBreakItemForm,
    ParsedBreakItem (..),
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.BreakItems.Form (BreakItemForm (..))
import API.Links
  ( apiLinks,
    dashboardPsasLinks,
    dashboardUnderwritingLinks,
    rootLink,
  )
import API.Types
import App.Common (renderDashboardTemplate)
import App.Config (Environment)
import App.Domains (audioUploadUrl)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
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
import Data.Either (fromRight)
import Data.Has qualified as Has
import Data.Int (Int64)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time
  ( Day,
    fromGregorian,
    getCurrentTime,
    gregorianMonthLength,
    toGregorian,
  )
import Domain.BreakPlanner (owedInMonth)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.FileStorage (BucketType (..), ResourceType (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Timezone (pacificDay, parseDateYMD)
import Effects.BreakPlan qualified as BreakPlan
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
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
    -- | The permission gate. Change this alone to re-gate a section.
    secRequire :: UserMetadata.Model -> ExceptT HandlerError AppM (),
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
      secRequire = requireStaffNotSuspended "You do not have permission to manage PSAs.",
      secNewGetLink = dashboardPsasLinks.newGet,
      secListLink = dashboardPsasLinks.list Nothing,
      secLogName = "PSA",
      secUrls =
        BreakItemsUI.SectionUrls
          { suNoun = "PSA",
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
      secRequire = requireStaffNotSuspended "You do not have permission to manage underwriting.",
      secNewGetLink = dashboardUnderwritingLinks.newGet,
      secListLink = dashboardUnderwritingLinks.list Nothing,
      secLogName = "Underwriting announcement",
      secUrls =
        BreakItemsUI.SectionUrls
          { suNoun = "Underwriting Announcement",
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

--------------------------------------------------------------------------------
-- List

-- | @GET /dashboard/psas@ and @GET /dashboard/underwriting@.
listHandler :: Section -> Maybe PageNumber -> Maybe Cookie -> Maybe HxRequest -> AppM (Lucid.Html ())
listHandler sec maybePage cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors (sec.secLogName <> " list") apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata

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

    if hxRequest == IsHxRequest && pageNum > 1
      then pure $ BreakItemsUI.renderItemsFragment sec.secUrls mNames items page hasMore
      else do
        (allShows, selectedShow) <- sidebarShows user userMetadata
        lift $
          renderDashboardTemplate
            hxRequest
            userMetadata
            allShows
            selectedShow
            sec.secNav
            Nothing
            (Just (BreakItemsUI.renderActionButton sec.secUrls))
            (BreakItemsUI.renderListPage sec.secUrls mNames items page hasMore)

--------------------------------------------------------------------------------
-- Upload

-- | @GET /dashboard/psas/new@ and @GET /dashboard/underwriting/new@.
newGetHandler :: Section -> Maybe Cookie -> Maybe HxRequest -> AppM (Lucid.Html ())
newGetHandler sec cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors (sec.secLogName <> " upload form") apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata
    (allShows, selectedShow) <- sidebarShows user userMetadata

    env <- asks (Has.getter @Environment)
    today <- pacificDay <$> liftIO getCurrentTime
    mUf <- sectionUnderwritingFields sec Nothing

    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        allShows
        selectedShow
        sec.secNav
        Nothing
        Nothing
        (BreakItemsUI.renderUploadForm sec.secUrls (audioUploadUrl env) today mUf)

-- | @POST /dashboard/psas/new@ and @POST /dashboard/underwriting/new@.
newPostHandler ::
  Section ->
  Maybe Cookie ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
newPostHandler sec cookie form =
  handleRedirectErrors (sec.secLogName <> " upload") sec.secNewGetLink $ do
    (user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata

    parsed <- parseBreakItemForm sec.secCategory form
    when (Text.null form.bifAudioToken) $
      throwValidationError "Audio file is required."
    duration <-
      fromMaybeM (throwValidationError "Could not read the audio length. Choose the file again.") $
        pure parsed.pbiDurationSeconds

    stagedUpload <-
      fromMaybeM (throwValidationError "Uploaded file not found or expired.") $
        fromRightM throwDatabaseError $
          execQuery (StagedUploads.getByToken (StagedUploads.Token form.bifAudioToken))

    now <- liftIO getCurrentTime
    claimResult <-
      lift $
        claimAndRelocateUpload
          (User.mId user)
          form.bifAudioToken
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

    _ <-
      fromMaybeM (throwHandlerFailure ("Failed to create " <> sec.secLogName <> " record.")) $
        fromRightM throwDatabaseError $
          execQuery
            ( BreakItems.insertBreakItem
                BreakItems.Insert
                  { biiTitle = parsed.pbiTitle,
                    biiCategory = sec.secCategory,
                    biiUnderwriterId = parsed.pbiUnderwriterId,
                    biiSpotsPerMonth = parsed.pbiSpotsPerMonth,
                    biiAudioFilePath = storagePath,
                    biiMimeType = StagedUploads.mimeType stagedUpload,
                    biiFileSize = StagedUploads.fileSize stagedUpload,
                    biiDurationSeconds = duration,
                    biiStartsOn = parsed.pbiStartsOn,
                    biiEndsOn = parsed.pbiEndsOn,
                    biiCreatorId = User.mId user
                  }
            )

    Log.logInfo (sec.secLogName <> " uploaded successfully") parsed.pbiTitle
    pure $
      redirectTo
        (rootLink sec.secListLink)
        (FlashMessage Success (sec.secLogName <> " Uploaded") "It will start airing on its first air date.")

--------------------------------------------------------------------------------
-- Edit

-- | @GET /dashboard/psas/:id/edit@ and @GET /dashboard/underwriting/:id/edit@.
editGetHandler ::
  Section ->
  BreakItems.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
editGetHandler sec itemId cookie (foldHxReq -> hxRequest) =
  handleRedirectErrors (sec.secLogName <> " edit form") sec.secListLink $ do
    (user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata
    item <- fetchInSection sec itemId
    mUf <- sectionUnderwritingFields sec (Just item)
    (allShows, selectedShow) <- sidebarShows user userMetadata
    html <-
      lift $
        renderDashboardTemplate
          hxRequest
          userMetadata
          allShows
          selectedShow
          sec.secNav
          Nothing
          Nothing
          (BreakItemsUI.renderEditForm sec.secUrls item mUf)
    pure $ Servant.noHeader html

-- | @POST /dashboard/psas/:id/edit@ and @POST /dashboard/underwriting/:id/edit@.
editPostHandler ::
  Section ->
  BreakItems.Id ->
  Maybe Cookie ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
editPostHandler sec itemId cookie form =
  handleRedirectErrors (sec.secLogName <> " edit") sec.secListLink $ do
    (_user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata

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
                parsed.pbiSpotsPerMonth
            )

    Log.logInfo (sec.secLogName <> " updated successfully") parsed.pbiTitle
    pure $
      redirectTo
        (rootLink sec.secListLink)
        (FlashMessage Success (sec.secLogName <> " Updated") "Your changes are live.")

--------------------------------------------------------------------------------
-- Delete

-- | @DELETE /dashboard/psas/:id@ and @DELETE /dashboard/underwriting/:id@.
deleteHandler :: Section -> BreakItems.Id -> Maybe Cookie -> AppM (Lucid.Html ())
deleteHandler sec itemId cookie =
  handleBannerErrors (sec.secLogName <> " delete") $ do
    (_user, userMetadata) <- requireAuth cookie
    sec.secRequire userMetadata
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
    -- | Absent when the browser could not read the file. Required on upload,
    -- and left at the stored value on an edit.
    pbiDurationSeconds :: Maybe Int64,
    pbiStartsOn :: Day,
    pbiEndsOn :: Maybe Day,
    -- | Set for underwriting, absent for a PSA.
    pbiUnderwriterId :: Maybe Underwriters.Id,
    -- | Set for underwriting, absent for a PSA.
    pbiSpotsPerMonth :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | Validate a submitted break item form.
--
-- The duration arrives in a form field, so it is checked for sanity rather than
-- trusted: a value outside one second to one hour is treated as absent. A break
-- window is two minutes, so anything near the upper bound would never be picked
-- anyway, and a wrong value would only ever cost one window.
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

  (underwriterId, spots) <- case category of
    BreakItems.Psa -> pure (Nothing, Nothing)
    BreakItems.Underwriting -> do
      uw <-
        fromMaybeM (throwValidationError "Choose an underwriter.") $
          pure (Underwriters.Id <$> readMaybe (Text.unpack (Text.strip form.bifUnderwriterId)))
      n <-
        fromMaybeM (throwValidationError "Spots per month must be a whole number above zero.") $
          pure $ do
            v <- readMaybe (Text.unpack (Text.strip form.bifSpotsPerMonth))
            if v > 0 then Just v else Nothing
      pure (Just uw, Just n)

  let duration = do
        parsed <- readMaybe (Text.unpack (Text.strip form.bifDurationSeconds))
        if parsed > 0 && parsed <= 3600 then Just parsed else Nothing

  pure
    ParsedBreakItem
      { pbiTitle = title,
        pbiDurationSeconds = duration,
        pbiStartsOn = startsOn,
        pbiEndsOn = endsOn,
        pbiUnderwriterId = underwriterId,
        pbiSpotsPerMonth = spots
      }

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

-- | The underwriter list and this month's capacity, for the underwriting form.
underwritingFields :: Maybe BreakItems.Model -> ExceptT HandlerError AppM BreakItemsUI.UnderwritingFields
underwritingFields mItem = do
  underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
  today <- pacificDay <$> liftIO getCurrentTime
  let (y, m, _) = toGregorian today
      firstDay = fromGregorian y m 1
      lastDay = fromGregorian y m (gregorianMonthLength y m)
  items <- fromRightM throwDatabaseError $ execQuery (BreakItems.getActiveInMonth firstDay lastDay)
  kinds <-
    fromRightM throwDatabaseError $
      execQuery
        ( ShowSchedule.breakKindsBetween
            (fst (BreakPlan.dayBoundaries firstDay))
            (snd (BreakPlan.dayBoundaries lastDay))
        )
  let owed =
        sum
          [ owedInMonth firstDay item.bimStartsOn item.bimEndsOn n
          | item <- items,
            Just n <- [item.bimSpotsPerMonth]
          ]
      showBreaks = fromIntegral (length [() | (_, ShowSchedule.ShowBreak) <- kinds])
  pure
    BreakItemsUI.UnderwritingFields
      { ufUnderwriters = underwriters,
        ufSelected = mItem >>= (.bimUnderwriterId),
        ufSpotsPerMonth = maybe "" (Text.pack . show) (mItem >>= (.bimSpotsPerMonth)),
        ufCapacity = (owed, showBreaks)
      }

-- | The shows the dashboard sidebar lists for this user.
sidebarShows ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM ([Shows.Model], Maybe Shows.Model)
sidebarShows user userMetadata = do
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
  pure (allShows, listToMaybe allShows)

-- | Build the redirect-with-flash response of Pattern A.
redirectTo ::
  Text ->
  FlashMessage ->
  Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent
redirectTo url flash =
  Servant.addHeader url $
    Servant.addHeader (flashCookie (Just flash)) Servant.NoContent

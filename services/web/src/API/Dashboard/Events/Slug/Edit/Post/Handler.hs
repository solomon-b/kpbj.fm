{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Edit.Post.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Edit.Post.Route (EventEditForm (..), parseDateTime, parseStatus)
import API.Links (dashboardEventsLinks, rootLink)
import API.Types (DashboardEventsRoutes (..))
import App.Handler.Combinators (requireAuth, requireJust, requireRight, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotAuthorized, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Monad (foldM, forM, forM_, unless, void, when)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON (..), (.:), (.:?))
import Data.Aeson qualified as Aeson
import Data.Has (getter)
import Data.Int (Int64)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Data.Text.Display (display)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.EventImages qualified as EventImages
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (uploadEventGalleryImage, uploadEventPosterImage)
import Effects.StagedUploadCleanup (deleteFile)
import Hasql.Transaction qualified as HT
import Log qualified
import Servant qualified
import Servant.Multipart (FileData, Mem)
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to build the post-update redirect.
data EventEditRedirectData = EventEditRedirectData
  { eerRedirectUrl :: Text,
    eerFlash :: FlashMessage
  }

-- | Business logic: fetch event, authorize, validate, update, build redirect data.
action ::
  User.Model ->
  UserMetadata.Model ->
  Events.Id ->
  Slug ->
  EventEditForm ->
  ExceptT HandlerError AppM EventEditRedirectData
action user userMetadata eventId _slug editForm = do
  -- 1. Fetch event
  event <-
    fromMaybeM (throwNotFound "Event") $
      fromRightM throwDatabaseError $
        execQuery (Events.getEventById eventId)

  -- 2. Check authorization: must be staff/admin or the creator
  unless (event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole) $
    throwNotAuthorized "You can only edit events you created or have staff permissions." (Just userMetadata.mUserRole)

  updateEvent eventId event editForm

-- | Servant handler: thin glue composing action + building redirect response.
handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  EventEditForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler eventId slug cookie editForm =
  handleRedirectErrors "Event update" (dashboardEventsLinks.editGet eventId slug) $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to edit events." userMetadata
    vd <- action user userMetadata eventId slug editForm
    pure $ Servant.addHeader vd.eerRedirectUrl $ Servant.addHeader (flashCookie (Just vd.eerFlash)) Servant.NoContent

updateEvent ::
  Events.Id ->
  Events.Model ->
  EventEditForm ->
  ExceptT HandlerError AppM EventEditRedirectData
updateEvent eventId event editForm = do
  -- 3. Parse and validate form data
  parsedStatus <- requireJust "Invalid event status value." (parseStatus (eefStatus editForm))
  parsedStartsAt <- requireJust "Invalid start date/time format." (parseDateTime (eefStartsAt editForm))
  parsedEndsAt <- requireJust "Invalid end date/time format." (parseDateTime (eefEndsAt editForm))

  -- 4. Sanitize and validate content
  let sanitizedTitle = Sanitize.sanitizeTitle (eefTitle editForm)
      sanitizedDescription = Sanitize.sanitizeUserContent (eefDescription editForm)
      sanitizedLocationName = Sanitize.sanitizePlainText (eefLocationName editForm)
      sanitizedLocationAddress = Sanitize.sanitizeDescription (eefLocationAddress editForm)

  validTitle <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 200 sanitizedTitle)
  validDescription <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 5000 sanitizedDescription)
  validLocationName <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 100 sanitizedLocationName)
  validLocationAddress <- requireRight Sanitize.displayContentValidationError (Sanitize.validateContentLength 500 sanitizedLocationAddress)

  -- 5. Upload poster image if provided, or clear if requested
  posterImagePath <- case eefPosterImage editForm of
    Nothing ->
      -- No new file: check if user wants to clear existing image
      if eefPosterImageClear editForm
        then do
          Log.logInfo "Clearing poster image" event.emId
          pure Nothing
        else pure event.emPosterImageUrl -- Keep existing image
    Just posterImageFile -> do
      let newSlug = Slug.mkSlug validTitle
      storageBackend <- asks getter
      mAwsEnv <- asks getter
      uploadResult <- lift $ uploadEventPosterImage storageBackend mAwsEnv newSlug posterImageFile
      case uploadResult of
        Left uploadError -> do
          Log.logInfo "Poster image upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
          pure event.emPosterImageUrl -- Keep existing image on error
        Right Nothing ->
          -- No file selected: check if user wants to clear existing image
          if eefPosterImageClear editForm
            then do
              Log.logInfo "Clearing poster image" event.emId
              pure Nothing
            else pure event.emPosterImageUrl -- Keep existing
        Right (Just result) -> do
          Log.logInfo "Poster image uploaded successfully" (Aeson.object ["path" Aeson..= uploadResultStoragePath result])
          pure (Just $ Text.pack $ uploadResultStoragePath result)

  -- 6. Build update data
  let featuredOnHomepage = eefFeaturedOnHomepage editForm == "true"
      newSlug = Slug.mkSlug validTitle
      updateData =
        Events.Insert
          { Events.eiTitle = validTitle,
            Events.eiSlug = newSlug,
            Events.eiDescription = validDescription,
            Events.eiStartsAt = parsedStartsAt,
            Events.eiEndsAt = parsedEndsAt,
            Events.eiLocationName = validLocationName,
            Events.eiLocationAddress = validLocationAddress,
            Events.eiStatus = parsedStatus,
            Events.eiAuthorId = event.emAuthorId,
            Events.eiPosterImageUrl = posterImagePath,
            Events.eiFeaturedOnHomepage = featuredOnHomepage
          }

  -- 7. Prepare gallery photo updates (validate deletions, upload new files — no DB writes)
  (failedUploads, imagesToDelete, imageIdsToDelete, metaUpdates, newInserts) <-
    prepareGalleryUpdates eventId (display newSlug) editForm

  -- 8. Update the event and apply gallery mutations in a single transaction
  mUpdateResult <-
    fromRightM throwDatabaseError $
      execTransaction $
        runMaybeT $ do
          when featuredOnHomepage $
            lift $
              void $
                HT.statement () Events.clearFeaturedEvents
          _ <- MaybeT $ HT.statement () (Events.updateEvent event.emId updateData)
          lift $ galleryTransaction imageIdsToDelete metaUpdates newInserts
          pure ()

  case mUpdateResult of
    Nothing -> throwHandlerFailure "Event update returned Nothing"
    Just _ -> do
      Log.logInfo "Successfully updated event" event.emId

      -- 9. Delete removed gallery files from storage (after transaction succeeded)
      forM_ imagesToDelete $ \(_imageId, imagePath) -> do
        storageBackend <- lift $ asks getter
        mAwsEnv <- lift $ asks getter
        deleted <- lift $ deleteFile storageBackend mAwsEnv imagePath
        if deleted
          then Log.logInfo "Deleted event gallery image file" imagePath
          else Log.logInfo "Failed to delete event gallery image file (may not exist)" imagePath

      let detailUrl = rootLink $ dashboardEventsLinks.detail eventId newSlug
          flash
            | failedUploads > 0 =
                FlashMessage Warning "Event Updated" $
                  "Event saved, but " <> Text.pack (show failedUploads) <> " photo(s) failed to upload."
            | otherwise =
                FlashMessage Success "Event Updated" "Your event has been updated and saved."
      pure $ EventEditRedirectData detailUrl flash

--------------------------------------------------------------------------------
-- Gallery photo handling
--
-- Mirrors the product-image edit pattern: a prepare phase (validate deletions,
-- upload new files — no DB writes) followed by an atomic transaction, then
-- post-transaction cleanup of removed files from storage.

-- | JSON metadata for a single gallery photo from the editor component.
data GalleryMetadata = GalleryMetadata
  { gmId :: Maybe Int64,
    gmSortOrder :: Int64,
    gmAltText :: Text,
    gmCaption :: Text
  }

instance FromJSON GalleryMetadata where
  parseJSON = Aeson.withObject "GalleryMetadata" $ \o ->
    GalleryMetadata
      <$> o .:? "id"
      <*> o .: "sort_order"
      <*> o .: "alt_text"
      <*> o .: "caption"

-- | Prepare gallery updates: validate deletions, upload new files. No DB writes.
--
-- Returns @(failedUploads, imagesToDelete, deleteIds, metaUpdates, newInserts)@
-- where @imagesToDelete@ carries storage paths for post-transaction cleanup.
prepareGalleryUpdates ::
  Events.Id ->
  -- | Event slug for generating filenames.
  Text ->
  EventEditForm ->
  ExceptT HandlerError AppM (Int, [(EventImages.Id, Text)], [EventImages.Id], [(EventImages.Id, Int64, Text, Text)], [EventImages.Insert])
prepareGalleryUpdates eventId eventSlug editForm = do
  -- 1. Validate the deletion list, collecting storage paths (no file I/O yet)
  imagesToDelete <- case eefGalleryDeleted editForm of
    Nothing -> pure []
    Just deletedJson -> do
      deletedIds <- case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 deletedJson) of
        Left err -> throwValidationError ("Invalid deleted photos JSON: " <> Text.pack err)
        Right ids -> pure (ids :: [Int64])
      fmap catMaybes $ forM deletedIds $ \rawId -> do
        let imageId = EventImages.Id rawId
        mImage <- fromRightM throwDatabaseError $ execQuery (EventImages.getById imageId)
        case mImage of
          Nothing -> pure Nothing
          Just image
            | EventImages.eviEventId image /= eventId -> do
                Log.logInfo "Skipping photo deletion: does not belong to event" (Aeson.object ["imageId" Aeson..= rawId])
                pure Nothing
            | otherwise -> pure (Just (imageId, EventImages.eviImagePath image))

  -- 2. Upload new files (I/O — orphaned files are harmless if the transaction fails)
  (failedUploads, metaUpdates, newInserts) <- case eefGalleryData editForm of
    Nothing -> pure (0, [], [])
    Just galleryJson -> do
      metaList <- case Aeson.eitherDecodeStrict (Text.Encoding.encodeUtf8 galleryJson) of
        Left err -> throwValidationError ("Invalid gallery metadata JSON: " <> Text.pack err)
        Right metas -> pure (metas :: [GalleryMetadata])
      prepareGalleryOps eventId eventSlug metaList (eefGalleryFiles editForm)

  let imageIdsToDelete = map fst imagesToDelete
  pure (failedUploads, imagesToDelete, imageIdsToDelete, metaUpdates, newInserts)

-- | Split gallery metadata into updates (existing photos) and inserts (new photos
-- with freshly uploaded files). Uploads happen here; DB writes are deferred.
prepareGalleryOps ::
  Events.Id ->
  Text ->
  [GalleryMetadata] ->
  -- | New photo files (consumed in order for entries without an id).
  [FileData Mem] ->
  ExceptT HandlerError AppM (Int, [(EventImages.Id, Int64, Text, Text)], [EventImages.Insert])
prepareGalleryOps eventId eventSlug metaList newFiles = do
  (remainingFiles, failCount, updates, inserts) <- foldM go (newFiles, 0, [], []) metaList
  unless (null remainingFiles) $
    Log.logInfo "Unused gallery files after processing metadata" (length remainingFiles)
  pure (failCount, reverse updates, reverse inserts)
  where
    go (files, fails, updates, inserts) meta
      | Just existingId <- gmId meta =
          pure
            ( files,
              fails,
              (EventImages.Id existingId, gmSortOrder meta, Sanitize.sanitizePlainText (gmCaption meta), Sanitize.sanitizePlainText (gmAltText meta)) : updates,
              inserts
            )
    go ([], fails, updates, inserts) meta = do
      Log.logInfo "No file available for new gallery metadata entry" (gmSortOrder meta)
      pure ([], fails + 1, updates, inserts)
    go (fileData : restFiles, fails, updates, inserts) meta = do
      storageBackend <- lift $ asks getter
      mAwsEnv <- lift $ asks getter
      uploadResult <- lift $ uploadEventGalleryImage storageBackend mAwsEnv eventSlug fileData
      case uploadResult of
        Left uploadError -> do
          Log.logInfo "Gallery photo upload failed" (Aeson.object ["error" Aeson..= Text.pack (show uploadError)])
          pure (restFiles, fails + 1, updates, inserts)
        Right Nothing ->
          pure (restFiles, fails + 1, updates, inserts)
        Right (Just result) -> do
          let storagePath = Text.pack (uploadResultStoragePath result)
          Log.logInfo "Gallery photo uploaded successfully" (Aeson.object ["path" Aeson..= storagePath])
          let ins =
                EventImages.Insert
                  { EventImages.iiEventId = eventId,
                    EventImages.iiImagePath = storagePath,
                    EventImages.iiCaption = Sanitize.sanitizePlainText (gmCaption meta),
                    EventImages.iiAltText = Sanitize.sanitizePlainText (gmAltText meta),
                    EventImages.iiSortOrder = gmSortOrder meta
                  }
          pure (restFiles, fails, updates, ins : inserts)

-- | Apply all gallery DB mutations atomically.
galleryTransaction ::
  [EventImages.Id] ->
  [(EventImages.Id, Int64, Text, Text)] ->
  [EventImages.Insert] ->
  HT.Transaction ()
galleryTransaction deleteIds metaUpdates newInserts = do
  forM_ deleteIds $ \imageId ->
    void $ HT.statement () (EventImages.deleteImage imageId)
  forM_ metaUpdates $ \(imageId, sortOrder, caption, altText) ->
    HT.statement () (EventImages.updateImageMeta imageId sortOrder caption altText)
  forM_ newInserts $ \ins ->
    void $ HT.statement () (EventImages.insertImage ins)

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesGetLink, hostDashboardGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad (when)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (partitionEithers)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.FileUpload (stripStorageRoot)
import Effects.FileUpload qualified as FileUpload
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Servant.Multipart qualified
import Text.HTML (HTML)
import Text.Read (readMaybe)

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI $ hostDashboardGetLink Nothing

episodesIdGetUrl :: Slug -> Episodes.Id -> Slug -> Links.URI
episodesIdGetUrl showSlug episodeId episodeSlug = Links.linkURI $ episodesGetLink showSlug episodeId episodeSlug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/episodes/:episode_id/:slug/edit"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_id" Episodes.Id
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem EpisodeEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for episode editing
data EpisodeEditForm = EpisodeEditForm
  { eefTitle :: Text,
    eefDescription :: Maybe Text,
    eefStatus :: Text,
    eefTracks :: [TrackInfo],
    -- File uploads (optional - only present if scheduled date is in the future)
    eefAudioFile :: Maybe (FileData Mem),
    eefArtworkFile :: Maybe (FileData Mem)
  }
  deriving (Show)

-- | Track data from form submission (includes ID for existing tracks)
data TrackInfo = TrackInfo
  { tiId :: Maybe EpisodeTrack.Id, -- Existing track ID (Nothing for new tracks)
    tiTrackNumber :: Int64,
    tiTitle :: Text,
    tiArtist :: Text,
    tiAlbum :: Maybe Text,
    tiYear :: Maybe Int64,
    tiDuration :: Maybe Text,
    tiLabel :: Maybe Text,
    tiIsExclusive :: Bool
  }
  deriving (Show)

instance FromMultipart Mem EpisodeEditForm where
  fromMultipart multipartData = do
    title <- lookupInput "title" multipartData
    let description = either (const Nothing) Just (lookupInput "description" multipartData)
    status <- lookupInput "status" multipartData
    -- Parse tracks - using safe parsing that handles multipart correctly
    let tracks = parseTracksFromMultipart multipartData
    -- File lookups - these must be done with lookupFile, not lookupInput
    let audioFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "audio_file" multipartData)
        artworkFile = either (const Nothing) (fileDataToNothing . Just) (lookupFile "artwork_file" multipartData)

    pure
      EpisodeEditForm
        { eefTitle = title,
          eefDescription = emptyToNothing description,
          eefStatus = status,
          eefTracks = tracks,
          eefAudioFile = audioFile,
          eefArtworkFile = artworkFile
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

-- | Parse track data from multipart form fields like tracks[0][title], tracks[0][artist], etc.
-- We try to parse tracks by checking indices starting from 0 until we can't find a title field.
-- This uses the `inputs` list from MultipartData which only contains text inputs, not file data.
parseTracksFromMultipart :: Servant.Multipart.MultipartData Mem -> [TrackInfo]
parseTracksFromMultipart multipartData = parseTracksFromIndex 0
  where
    -- Get all inputs as a map for efficient lookup
    -- This is safer than using lookupInput because we can filter and handle errors
    inputsMap :: [(Text, Text)]
    inputsMap = [(Servant.Multipart.iName inp, Servant.Multipart.iValue inp) | inp <- Servant.Multipart.inputs multipartData]

    lookupInputSafe :: Text -> Maybe Text
    lookupInputSafe key = lookup key inputsMap

    parseTracksFromIndex :: Int -> [TrackInfo]
    parseTracksFromIndex idx =
      let prefix = "tracks[" <> Text.pack (show idx) <> "]"
          titleKey = prefix <> "[title]"
       in -- Try to parse title for this index - if it doesn't exist, we're done
          case lookupInputSafe titleKey of
            Nothing -> [] -- No more tracks
            Just _ ->
              -- Parse this track and remaining tracks
              parseTrack prefix : parseTracksFromIndex (idx + 1)

    parseTrack :: Text -> TrackInfo
    parseTrack prefix =
      let getField field = fromMaybe "" (lookupInputSafe (prefix <> "[" <> field <> "]"))
          getFieldMaybe field = lookupInputSafe (prefix <> "[" <> field <> "]")
          trackIdText = getFieldMaybe "id"
          trackId = trackIdText >>= (fmap EpisodeTrack.Id . readMaybe . Text.unpack)
          trackNumber = getField "track_number"
          title = getField "title"
          artist = getField "artist"
          album = getFieldMaybe "album"
          year = getFieldMaybe "year"
          duration = getFieldMaybe "duration"
          label = getFieldMaybe "label"
          isPremiere = case lookupInputSafe (prefix <> "[is_exclusive_premiere]") of
            Just _ -> True
            Nothing -> False
       in TrackInfo
            { tiId = trackId,
              tiTrackNumber = fromMaybe 1 (readMaybe $ Text.unpack trackNumber),
              tiTitle = title,
              tiArtist = artist,
              tiAlbum = emptyToNothing album,
              tiYear = year >>= readMaybe . Text.unpack,
              tiDuration = emptyToNothing duration,
              tiLabel = emptyToNothing label,
              tiIsExclusive = isPremiere
            }

    emptyToNothing :: Maybe Text -> Maybe Text
    emptyToNothing (Just "") = Nothing
    emptyToNothing x = x

-- | Parse episode status from text
parseStatus :: Text -> Maybe Episodes.Status
parseStatus "draft" = Just Episodes.Draft
parseStatus "published" = Just Episodes.Published
parseStatus "deleted" = Just Episodes.Deleted
parseStatus _ = Nothing

-- | Success template after episode update
successTemplate :: Slug -> Episodes.Id -> Slug -> Lucid.Html ()
successTemplate showSlug episodeId episodeSlug = do
  let epUrl = episodesIdGetUrl showSlug episodeId episodeSlug
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Episode Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your episode has been updated and saved."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{epUrl}|],
          hxGet_ [i|/#{epUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "VIEW EPISODE"
      Lucid.a_
        [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
          hxGet_ [i|/#{hostDashboardGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-6 py-3 font-bold hover:bg-gray-500"
        ]
        "DASHBOARD"

-- | Error templates
unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit episodes."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Episode Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The episode you're trying to update doesn't exist."
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
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit episodes you created, or episodes for shows you host (or with staff permissions)."
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
  Episodes.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  EpisodeEditForm ->
  m (Lucid.Html ())
handler _tracer _showSlug episodeId _urlSlug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized episode edit attempt" episodeId
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the episode to verify it exists and check authorization
      execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
        Left err -> do
          Log.logAttention "getEpisodeById execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No episode with ID: '" <> display episodeId <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just episode) -> do
          -- Fetch show info
          execQuerySpan (Shows.getShowById episode.showId) >>= \case
            Left err -> do
              Log.logAttention "getShowById execution error" (show err)
              renderTemplate hxRequest (Just userMetadata) notFoundTemplate
            Right Nothing -> do
              Log.logInfo "Episode's show not found" episode.showId
              renderTemplate hxRequest (Just userMetadata) notFoundTemplate
            Right (Just showModel) -> do
              -- Check authorization - user must be creator, host, or staff+
              -- Admins don't need explicit host check since they have access to all shows
              if UserMetadata.isAdmin userMetadata.mUserRole
                then updateEpisode hxRequest user userMetadata episode showModel editForm
                else
                  execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id) >>= \case
                    Left err -> do
                      Log.logAttention "isUserHostOfShow execution error" (show err)
                      renderTemplate hxRequest (Just userMetadata) forbiddenTemplate
                    Right True -> updateEpisode hxRequest user userMetadata episode showModel editForm
                    Right False ->
                      if UserMetadata.isStaffOrHigher userMetadata.mUserRole || episode.createdBy == user.mId
                        then updateEpisode hxRequest user userMetadata episode showModel editForm
                        else do
                          Log.logInfo "User attempted to edit episode they don't own" episode.id
                          renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

-- | Check if the episode's scheduled date is in the future (allowing file uploads)
isScheduledInFuture :: UTCTime -> Episodes.Model -> Bool
isScheduledInFuture now episode = case episode.scheduledAt of
  Nothing -> True -- No scheduled date means it's still editable
  Just scheduledAt -> scheduledAt > now

updateEpisode ::
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
  Episodes.Model ->
  Shows.Model ->
  EpisodeEditForm ->
  m (Lucid.Html ())
updateEpisode hxRequest _user userMetadata episode showModel editForm = do
  -- Parse and validate form data
  case parseStatus (eefStatus editForm) of
    Nothing -> do
      Log.logInfo "Invalid status in episode edit form" (eefStatus editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid episode status value.")
    Just _parsedStatus -> do
      -- Sanitize user input to prevent XSS attacks
      let sanitizedTitle = Sanitize.sanitizeTitle (eefTitle editForm)
          sanitizedDescription = maybe "" Sanitize.sanitizeUserContent (eefDescription editForm)

      -- Validate content lengths
      case (Sanitize.validateContentLength 200 sanitizedTitle, Sanitize.validateContentLength 10000 sanitizedDescription) of
        (Left titleError, _) -> do
          let errorMsg = Sanitize.displayContentValidationError titleError
          renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
        (_, Left descError) -> do
          let errorMsg = Sanitize.displayContentValidationError descError
          renderTemplate hxRequest (Just userMetadata) (errorTemplate errorMsg)
        (Right validTitle, Right validDescription) -> do
          -- Update episode metadata (basic update, doesn't change audio/artwork)
          let updateData =
                Episodes.Update
                  { euId = episode.id,
                    euTitle = validTitle,
                    euDescription = Just validDescription
                  }

          -- Update the episode
          execQuerySpan (Episodes.updateEpisode updateData) >>= \case
            Left err -> do
              Log.logInfo "Failed to update episode" (episode.id, show err)
              renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
            Right Nothing -> do
              Log.logInfo "Episode update returned Nothing" episode.id
              renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update episode. Please try again.")
            Right (Just _) -> do
              -- Check if file uploads are allowed (scheduled date is in the future)
              currentTime <- liftIO getCurrentTime
              let allowFileUpload = isScheduledInFuture currentTime episode

              -- Process file uploads if allowed and provided
              fileUpdateResult <-
                if allowFileUpload && (isJust (eefAudioFile editForm) || isJust (eefArtworkFile editForm))
                  then processFileUploads showModel episode editForm
                  else pure $ Right (Nothing, Nothing)

              case fileUpdateResult of
                Left fileErr -> do
                  Log.logInfo "Failed to upload files" (episode.id, fileErr)
                  renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "Episode updated but file upload failed: " <> fileErr)
                Right (mAudioPath, mArtworkPath) -> do
                  -- Update file paths in database if any files were uploaded
                  when (isJust mAudioPath || isJust mArtworkPath) $ do
                    let fileUpdate =
                          Episodes.FileUpdate
                            { efuId = episode.id,
                              efuAudioFilePath = mAudioPath,
                              efuArtworkUrl = mArtworkPath
                            }
                    execQuerySpan (Episodes.updateEpisodeFiles fileUpdate) >>= \case
                      Left err -> Log.logInfo "Failed to update file paths" (episode.id, show err)
                      Right Nothing -> Log.logInfo "File path update returned Nothing" episode.id
                      Right (Just _) -> Log.logInfo "Successfully updated file paths" episode.id

                  -- Update tracks
                  updateTracksResult <- updateTracks episode.id (eefTracks editForm)
                  case updateTracksResult of
                    Left trackErr -> do
                      Log.logInfo "Failed to update tracks" (episode.id, trackErr)
                      renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "Episode updated but track update failed: " <> trackErr)
                    Right _ -> do
                      Log.logInfo "Successfully updated episode and tracks" episode.id
                      renderTemplate hxRequest (Just userMetadata) (successTemplate showModel.slug episode.id episode.slug)

-- | Process file uploads for episode editing
processFileUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  Shows.Model ->
  Episodes.Model ->
  EpisodeEditForm ->
  m (Either Text (Maybe Text, Maybe Text))
processFileUploads showModel episode editForm = do
  -- Process audio file if provided
  audioResult <- case eefAudioFile editForm of
    Nothing -> pure $ Right Nothing
    Just audioFile -> do
      result <- FileUpload.uploadEpisodeAudio showModel.slug episode.slug episode.scheduledAt audioFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload audio file" (Text.pack $ show err)
          pure $ Left "Failed to upload audio file"
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  -- Process artwork file if provided
  artworkResult <- case eefArtworkFile editForm of
    Nothing -> pure $ Right Nothing
    Just artworkFile -> do
      result <- FileUpload.uploadEpisodeArtwork showModel.slug episode.slug episode.scheduledAt artworkFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right _audioPath, Left artworkErr) -> pure $ Left artworkErr
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

--------------------------------------------------------------------------------
-- Track Update Logic

-- | Update tracks for an episode
-- Strategy: For each track in the form:
--   - If it has an ID, update the existing track
--   - If it has no ID, insert a new track
-- Note: Removed tracks are handled by the frontend (they won't be in the form submission)
updateTracks ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  Episodes.Id ->
  [TrackInfo] ->
  m (Either Text [EpisodeTrack.Id])
updateTracks episodeId tracks = do
  results <- mapM (processTrack episodeId) tracks
  let (errors, trackIds) = partitionEithers results
  if null errors
    then pure $ Right trackIds
    else do
      Log.logInfo "Some tracks failed to process" errors
      pure $ Left "Failed to process some tracks"

-- | Process a single track (update existing or insert new)
processTrack ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  Episodes.Id ->
  TrackInfo ->
  m (Either Text EpisodeTrack.Id)
processTrack episodeId track = do
  let trackInsert =
        EpisodeTrack.Insert
          { EpisodeTrack.etiEpisodeId = episodeId,
            EpisodeTrack.etiTrackNumber = tiTrackNumber track,
            EpisodeTrack.etiTitle = tiTitle track,
            EpisodeTrack.etiArtist = tiArtist track,
            EpisodeTrack.etiAlbum = tiAlbum track,
            EpisodeTrack.etiYear = tiYear track,
            EpisodeTrack.etiDuration = tiDuration track,
            EpisodeTrack.etiLabel = tiLabel track,
            EpisodeTrack.etiIsExclusivePremiere = tiIsExclusive track
          }

  case tiId track of
    -- Update existing track
    Just trackId -> do
      execQuerySpan (Episodes.updateEpisodeTrack trackId trackInsert) >>= \case
        Left err -> do
          Log.logInfo "Failed to update track" (trackId, show err)
          pure $ Left "Failed to update track"
        Right Nothing -> do
          Log.logInfo "Track update returned Nothing" trackId
          pure $ Left "Track not found"
        Right (Just updatedId) -> pure $ Right updatedId
    -- Insert new track
    Nothing -> do
      execQuerySpan (Episodes.insertEpisodeTrack trackInsert) >>= \case
        Left err -> do
          Log.logInfo "Failed to insert track" (show err)
          pure $ Left "Failed to insert track"
        Right trackId -> pure $ Right trackId

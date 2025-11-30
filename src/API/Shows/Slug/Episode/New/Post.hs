{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.New.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesGetLink, userLoginGetLink)
import API.Shows.Slug.Episode.Get.Templates.Page qualified as DetailPage
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (UTCTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale, parseTimeM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.FileUpload (uploadResultStoragePath)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.Slug qualified as Slug
import Effects.ContentSanitization qualified as Sanitize
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTracks
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
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
import OrphanInstances.OneRow ()
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Servant.Multipart (FileData, FromMultipart, Mem, MultipartForm, fdFileName, fromMultipart, lookupFile, lookupInput)
import Text.HTML (HTML)
import Text.Read (readMaybe)
import Utils (partitionEithers)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/episodes/new"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem EpisodeUploadForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

--------------------------------------------------------------------------------
-- Form Data Types

data TrackInfo = TrackInfo
  { tiTitle :: Text,
    tiArtist :: Text,
    tiAlbum :: Maybe Text,
    tiYear :: Maybe Int64,
    tiDuration :: Maybe Text,
    tiLabel :: Maybe Text,
    tiIsExclusive :: Bool
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

data EpisodeUploadForm = EpisodeUploadForm
  { -- Show and scheduling
    eufId :: Text,
    eufScheduledDate :: Maybe Text,
    -- Episode metadata
    eufTitle :: Text,
    eufDescription :: Text,
    eufTags :: Maybe Text,
    eufDurationSeconds :: Maybe Text, -- Duration from browser audio detection
    -- Publishing status
    eufStatus :: Text, -- "draft" or "published"
    -- Track data (JSON encoded)
    eufTracksJson :: Maybe Text,
    -- File uploads
    eufAudioFile :: Maybe (FileData Mem),
    eufArtworkFile :: Maybe (FileData Mem)
  }
  deriving stock (Show, Generic, Eq)

instance FromMultipart Mem EpisodeUploadForm where
  fromMultipart multipartData =
    EpisodeUploadForm
      <$> lookupInput "show_id" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "scheduled_date" multipartData))
      <*> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) Just (lookupInput "duration_seconds" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (either (const Nothing) Just (lookupInput "tracks_json" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "audio_file" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "artwork_file" multipartData))
    where
      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

--------------------------------------------------------------------------------

-- | URL helper for login page
userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

-- | Error template for upload failures
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Upload Failed"
    Lucid.p_ [Lucid.class_ "mb-6 text-red-700"] $ Lucid.toHtml errorMsg

-- | Template for login required error
loginRequiredTemplate :: Lucid.Html ()
loginRequiredTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-4"] "You must be logged in to upload episodes."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
      ]
      "LOGIN"

-- | Template for permission denied error
permissionDeniedTemplate :: Lucid.Html ()
permissionDeniedTemplate =
  Lucid.div_ [Lucid.class_ "text-center p-8"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Permission Denied"
    Lucid.p_ [Lucid.class_ "mb-4"] "You are not authorized to upload episodes for this show."

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
  EpisodeUploadForm ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer showSlug cookie (foldHxReq -> hxRequest) form = do
  getUserInfo cookie >>= \case
    Nothing ->
      Servant.noHeader <$> renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (user, userMetadata) -> do
      -- Verify show exists and user is authorized
      showResult <- execQuerySpan (Shows.getShowBySlug showSlug)
      case showResult of
        Left _err -> do
          Log.logInfo "Failed to fetch show" showSlug
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to load show information.")
        Right Nothing -> do
          Log.logInfo "Show not found" showSlug
          Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Show not found.")
        Right (Just showModel) -> do
          -- Verify user is host (admins can upload to any show)
          isAuthorized <-
            if UserMetadata.isAdmin userMetadata.mUserRole
              then pure (Right True) -- Admins always authorized
              else execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id)
          case isAuthorized of
            Left _err -> do
              Log.logInfo "Failed to check host permissions" showSlug
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to verify permissions.")
            Right True | not (UserMetadata.isSuspended userMetadata) -> do
              processEpisodeUpload userMetadata user form >>= \case
                Left err -> do
                  Log.logInfo "Episode upload failed" err
                  Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate err)
                Right episodeId -> do
                  Log.logInfo ("Episode uploaded successfully: " <> display episodeId) ()
                  handleUploadSuccess hxRequest userMetadata showModel episodeId
            Right _ -> do
              Log.logInfo "User is not host of show" (user.mId, showSlug)
              Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) permissionDeniedTemplate

-- | Handle successful upload by redirecting to episode page with banner
handleUploadSuccess ::
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
  Shows.Model ->
  Episodes.Id ->
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handleUploadSuccess hxRequest userMetadata showModel episodeId = do
  -- Fetch the created episode
  execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
    Right (Just episode) -> do
      -- Fetch tracks for the episode
      tracksResult <- execQuerySpan (Episodes.getTracksForEpisode episodeId)
      let tracks = fromRight [] tracksResult
          detailUrl = Links.linkURI $ episodesGetLink showModel.slug episodeId episode.slug
          banner = renderBanner Success "Episode Uploaded" "Your episode has been uploaded successfully."
      html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
        IsHxRequest -> do
          DetailPage.template showModel episode tracks
          banner
        IsNotHxRequest -> do
          banner
          DetailPage.template showModel episode tracks
      pure $ Servant.addHeader [i|/#{detailUrl}|] html
    _ -> do
      Log.logInfo_ "Created episode but failed to retrieve it"
      Servant.noHeader <$> renderTemplate hxRequest (Just userMetadata) (errorTemplate "Episode was created but there was an error displaying the confirmation.")

-- | Process episode upload form
processEpisodeUpload ::
  ( MonadIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    Has HSQL.Pool.Pool env,
    Has Tracer env
  ) =>
  UserMetadata.Model ->
  User.Model ->
  EpisodeUploadForm ->
  m (Either Text Episodes.Id)
processEpisodeUpload userMetadata user form = do
  _currentTime <- liftIO getCurrentTime

  -- Debug logging for duration
  Log.logInfo "Duration from form" (Text.pack $ show $ eufDurationSeconds form)

  -- Parse show ID first
  case readMaybe (Text.unpack (eufId form)) of
    Nothing -> pure $ Left "Invalid show ID"
    Just showIdInt -> do
      let showId = Shows.Id showIdInt

      -- Look up show to get slug
      showResult <- execQuerySpan (Shows.getShowById showId)

      case showResult of
        Left _err -> pure $ Left "Database error looking up show"
        Right Nothing -> pure $ Left "Show not found"
        Right (Just showModel) -> do
          -- Parse remaining form data
          case parseFormDataWithShow showModel.id showModel.slug form of
            Left validationError -> pure $ Left $ Sanitize.displayContentValidationError validationError
            Right episodeData -> do
              -- Verify user is host of the show (admins can upload to any show)
              isAuthorized <-
                if UserMetadata.isAdmin userMetadata.mUserRole
                  then pure (Right True) -- Admins always authorized
                  else execQuerySpan (ShowHost.isUserHostOfShow (User.mId user) (Shows.Id episodeData.showId))

              case isAuthorized of
                Left _err -> pure $ Left "Database error checking host permissions"
                Right isHost ->
                  if not isHost
                    then pure $ Left "You are not authorized to create episodes for this show"
                    else do
                      -- Generate episode slug for filename
                      let episodeSlug = Slug.mkSlug episodeData.title
                      -- Handle file uploads (pass scheduled date for file organization)
                      uploadResults <- processFileUploads episodeData.showSlug episodeSlug episodeData.scheduledAt episodeData.status (eufAudioFile form) (eufArtworkFile form)

                      case uploadResults of
                        Left uploadErr -> pure $ Left uploadErr
                        Right (audioPath, artworkPath) -> do
                          -- Create episode insert
                          let episodeInsert =
                                Episodes.Insert
                                  { Episodes.eiId = Shows.Id episodeData.showId,
                                    Episodes.eiTitle = episodeData.title,
                                    Episodes.eiSlug = Slug.mkSlug episodeData.title,
                                    Episodes.eiDescription = episodeData.description,
                                    Episodes.eiAudioFilePath = audioPath,
                                    Episodes.eiAudioFileSize = Nothing, -- TODO: Get from upload
                                    Episodes.eiAudioMimeType = Nothing, -- TODO: Get from upload
                                    Episodes.eiDurationSeconds = episodeData.durationSeconds,
                                    Episodes.eiArtworkUrl = artworkPath,
                                    Episodes.eiScheduledAt = episodeData.scheduledAt,
                                    Episodes.eiStatus = episodeData.status,
                                    Episodes.eiCreatedBy = User.mId user
                                  }

                          -- Insert episode
                          episodeResult <- execQuerySpan (Episodes.insertEpisode episodeInsert)

                          case episodeResult of
                            Left err -> do
                              Log.logInfo "Failed to insert episode" (Text.pack $ show err)
                              pure $ Left "Failed to create episode"
                            Right episodeId -> do
                              -- Insert tracks if provided
                              _ <- insertTracks episodeId episodeData.tracks
                              pure $ Right episodeId

-- | Parse form data into structured format with show info
parseFormDataWithShow :: Shows.Id -> Slug -> EpisodeUploadForm -> Either Sanitize.ContentValidationError ParsedEpisodeData
parseFormDataWithShow (Shows.Id showId) showSlug form = do
  -- Parse scheduled timestamp (now receives full UTC timestamp from form)
  scheduledAt <- case eufScheduledDate form of
    Nothing -> Right Nothing
    Just timestampStr ->
      -- Try parsing as full UTCTime (format: "YYYY-MM-DD HH:MM:SS.ssssss UTC")
      case parseTimeM True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q %Z" (Text.unpack timestampStr) of
        Just timestamp -> Right (Just timestamp)
        Nothing -> Left $ Sanitize.ContentInvalid $ "Invalid scheduled timestamp format: " <> timestampStr

  -- Determine episode status
  status <- case eufStatus form of
    "draft" -> Right Episodes.Draft
    "published" -> Right Episodes.Published
    _ -> Left $ Sanitize.ContentInvalid "Invalid status value"

  -- Sanitize and validate episode metadata
  let sanitizedTitle = Sanitize.sanitizeTitle (eufTitle form)
      sanitizedDescription = Sanitize.sanitizeUserContent (eufDescription form)

  -- Validate content lengths
  validTitle <- Sanitize.validateContentLength 200 sanitizedTitle
  validDescription <- Sanitize.validateContentLength 10000 sanitizedDescription

  -- Parse and sanitize tracks JSON
  tracks <- case eufTracksJson form of
    Nothing -> Right []
    Just tracksJson -> case Aeson.eitherDecodeStrict (Text.encodeUtf8 tracksJson) of
      Left err -> Left $ Sanitize.ContentInvalid $ "Invalid tracks JSON: " <> Text.pack err
      Right trackList -> Right $ sanitizeTrackList trackList

  -- Parse duration seconds (with debugging)
  let durationSeconds = case eufDurationSeconds form of
        Nothing -> Nothing
        Just "" -> Nothing
        Just durStr -> readMaybe (Text.unpack durStr)

  Right $
    ParsedEpisodeData
      { showId = showId,
        showSlug = showSlug,
        title = validTitle,
        description = Just validDescription,
        scheduledAt = scheduledAt,
        status = status,
        tracks = tracks,
        durationSeconds = durationSeconds
      }

-- | Sanitize track list by sanitizing all text fields
sanitizeTrackList :: [TrackInfo] -> [TrackInfo]
sanitizeTrackList = map sanitizeTrack
  where
    sanitizeTrack track =
      TrackInfo
        { tiTitle = Sanitize.sanitizePlainText (tiTitle track),
          tiArtist = Sanitize.sanitizePlainText (tiArtist track),
          tiAlbum = Sanitize.sanitizePlainText <$> tiAlbum track,
          tiYear = tiYear track, -- Keep numeric values as-is
          tiDuration = Sanitize.sanitizePlainText <$> tiDuration track,
          tiLabel = Sanitize.sanitizePlainText <$> tiLabel track,
          tiIsExclusive = tiIsExclusive track -- Keep boolean as-is
        }

data ParsedEpisodeData = ParsedEpisodeData
  { showId :: Int64,
    showSlug :: Slug,
    title :: Text,
    description :: Maybe Text,
    scheduledAt :: Maybe UTCTime,
    status :: Episodes.Status,
    tracks :: [TrackInfo],
    durationSeconds :: Maybe Int64
  }
  deriving stock (Show, Eq)

-- | Process file uploads (audio and artwork)
processFileUploads ::
  ( MonadIO m,
    Log.MonadLog m
  ) =>
  -- | Show slug
  Slug ->
  -- | Episode slug (for filename)
  Slug ->
  -- | Scheduled date (for file organization)
  Maybe UTCTime ->
  -- | Episode status (audio only required for published)
  Episodes.Status ->
  -- | Audio file
  Maybe (FileData Mem) ->
  -- | Artwork file
  Maybe (FileData Mem) ->
  -- | (audioPath, artworkPath)
  m (Either Text (Maybe Text, Maybe Text))
processFileUploads showSlug episodeSlug mScheduledDate status mAudioFile mArtworkFile = do
  -- Process main audio file (required for published, optional for draft)
  audioResult <- case mAudioFile of
    Nothing ->
      case status of
        Episodes.Draft -> pure $ Right Nothing -- Audio optional for drafts
        _ -> pure $ Left "Audio file is required for published episodes"
    Just audioFile -> do
      result <- FileUpload.uploadEpisodeAudio showSlug episodeSlug mScheduledDate audioFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload audio file" (Text.pack $ show err)
          pure $ Left "Failed to upload audio file"
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  -- Process artwork file (optional)
  artworkResult <- case mArtworkFile of
    Nothing -> pure $ Right Nothing
    Just artworkFile -> do
      result <- FileUpload.uploadEpisodeArtwork showSlug episodeSlug mScheduledDate artworkFile
      case result of
        Left err -> do
          Log.logInfo "Failed to upload artwork file" (Text.pack $ show err)
          pure $ Left $ Text.pack $ show err -- Invalid file provided, reject entire operation
        Right uploadResult ->
          pure $ Right $ Just $ stripStorageRoot $ uploadResultStoragePath uploadResult

  case (audioResult, artworkResult) of
    (Left audioErr, _) -> pure $ Left audioErr
    (Right audioPath, Left _artworkErr) -> pure $ Right (audioPath, Nothing)
    (Right audioPath, Right artworkPath) -> pure $ Right (audioPath, artworkPath)

-- | Insert track listings for episode
insertTracks ::
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
  m (Either Text [EpisodeTracks.Id])
insertTracks episodeId tracks = do
  results <- mapM (insertTrack episodeId) (zip [1 ..] tracks)
  let (errors, trackIds) = partitionEithers results

  if null errors
    then pure $ Right trackIds
    else do
      Log.logInfo "Some tracks failed to insert" errors
      pure $ Left "Failed to insert some tracks"
  where
    insertTrack ::
      ( MonadIO m,
        MonadDB m,
        Log.MonadLog m,
        MonadReader env m,
        MonadUnliftIO m,
        Has HSQL.Pool.Pool env,
        Has Tracer env
      ) =>
      Episodes.Id ->
      (Int64, TrackInfo) ->
      m (Either Text EpisodeTracks.Id)
    insertTrack epId (trackNum, track) = do
      let trackInsert =
            EpisodeTracks.Insert
              { EpisodeTracks.etiEpisodeId = epId,
                EpisodeTracks.etiTrackNumber = trackNum,
                EpisodeTracks.etiTitle = tiTitle track,
                EpisodeTracks.etiArtist = tiArtist track,
                EpisodeTracks.etiAlbum = tiAlbum track,
                EpisodeTracks.etiYear = tiYear track,
                EpisodeTracks.etiDuration = tiDuration track,
                EpisodeTracks.etiLabel = tiLabel track,
                EpisodeTracks.etiIsExclusivePremiere = tiIsExclusive track
              }

      result <- execQuerySpan (Episodes.insertEpisodeTrack trackInsert)
      case result of
        Left err -> pure $ Left $ "Failed to insert track: " <> Text.pack (show err)
        Right trackId -> pure $ Right trackId

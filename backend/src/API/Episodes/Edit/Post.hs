{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodesGetLink, hostDashboardGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (partitionEithers)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)
import Text.Read (readMaybe)
import Web.FormUrlEncoded (Form, FromForm)
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

episodesIdGetUrl :: Slug -> Slug -> Links.URI
episodesIdGetUrl showSlug episodeSlug = Links.linkURI $ episodesGetLink showSlug episodeSlug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:show_slug/episodes/:episode_slug/edit"
    ( "shows"
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] EpisodeEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for episode editing
data EpisodeEditForm = EpisodeEditForm
  { eefTitle :: Text,
    eefDescription :: Maybe Text,
    eefStatus :: Text,
    eefTracks :: [TrackInfo]
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

instance FromForm EpisodeEditForm where
  fromForm form = do
    title <- Form.parseUnique "title" form
    description <- Form.parseMaybe "description" form
    status <- Form.parseUnique "status" form
    tracks <- parseTracksFromForm form

    pure
      EpisodeEditForm
        { eefTitle = title,
          eefDescription = emptyToNothing description,
          eefStatus = status,
          eefTracks = tracks
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse track data from form fields like tracks[0][title], tracks[0][artist], etc.
-- We try to parse tracks by checking indices starting from 0 until we can't find a title field
parseTracksFromForm :: Form -> Either Text [TrackInfo]
parseTracksFromForm form = parseTracksFromIndex 0
  where
    parseTracksFromIndex :: Int -> Either Text [TrackInfo]
    parseTracksFromIndex idx = do
      let prefix = "tracks[" <> Text.pack (show idx) <> "]"
          titleKey = prefix <> "[title]"

      -- Try to parse title for this index - if it doesn't exist, we're done
      case (Form.parseUnique titleKey form :: Either Text Text) of
        Left _ -> Right [] -- No more tracks
        Right _ -> do
          -- Parse this track
          track <- parseTrack form idx
          -- Parse remaining tracks
          remainingTracks <- parseTracksFromIndex (idx + 1)
          Right (track : remainingTracks)

    parseTrack :: Form -> Int -> Either Text TrackInfo
    parseTrack f idx = do
      let prefix = "tracks[" <> Text.pack (show idx) <> "]"
          getField field = Form.parseUnique (prefix <> "[" <> field <> "]") f
          getFieldMaybe field = Right $ either (const Nothing) Just (getField field)

      trackId <- getFieldMaybe "id"
      trackNumber <- getField "track_number"
      title <- getField "title"
      artist <- getField "artist"
      album <- getFieldMaybe "album"
      year <- getFieldMaybe "year"
      duration <- getFieldMaybe "duration"
      label <- getFieldMaybe "label"
      isPremiere <- Right $ either (const False) (const True) (getField "is_exclusive_premiere")

      pure
        TrackInfo
          { tiId = trackId >>= (fmap EpisodeTrack.Id . readMaybe . Text.unpack),
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
parseStatus "scheduled" = Just Episodes.Scheduled
parseStatus "published" = Just Episodes.Published
parseStatus "archived" = Just Episodes.Archived
parseStatus _ = Nothing

-- | Success template after episode update
successTemplate :: Slug -> Slug -> Lucid.Html ()
successTemplate showSlug episodeSlug = do
  let epUrl = episodesIdGetUrl showSlug episodeSlug
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  EpisodeEditForm ->
  m (Lucid.Html ())
handler _tracer showSlug episodeSlug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized episode edit attempt" (showSlug, episodeSlug)
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the episode to verify it exists and check authorization
      execQuerySpan (Episodes.getEpisodeBySlug showSlug episodeSlug) >>= \case
        Left err -> do
          Log.logAttention "getEpisodeBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No episode with slugs: '" <> display showSlug <> "' / '" <> display episodeSlug <> "'"
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
              execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id) >>= \case
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

updateEpisode ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m,
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
      -- Update episode metadata (basic update, doesn't change audio/artwork)
      -- Note: Status changes could be handled here if needed
      let updateData =
            Episodes.Update
              { euId = episode.id,
                euTitle = eefTitle editForm,
                euDescription = eefDescription editForm
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
          -- Update tracks
          updateTracksResult <- updateTracks episode.id (eefTracks editForm)
          case updateTracksResult of
            Left trackErr -> do
              Log.logInfo "Failed to update tracks" (episode.id, trackErr)
              renderTemplate hxRequest (Just userMetadata) (errorTemplate $ "Episode updated but track update failed: " <> trackErr)
            Right _ -> do
              Log.logInfo "Successfully updated episode and tracks" episode.id
              renderTemplate hxRequest (Just userMetadata) (successTemplate showModel.slug episode.slug)

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

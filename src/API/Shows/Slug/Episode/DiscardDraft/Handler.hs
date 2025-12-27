{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Episode.DiscardDraft.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Get.Templates.EpisodeRow (renderEpisodeTableRow)
import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan, execTransactionSpan)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as Txn
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Handler for discarding draft episodes.
--
-- Only draft episodes can be discarded. Hosts can discard drafts for shows
-- they are assigned to, or drafts they created. This performs a hard delete.
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
  Episodes.EpisodeNumber ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer showSlug episodeNumber cookie = do
  -- Fetch the show by slug
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> do
      Log.logInfo "Discard draft failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      pure $ renderBanner Error "Discard Failed" "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Discard draft failed: Show not found" (Aeson.object ["showSlug" .= showSlug])
      pure $ renderBanner Error "Discard Failed" "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          pure $ renderBanner Error "Discard Failed" "You must be logged in to discard episodes."
        Just (user, userMeta) -> do
          execQuerySpan (Episodes.getEpisodeByShowAndNumber showSlug episodeNumber) >>= \case
            Left err -> do
              Log.logInfo "Discard draft failed: Failed to fetch episode" (Aeson.object ["error" .= show err])
              pure $ renderBanner Error "Discard Failed" "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Discard draft failed: Episode not found" (Aeson.object ["episodeNumber" .= episodeNumber])
              pure $ renderBanner Error "Discard Failed" "Episode not found."
            Right (Just episode) -> do
              -- Check that episode is a draft
              if episode.status /= Episodes.Draft
                then do
                  Log.logInfo "Discard draft failed: Episode is not a draft" (Aeson.object ["episodeId" .= episode.id, "status" .= show episode.status])
                  pure $ renderErrorWithRow userMeta showModel episode "Only draft episodes can be discarded. Published episodes must be archived by staff."
                else do
                  -- Check authorization: staff, creator, or host
                  let isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
                      isCreator = episode.createdBy == user.mId

                  isHost <- if isStaff || isCreator then pure True else checkIfHost user episode

                  if (isStaff || isCreator || isHost) && not (UserMetadata.isSuspended userMeta)
                    then hardDeleteEpisode userMeta showModel episode
                    else do
                      Log.logInfo "Discard draft failed: Not authorized" (Aeson.object ["userId" .= user.mId, "episodeId" .= episode.id])
                      pure $ renderErrorWithRow userMeta showModel episode "You don't have permission to discard this episode."

hardDeleteEpisode ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  UserMetadata.Model ->
  Shows.Model ->
  Episodes.Model ->
  m (Lucid.Html ())
hardDeleteEpisode userMeta showModel episode = do
  -- Delete tracks and episode in a single transaction
  execTransactionSpan (deleteEpisodeTransaction episode.id) >>= \case
    Left err -> do
      Log.logInfo "Discard draft failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Failed to discard episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Discard draft failed: Episode not found during delete" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorWithRow userMeta showModel episode "Episode not found during discard operation."
    Right (Just _) -> do
      Log.logInfo "Draft episode discarded successfully" (Aeson.object ["episodeId" .= episode.id])
      -- Return empty response to remove the row
      pure emptyResponse

-- | Render an error banner AND the episode row (to prevent row removal on error)
renderErrorWithRow :: UserMetadata.Model -> Shows.Model -> Episodes.Model -> Text -> Lucid.Html ()
renderErrorWithRow userMeta showModel episode errorMsg = do
  renderEpisodeTableRow userMeta showModel episode
  renderBanner Error "Discard Failed" errorMsg

-- | Empty response for successful deletes (row is removed by HTMX)
emptyResponse :: Lucid.Html ()
emptyResponse = ""

-- | Transaction to delete episode tracks and then the episode itself.
deleteEpisodeTransaction :: Episodes.Id -> Txn.Transaction (Maybe Episodes.Id)
deleteEpisodeTransaction episodeId = do
  -- First delete all tracks for this episode
  _ <- Txn.statement () (EpisodeTrack.deleteAllTracksForEpisode episodeId)
  -- Then hard delete the episode
  Txn.statement () (Episodes.hardDeleteEpisode episodeId)

checkIfHost ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  User.Model ->
  Episodes.Model ->
  m Bool
checkIfHost user episode = do
  result <- execQuerySpan (Episodes.isUserHostOfEpisodeShow user.mId episode.id)
  case result of
    Left _ -> pure False
    Right authorized -> pure authorized

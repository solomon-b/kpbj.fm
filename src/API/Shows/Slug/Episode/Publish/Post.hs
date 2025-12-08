{-# LANGUAGE OverloadedRecordDot #-}

module API.Shows.Slug.Episode.Publish.Post where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Publish.Post.Templates.Response (renderErrorBannerWithRow, renderSuccessRow)
import App.Common (getUserInfo)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Time (UTCTime, getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

-- | Check if the episode's scheduled date has passed
isScheduledInPast :: UTCTime -> Episodes.Model -> Bool
isScheduledInPast now episode = case episode.scheduledAt of
  Nothing -> False -- No scheduled date means it hasn't "passed"
  Just scheduledAt -> scheduledAt <= now

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
  m (Lucid.Html ())
handler _tracer showSlug episodeId _episodeSlug cookie = do
  -- Fetch the show by slug
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left err -> do
      Log.logInfo "Publish failed: Failed to fetch show" (Aeson.object ["error" .= show err])
      pure $ renderBanner Error "Publish Failed" "Database error. Please try again or contact support."
    Right Nothing -> do
      Log.logInfo "Publish failed: Show not found" (Aeson.object ["showSlug" .= showSlug])
      pure $ renderBanner Error "Publish Failed" "Show not found."
    Right (Just showModel) -> do
      getUserInfo cookie >>= \case
        Nothing -> do
          Log.logInfo_ "No user session"
          pure $ renderBanner Error "Publish Failed" "You must be logged in to publish episodes."
        Just (user, userMeta) -> do
          execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
            Left err -> do
              Log.logInfo "Publish failed: Failed to fetch episode" (Aeson.object ["error" .= show err])
              pure $ renderBanner Error "Publish Failed" "Database error. Please try again or contact support."
            Right Nothing -> do
              Log.logInfo "Publish failed: Episode not found" (Aeson.object ["episodeId" .= episodeId])
              pure $ renderBanner Error "Publish Failed" "Episode not found."
            Right (Just episode) -> do
              -- Check if episode is already published
              if episode.status == Episodes.Published
                then do
                  Log.logInfo "Publish failed: Episode already published" (Aeson.object ["episodeId" .= episode.id])
                  pure $ renderErrorBannerWithRow showModel episode "This episode is already published."
                else do
                  -- Check authorization: staff, creator, or host
                  let isStaff = UserMetadata.isStaffOrHigher userMeta.mUserRole
                      isCreator = episode.createdBy == user.mId

                  isHost <- if isStaff || isCreator then pure True else checkIfHost user episode

                  -- Check if scheduled date has passed - only staff can publish past episodes
                  currentTime <- liftIO getCurrentTime
                  let isPast = isScheduledInPast currentTime episode

                  if isPast && not isStaff
                    then do
                      Log.logInfo "Publish failed: Past episode, user not staff" (Aeson.object ["userId" .= user.mId, "episodeId" .= episode.id])
                      pure $ renderErrorBannerWithRow showModel episode "This episode's scheduled date has passed. Only staff or admin users can publish past episodes."
                    else
                      if isStaff || ((isCreator || isHost) && not (UserMetadata.isSuspended userMeta))
                        then publishEpisode showModel episode
                        else do
                          Log.logInfo "Publish failed: Not authorized" (Aeson.object ["userId" .= user.mId, "episodeId" .= episode.id])
                          pure $ renderErrorBannerWithRow showModel episode "You don't have permission to publish this episode."

publishEpisode ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Shows.Model ->
  Episodes.Model ->
  m (Lucid.Html ())
publishEpisode showModel episode = do
  execQuerySpan (Episodes.publishEpisode episode.id) >>= \case
    Left err -> do
      Log.logInfo "Publish failed: Database error" (Aeson.object ["error" .= show err, "episodeId" .= episode.id])
      pure $ renderErrorBannerWithRow showModel episode "Failed to publish episode due to a database error."
    Right Nothing -> do
      Log.logInfo "Publish failed: Episode not found during publish" (Aeson.object ["episodeId" .= episode.id])
      pure $ renderErrorBannerWithRow showModel episode "Episode not found during publish operation."
    Right (Just _) -> do
      Log.logInfo "Episode published successfully" (Aeson.object ["episodeId" .= episode.id])
      -- Return the updated row with Published status
      let publishedEpisode = episode {Episodes.status = Episodes.Published}
      pure $ renderSuccessRow showModel publishedEpisode

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

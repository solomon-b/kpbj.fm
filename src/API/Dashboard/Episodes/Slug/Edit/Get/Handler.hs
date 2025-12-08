{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Episodes.Slug.Edit.Get.Templates.Form (template)
import API.Get.Templates qualified as HomeTemplate
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Component.Redirect (redirectTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

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
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text, Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug episodeId urlSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      let banner = renderBanner Warning "Login Required" "Please login to edit episodes."
      html <- renderTemplate hxRequest Nothing $ case hxRequest of
        IsHxRequest -> HomeTemplate.template <> banner
        IsNotHxRequest -> banner <> HomeTemplate.template
      pure $ Servant.addHeader "/" $ Servant.noHeader html
    Just (user, userMetadata) -> do
      -- NOTE: Experimental use of a 'HT.Transaction' here. We aren't applying
      -- these globally yet due to difficulty logging the raw sql. However, I
      -- want a reference in the repo in case we end up needing a transaction
      -- somewhere.
      mResult <- execTransactionSpan $ runMaybeT $ do
        episode <- MaybeT $ HT.statement () (Episodes.getEpisodeById episodeId)
        showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
        tracks <- lift $ HT.statement () (Episodes.getTracksForEpisode episode.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
        MaybeT $ pure $ Just (episode, showResult, tracks, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getEpisodeById execution error" (show err)
          let banner = renderBanner Warning "Episode Not Found" "The episode you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> HomeTemplate.template <> banner
            IsNotHxRequest -> banner <> HomeTemplate.template
          pure $ Servant.addHeader "/" $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo_ $ "No episode : '" <> display episodeId <> "'"
          let banner = renderBanner Warning "Episode Not Found" "The episode you're trying to edit doesn't exist."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> HomeTemplate.template <> banner
            IsNotHxRequest -> banner <> HomeTemplate.template
          pure $ Servant.addHeader "/" $ Servant.noHeader html
        Right (Just (episode, showModel, tracks, isHost)) -> do
          let canonicalSlug = episode.slug
              showSlugText = display showSlug
              episodeIdText = display episodeId
              slugText = display canonicalSlug
              canonicalUrl = [i|/dashboard/episodes/#{showSlugText}/#{episodeIdText}/#{slugText}/edit|]

          if matchSlug canonicalSlug (Just urlSlug)
            then
              if episode.createdBy == user.mId || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then do
                  Log.logInfo "Authorized user accessing episode edit form" episode.id
                  currentTime <- liftIO getCurrentTime
                  Log.logInfo "Episode scheduled_at" (show episode.scheduledAt)
                  Log.logInfo "Current time" (show currentTime)
                  Log.logInfo "Is scheduled in future?" (show $ case episode.scheduledAt of Nothing -> True; Just s -> s > currentTime)
                  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                      editTemplate = template currentTime showModel episode tracks userMetadata isStaff
                  html <- renderTemplate hxRequest (Just userMetadata) editTemplate
                  pure $ Servant.noHeader $ Servant.noHeader html
                else do
                  Log.logInfo "User tried to edit episode they don't own" episode.id
                  let banner = renderBanner Error "Access Denied" "You can only edit your own episodes."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> HomeTemplate.template <> banner
                    IsNotHxRequest -> banner <> HomeTemplate.template
                  pure $ Servant.addHeader "/" $ Servant.noHeader html
            else do
              Log.logInfo "Redirecting to canonical episode edit URL" canonicalUrl
              html <- renderTemplate hxRequest (Just userMetadata) (redirectTemplate canonicalUrl)
              pure $ Servant.noHeader $ Servant.addHeader canonicalUrl html

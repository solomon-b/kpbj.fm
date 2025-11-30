{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Edit.Get where

--------------------------------------------------------------------------------

import API.Get.Templates qualified as HomeTemplate
import API.Shows.Slug.Edit.Get.Templates.Form (schedulesToJson, template)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Bool (bool)
import Data.Has (Has)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug/edit"
    ( "shows"
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

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
  m (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to show edit" ()
      let banner = renderBanner Error "Not Logged In" "You must be logged in to edit a show."
      html <- renderTemplate hxRequest Nothing $ case hxRequest of
        IsHxRequest -> HomeTemplate.template <> banner
        IsNotHxRequest -> banner <> HomeTemplate.template
      pure $ Servant.addHeader "/" html
    Just (user, userMetadata) -> do
      execQuerySpan (ShowHost.isUserHostOfShowSlug user.mId slug) >>= \case
        Left err -> do
          Log.logAttention "isUserHostOfShow execution error" (show err)
          let banner = renderBanner Error "Not Authorized" "You don't have permission to edit this show."
          html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
            IsHxRequest -> HomeTemplate.template <> banner
            IsNotHxRequest -> banner <> HomeTemplate.template
          pure $ Servant.addHeader "/" html
        Right isHost -> do
          let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
          if not (isHost || isStaff)
            then do
              let banner = renderBanner Error "Not Authorized" "You don't have permission to edit this show."
              html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                IsHxRequest -> HomeTemplate.template <> banner
                IsNotHxRequest -> banner <> HomeTemplate.template
              pure $ Servant.addHeader "/" html
            else
              execQuerySpan (Shows.getShowBySlug slug) >>= \case
                Left err -> do
                  Log.logAttention "getShowBySlug execution error" (show err)
                  let banner = renderBanner Warning "Show Not Found" "The show you're trying to edit doesn't exist."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> HomeTemplate.template <> banner
                    IsNotHxRequest -> banner <> HomeTemplate.template
                  pure $ Servant.addHeader "/" html
                Right Nothing -> do
                  Log.logInfo_ $ "No show with slug: '" <> display slug <> "'"
                  let banner = renderBanner Warning "Show Not Found" "The show you're trying to edit doesn't exist."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> HomeTemplate.template <> banner
                    IsNotHxRequest -> banner <> HomeTemplate.template
                  pure $ Servant.addHeader "/" html
                Right (Just showModel) -> do
                  bool (pure (Right ("[]", [], Set.empty))) (fetchStaffData showModel.id) isStaff >>= \case
                    Left err -> do
                      Log.logAttention "Failed to fetchStaffData" (show err)
                      let banner = renderBanner Warning "Show Not Found" "An error occurred fetching show data"
                      html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                        IsHxRequest -> HomeTemplate.template <> banner
                        IsNotHxRequest -> banner <> HomeTemplate.template
                      pure $ Servant.addHeader "/" html
                    Right (schedulesJson, eligibleHosts, currentHostIds) -> do
                      let editTemplate = template showModel userMetadata isStaff schedulesJson eligibleHosts currentHostIds
                      html <- renderTemplate hxRequest (Just userMetadata) editTemplate
                      pure $ Servant.noHeader html

-- | Fetch staff-only data for the edit form (schedules and hosts)
fetchStaffData ::
  ( Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadDB m,
    Has Tracer env
  ) =>
  Shows.Id ->
  m (Either HSQL.Pool.UsageError (Text, [UserMetadata.UserWithMetadata], Set User.Id))
fetchStaffData showId = runDBTransaction $ do
  schedulesJson <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showId
  eligibleHosts <- TRX.statement () $ UserMetadata.getAllUsersWithPagination 1000 0
  currentHostIds <- TRX.statement () $ ShowHost.getShowHosts showId

  pure (schedulesToJson schedulesJson, eligibleHosts, Set.fromList $ fmap (.shmUserId) currentHostIds)

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
import Data.Has (Has)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
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
          -- Check if user is a host of this show or is staff+
          execQuerySpan (ShowHost.isUserHostOfShow user.mId showModel.id) >>= \case
            Left err -> do
              Log.logAttention "isUserHostOfShow execution error" (show err)
              let banner = renderBanner Error "Not Authorized" "You don't have permission to edit this show."
              html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                IsHxRequest -> HomeTemplate.template <> banner
                IsNotHxRequest -> banner <> HomeTemplate.template
              pure $ Servant.addHeader "/" html
            Right True -> do
              Log.logInfo "Authorized user accessing show edit form" showModel.id
              let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
              -- Fetch staff-only data (schedules and hosts) if user is staff
              (schedulesJson, eligibleHosts, currentHostIds) <-
                if isStaff
                  then fetchStaffData showModel.id
                  else pure ("[]", [], Set.empty)
              let editTemplate = template showModel userMetadata isStaff schedulesJson eligibleHosts currentHostIds
              html <- renderTemplate hxRequest (Just userMetadata) editTemplate
              pure $ Servant.noHeader html
            Right False ->
              if UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then do
                  Log.logInfo "Staff user accessing show edit form" showModel.id
                  -- Fetch staff-only data (schedules and hosts)
                  (schedulesJson, eligibleHosts, currentHostIds) <- fetchStaffData showModel.id
                  let editTemplate = template showModel userMetadata True schedulesJson eligibleHosts currentHostIds
                  html <- renderTemplate hxRequest (Just userMetadata) editTemplate
                  pure $ Servant.noHeader html
                else do
                  Log.logInfo "User tried to edit show they don't host" showModel.id
                  let banner = renderBanner Error "Not Authorized" "You don't have permission to edit this show. Only hosts and staff can edit show details."
                  html <- renderTemplate hxRequest (Just userMetadata) $ case hxRequest of
                    IsHxRequest -> HomeTemplate.template <> banner
                    IsNotHxRequest -> banner <> HomeTemplate.template
                  pure $ Servant.addHeader "/" html

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
  m (Text, [UserMetadata.UserWithMetadata], Set User.Id)
fetchStaffData showId = do
  -- Fetch schedules
  schedulesJson <-
    execQuerySpan (ShowSchedule.getActiveScheduleTemplatesForShow showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch schedules" (show err)
        pure "[]"
      Right scheds -> pure $ schedulesToJson scheds

  -- Fetch all eligible hosts (all users)
  eligibleHosts <-
    execQuerySpan (UserMetadata.getAllUsersWithPagination 1000 0) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch eligible hosts" (show err)
        pure []
      Right hosts -> pure hosts

  -- Fetch current hosts for this show
  currentHostIds <-
    execQuerySpan (ShowHost.getShowHosts showId) >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch current hosts" (show err)
        pure Set.empty
      Right hosts -> pure $ Set.fromList $ map (.shmUserId) hosts

  pure (schedulesJson, eligibleHosts, currentHostIds)

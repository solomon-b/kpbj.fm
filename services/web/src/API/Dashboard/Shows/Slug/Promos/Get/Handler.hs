{-# LANGUAGE ViewPatterns #-}

-- | @GET /dashboard/shows/:show_slug/promos@, the promos page of one show.
module API.Dashboard.Shows.Slug.Promos.Get.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Promos.Get.Templates (template)
import API.Links (apiLinks)
import API.Types
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (breakItemUploadUrl)
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Time (getCurrentTime)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check the session, run the action, and render.
handler :: Slug -> Maybe Cookie -> Maybe HxRequest -> AppM (Lucid.Html ())
handler showSlug cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show promos" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    (showModel, promos) <- action user userMetadata showSlug

    env <- asks (Has.getter @Environment)
    backend <- asks (Has.getter @StorageBackend)
    today <- pacificDay <$> liftIO getCurrentTime

    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        (ForShow showModel)
        NavShowPromos
        Nothing
        Nothing
        (template backend showModel (breakItemUploadUrl env BreakItems.ShowPromo) today promos)

-- | Business logic: check access to the show, then load its promos.
--
-- A host sees the promos of the shows they host. Staff see every show.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  ExceptT HandlerError AppM (Shows.Model, [BreakItems.Model])
action user userMetadata showSlug = do
  requireShowHostOrStaff (User.mId user) showSlug userMetadata

  showModel <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowBySlug showSlug)

  promos <-
    fromRightM throwDatabaseError $
      execQuery (BreakItems.getPromosForShow (Shows.id showModel))

  pure (showModel, promos)

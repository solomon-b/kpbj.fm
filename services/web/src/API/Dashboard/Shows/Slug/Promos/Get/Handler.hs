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
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Config (Environment)
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Domains (breakItemUploadUrl)
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Time (getCurrentTime)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Domain.Types.Timezone (pacificDay)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check access to the show, run the action, and render.
--
-- A host sees the promos of the shows they host. Staff see every show.
handler :: Slug -> Satisfies 'UserMetadata.Host RoleAuth -> Maybe HxRequest -> AppM (Lucid.Html ())
handler showSlug host@(Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show promos" apiLinks.rootGet $ do
    showModel <- authorizeShow host showSlug
    promos <- action showModel

    env <- asks (Has.getter @Environment)
    backend <- asks (Has.getter @StorageBackend)
    today <- pacificDay <$> liftIO getCurrentTime

    lift $
      renderDashboardTemplate
        hxRequest
        auth.raMetadata
        (ForShow showModel)
        NavShowPromos
        Nothing
        Nothing
        (template backend showModel (breakItemUploadUrl env BreakItems.ShowPromo) today promos)

-- | Business logic: load the promos of a show that 'authorizeShow' checked.
action ::
  Shows.Model ->
  ExceptT HandlerError AppM [BreakItems.Model]
action showModel =
  fromRightM throwDatabaseError $
    execQuery (BreakItems.getPromosForShow (Shows.id showModel))

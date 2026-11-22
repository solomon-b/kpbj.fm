{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.CurrentShow.Get.Handler (handler, action) where

--------------------------------------------------------------------------------

import API.Dashboard.CurrentShow.Get.Templates (noShowsEmptyState)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.CurrentShow (resolveCurrentShow)
import App.Dashboard.ShowContext (ShowContext (..), ShowSection (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..), pageSwap, sectionUrl)
import Component.Flash (throwHxLocation, throwHxRedirect)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.String.Interpolate (i)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Business logic: find the page of the section for the current show.
--
-- A user with no shows gets 'Nothing'.
action ::
  Maybe Cookie ->
  UserMetadata.Model ->
  ShowSection ->
  ExceptT HandlerError AppM (Maybe Links.URI)
action mCookie userMetadata section = do
  mCurrentShow <- lift $ resolveCurrentShow mCookie userMetadata
  pure $ sectionUrl section <$> mCurrentShow

-- | Handler that redirects to a section of the current show.
--
-- An HTMX client gets the show page by swap into the main content. Other
-- clients get a full page redirect.
handler ::
  ShowSection ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler section (Satisfies _ auth) cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Current show redirect" apiLinks.rootGet $ do
    mUrl <- action cookie auth.raMetadata section
    case mUrl of
      Nothing ->
        lift $ renderDashboardTemplate hxRequest auth.raMetadata NoShow (navFor section) Nothing Nothing noShowsEmptyState
      Just url -> do
        Log.logInfo "Redirecting to the current show" (show url)
        lift $ case hxRequest of
          IsHxRequest -> throwHxLocation [i|/#{url}|] "#main-content" pageSwap
          IsNotHxRequest -> throwHxRedirect [i|/#{url}|] Nothing

-- | The sidebar entry of a section.
navFor :: ShowSection -> DashboardNav
navFor = \case
  SectionEpisodes -> NavEpisodes
  SectionPromos -> NavShowPromos
  SectionSettings -> NavSettings

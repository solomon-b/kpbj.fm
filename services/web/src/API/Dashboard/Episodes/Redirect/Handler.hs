{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Redirect.Handler (handler, action, EpisodeRedirectViewData (..)) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardEpisodesLinks)
import API.Types
import App.Common (renderDashboardTemplate, renderTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (redirectTemplate)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.List (uncons)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEpisodesGetUrl :: Shows.Model -> Links.URI
dashboardEpisodesGetUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug Nothing

--------------------------------------------------------------------------------

-- | Data needed to render the episodes redirect page.
data EpisodeRedirectViewData = EpisodeRedirectViewData
  { ervUserMetadata :: UserMetadata.Model,
    ervUserShows :: [Shows.Model]
  }

-- | Business logic: show fetching.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM EpisodeRedirectViewData
action user userMetadata = do
  -- 1. Fetch shows (admins see all, hosts see their own)
  userShows <- lift $ fetchShowsForUser user userMetadata

  pure
    EpisodeRedirectViewData
      { ervUserMetadata = userMetadata,
        ervUserShows = userShows
      }

-- | Handler that redirects to the first show's episodes page
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes redirect" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata
    case uncons vd.ervUserShows of
      Nothing ->
        -- No shows available - render dashboard with empty state
        lift $ renderDashboardTemplate hxRequest vd.ervUserMetadata [] Nothing NavEpisodes Nothing Nothing renderNoShowsEmptyState
      Just (firstShow, _) -> do
        Log.logInfo "Redirecting to first show's episodes" firstShow.slug
        lift $ renderTemplate hxRequest (Just vd.ervUserMetadata) (redirectTemplate [i|/#{dashboardEpisodesGetUrl firstShow}|])

-- | Empty state when user has no shows assigned
renderNoShowsEmptyState :: Lucid.Html ()
renderNoShowsEmptyState =
  Lucid.section_ [class_ $ base [Theme.bgAlt, "rounded", "overflow-hidden"]] $
    Lucid.div_ [class_ $ base [Theme.fgMuted, "text-center", "p-8"]] $ do
      Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Theme.fgPrimary]] "No Shows Available"
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "You don't have any shows assigned to your account yet."
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-1"]] "Contact an administrator to get started."

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

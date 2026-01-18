{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Episodes.Redirect.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardEpisodesLinks)
import API.Types
import App.Common (renderDashboardTemplate, renderTemplate)
import App.Handler.Combinators (requireAuth, requireHostNotSuspended)
import App.Handler.Error (handleHtmlErrors)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (redirectTemplate)
import Data.Either (fromRight)
import Data.List (uncons)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardEpisodesGetUrl :: Shows.Model -> Links.URI
dashboardEpisodesGetUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug Nothing

--------------------------------------------------------------------------------

-- | Handler that redirects to the first show's episodes page
handler ::
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Episodes redirect" apiLinks.rootGet $ do
    -- 1. Require authentication and host role
    (user, userMetadata) <- requireAuth cookie
    requireHostNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Fetch shows (admins see all, hosts see their own)
    userShows <- fetchShowsForUser user userMetadata

    -- 3. Redirect to first show's episodes page, or show empty state
    case uncons userShows of
      Nothing -> do
        -- No shows available - render dashboard with empty state
        renderDashboardTemplate hxRequest userMetadata [] Nothing NavEpisodes Nothing Nothing renderNoShowsEmptyState
      Just (firstShow, _) -> do
        Log.logInfo "Redirecting to first show's episodes" firstShow.slug
        renderTemplate hxRequest (Just userMetadata) (redirectTemplate [i|/#{dashboardEpisodesGetUrl firstShow}|])

-- | Empty state when user has no shows assigned
renderNoShowsEmptyState :: Lucid.Html ()
renderNoShowsEmptyState =
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden"]] $
    Lucid.div_ [class_ $ base [Tokens.textGray600, "text-center", "p-8"]] $ do
      Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.fontBold]] "No Shows Available"
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "You don't have any shows assigned to your account yet."
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-1"]] "Contact an administrator to get started."

-- | Fetch shows based on user role (admins see all, hosts see their own)
fetchShowsForUser ::
  User.Model ->
  UserMetadata.Model ->
  AppM [Shows.Model]
fetchShowsForUser user userMetadata =
  if UserMetadata.isAdmin userMetadata.mUserRole
    then fromRight [] <$> execQuerySpan Shows.getAllActiveShows
    else fromRight [] <$> execQuerySpan (Shows.getShowsForUser (User.mId user))

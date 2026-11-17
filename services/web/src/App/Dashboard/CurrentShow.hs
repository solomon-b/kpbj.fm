-- | Resolve the current show of the dashboard for a user.
module App.Dashboard.CurrentShow
  ( fetchDashboardShows,
    resolveCurrentShow,
  )
where

--------------------------------------------------------------------------------

import App.Dashboard.ShowContext (lookupCurrentShowSlug, pickCurrentShow)
import App.Monad (AppM)
import Control.Applicative ((<|>))
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified

--------------------------------------------------------------------------------

-- | The shows a user works with.
--
-- An admin gets all active shows. Every other role gets the hosted shows.
-- A failed query gives an empty list and an attention log line.
fetchDashboardShows :: UserMetadata.Model -> AppM [Shows.Model]
fetchDashboardShows userMetadata =
  execQuery showsQuery >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch the dashboard shows" (show err)
      pure []
    Right dashboardShows -> pure dashboardShows
  where
    showsQuery
      | UserMetadata.isAdmin userMetadata.mUserRole = Shows.getAllActiveShows
      | otherwise = Shows.getShowsForUser userMetadata.mUserId

-- | Find the current show from the cookie and the shows of the user.
--
-- Staff and admins can open a show that is not in their list, and that page
-- writes the cookie. For them, a cookie slug outside the list is looked up
-- among all shows.
resolveCurrentShow :: Maybe Cookie -> UserMetadata.Model -> AppM (Maybe Shows.Model)
resolveCurrentShow mCookie userMetadata = do
  userShows <- fetchDashboardShows userMetadata
  let mSlug = mCookie >>= lookupCurrentShowSlug
  case mSlug of
    Just slug
      | UserMetadata.isStaffOrHigher userMetadata.mUserRole,
        all (\s -> s.slug /= slug) userShows -> do
          mShow <- fetchShowBySlug slug
          pure $ mShow <|> pickCurrentShow Nothing userShows
    _ -> pure $ pickCurrentShow mSlug userShows

-- | Load one show by slug. A failed query gives nothing and an attention log
-- line.
fetchShowBySlug :: Slug -> AppM (Maybe Shows.Model)
fetchShowBySlug slug =
  execQuery (Shows.getShowBySlug slug) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch the current show" (show err)
      pure Nothing
    Right mShow -> pure mShow

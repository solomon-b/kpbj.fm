module API.Dashboard.NewsletterSubscribers.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for DELETE /dashboard/newsletter-subscribers/:subscriberId.
--
-- Removes a newsletter subscriber. Only staff and above can delete subscribers.
-- Returns an empty body plus an OOB banner; the row is removed via
-- @hx-swap="outerHTML"@ from the caller.
handler ::
  NewsletterSubscribers.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler subId cookie =
  handleBannerErrors "Newsletter subscriber delete" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can delete newsletter subscribers." userMetadata
    result <- execQuery (NewsletterSubscribers.deleteById subId)
    case result of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwNotFound "Subscriber"
      Right (Just _) -> pure ()
    pure $ do
      mempty
      renderBanner Success "Subscriber Deleted" "The subscriber has been removed."

module API.Dashboard.NewsletterSubscribers.Delete.Handler (handler) where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans.Class (lift)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Effects.Mailchimp.Sync (SyncOp (..), syncAsync)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Handler for DELETE /dashboard/newsletter-subscribers/:subscriberId.
--
-- Removes a newsletter subscriber. Only staff and above can delete subscribers.
-- Returns an empty body plus an OOB banner; the row is removed via
-- @hx-swap="outerHTML"@ from the caller.
handler ::
  NewsletterSubscribers.Id ->
  Satisfies 'Staff RoleAuth ->
  AppM (Lucid.Html ())
handler subId _ =
  handleBannerErrors "Newsletter subscriber delete" $ do
    result <- execQuery (NewsletterSubscribers.deleteById subId)
    case result of
      Left dbErr -> throwDatabaseError dbErr
      Right Nothing -> throwNotFound "Subscriber"
      Right (Just (_id, email)) ->
        lift $ syncAsync (Archive email subId)
    pure $ do
      mempty
      renderBanner Success "Subscriber Deleted" "The subscriber has been removed."

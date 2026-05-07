{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for @POST /api/newsletter/subscribe@.
--
-- Validates the email address, inserts into @newsletter_subscribers@, and
-- returns either a thank-you HTML fragment (which HTMX swaps in for the
-- form) or an OOB error banner. Duplicate emails are silently treated as
-- success so subscribers never see "you're already subscribed".
module API.Newsletter.Subscribe.Post.Handler (handler, action, SubscribeResult (..)) where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Newsletter.Subscribe.Post.Route (SubscribeForm (..))
import API.Newsletter.Subscribe.Post.Templates (thanksFragment)
import API.Types (Routes (..))
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Text.Display (display)
import Domain.Types.EmailAddress qualified as EmailAddress
import Effects.Database.Execute (execQueryThrow)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Mailchimp.Sync (SyncOp (..), syncAsync)
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Outcome of a newsletter subscribe attempt.
data SubscribeResult
  = -- | Email was valid and either inserted fresh or already existed.
    SubscribeSuccess
  | -- | Email failed domain-level validation.
    SubscribeInvalidEmail
  | -- | Email matches an existing tombstoned row whose Mailchimp status is
    -- @cleaned@ (a hard bounce). Mailchimp won't accept re-subscribe via API,
    -- so we surface a friendly message asking the user to contact us.
    SubscribeBlockedCleaned
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------

handler :: SubscribeForm -> AppM (Lucid.Html ())
handler form =
  handleHtmlErrors "Newsletter Subscribe" apiLinks.rootGet $ do
    result <- action form
    case result of
      SubscribeSuccess ->
        pure thanksFragment
      SubscribeInvalidEmail ->
        pure $ renderBanner Error "Invalid email" "Please enter a valid email address."
      SubscribeBlockedCleaned ->
        pure $
          renderBanner
            Warning
            "Address previously bounced"
            "This email address was previously marked undeliverable by our mail provider. Email us at contact@kpbj.fm to resubscribe."

--------------------------------------------------------------------------------

-- | Core subscription business logic.
--
-- Validates the email, then inserts via @ON CONFLICT DO NOTHING@. Conflicts
-- and successful inserts both yield 'SubscribeSuccess'.
action :: SubscribeForm -> ExceptT HandlerError AppM SubscribeResult
action SubscribeForm {..} =
  case EmailAddress.validate sfEmail of
    Left _ -> do
      lift $ Log.logInfo "Newsletter subscribe: invalid email" (Aeson.object ["email" .= display sfEmail])
      pure SubscribeInvalidEmail
    Right validEmail -> do
      mSubId <- lift $ execQueryThrow (NewsletterSubscribers.insert (NewsletterSubscribers.Insert validEmail))
      case mSubId of
        Just subId -> do
          lift $ syncAsync (Upsert validEmail subId)
          lift $ Log.logInfo "Newsletter subscribe: success" (Aeson.object ["email" .= display validEmail])
          pure SubscribeSuccess
        Nothing -> do
          -- ON CONFLICT swallowed the insert. Look up the existing row to
          -- distinguish a normal duplicate (treat as success) from a
          -- 'cleaned' tombstone (surface friendly message).
          existing <- lift $ execQueryThrow (NewsletterSubscribers.getByEmail validEmail)
          case existing >>= NewsletterSubscribers.mMailchimpStatus of
            Just "cleaned" -> do
              lift $
                Log.logInfo
                  "Newsletter subscribe: blocked (cleaned tombstone)"
                  (Aeson.object ["email" .= display validEmail])
              pure SubscribeBlockedCleaned
            _ -> do
              lift $
                Log.logInfo
                  "Newsletter subscribe: success (already subscribed)"
                  (Aeson.object ["email" .= display validEmail])
              pure SubscribeSuccess

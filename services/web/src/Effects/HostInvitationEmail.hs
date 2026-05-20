{-# LANGUAGE QuasiQuotes #-}

-- | Host invitation email builder and send wrapper.
--
-- Builds the plain-text invitation email used when staff create a host
-- invitation, and provides a fire-and-forget send wrapper that logs on
-- failure instead of throwing.
module Effects.HostInvitationEmail
  ( buildHostInvitationEmail,
    sendHostInvitationEmail,
  )
where

--------------------------------------------------------------------------------

import App.BaseUrl (baseUrl)
import App.Monad (AppM)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Lazy qualified as LT
import Domain.Types.EmailAddress (EmailAddress)
import Effects.Email.Send qualified as Email
import Log qualified

--------------------------------------------------------------------------------

-- | Build the host-invitation email. Plain text, voice matches brand:
--   direct, unpretentious, community-first.
buildHostInvitationEmail ::
  -- | Application base URL (e.g. @https:\/\/kpbj.fm@)
  Text ->
  -- | Recipient email address
  EmailAddress ->
  -- | Invitation token
  Text ->
  Email.Email
buildHostInvitationEmail appBaseUrl toEmail token =
  let inviteUrl = appBaseUrl <> "/invite/" <> token
   in Email.Email
        { Email.emailTo = display toEmail,
          Email.emailSubject = "You're invited to host a show on KPBJ 95.9FM",
          Email.emailBody =
            LT.fromStrict
              [i|
KPBJ 95.9FM

You've been invited to host a show on KPBJ. To accept:

  #{inviteUrl}

This link works once and expires in 7 days. On the page you'll set up
your account and confirm your show's details. If you didn't expect
this invitation, you can ignore this email.

— KPBJ
|],
          Email.emailLabel = "host-invitation"
        }

--------------------------------------------------------------------------------

-- | Fire-and-forget send of the host-invitation email. Logs on failure;
--   never throws.
--
--   Reads the application base URL from the AppM environment so callers
--   don't have to plumb it through.
sendHostInvitationEmail ::
  -- | Recipient email address
  EmailAddress ->
  -- | Invitation token
  Text ->
  AppM ()
sendHostInvitationEmail toEmail token = do
  url <- baseUrl
  Email.sendAsync (buildHostInvitationEmail url toEmail token)
  Log.logInfo "Host invitation email queued" (display toEmail)

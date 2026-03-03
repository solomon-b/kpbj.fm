-- | Re-exports SMTP configuration from @kpbj-email@.
module App.Smtp
  ( SmtpConfig (..),
    loadSmtpConfig,
  )
where

import Effects.Email.Config (SmtpConfig (..), loadSmtpConfig)

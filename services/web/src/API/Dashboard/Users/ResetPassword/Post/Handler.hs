{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.ResetPassword.Post.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.ResetPassword.Post.Templates qualified as Templates
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Password.Argon2 (hashPassword, mkPassword)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.PasswordReset qualified as PasswordReset
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

-- | Admin-only: assign a freshly generated password to a user.
--
-- Generates a random strong password, hashes it with Argon2, writes the hash to
-- the user's record, and deletes all of the user's sessions (forcing re-login
-- everywhere). The plaintext password is returned once inside a modal fragment;
-- it is never persisted.
handler ::
  User.Id ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler targetUserId cookie =
  handleBannerErrors "Assign password" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can reset user passwords." userMetadata
    user <-
      execQuery (UserMetadata.getUserWithMetadataById targetUserId) >>= \case
        Left err -> throwDatabaseError err
        Right Nothing -> throwNotFound "User"
        Right (Just u) -> pure u
    plaintext <- PasswordReset.generateRandomPassword
    passwordHash <- liftIO $ hashPassword (mkPassword plaintext)
    execQuery (PasswordReset.updateUserPassword targetUserId passwordHash) >>= \case
      Left err -> throwDatabaseError err
      Right Nothing -> throwNotFound "User"
      Right (Just _) -> pure ()
    _ <- execQuery (PasswordReset.deleteAllSessionsForUser targetUserId)
    Log.logInfo "Admin reset user password" (Aeson.object ["targetUserId" .= display targetUserId])
    pure (Templates.renderPasswordModal user plaintext)

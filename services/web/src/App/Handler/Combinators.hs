-- | Reusable handler combinators for common patterns.
--
-- These combinators use ExceptT-based error handling from "App.Handler.Error"
-- to provide early-exit semantics in handler pipelines.
--
-- Add new combinators here only when they are used in 2+ handlers.
module App.Handler.Combinators
  ( -- * Authentication
    requireAuth,

    -- * Authorization
    requireHostNotSuspended,
    requireStaffNotSuspended,
    requireAdminNotSuspended,
    requireShowHostOrStaff,

    -- * Validation Helpers
    requireJust,
    requireRight,
  )
where

--------------------------------------------------------------------------------

import App.Common (getUserInfo)
import App.Handler.Error (HandlerError, throwDatabaseError, throwNotAuthenticated, throwNotAuthorized, throwValidationError)
import App.Monad (AppM)
import Control.Monad (unless, when)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Require an authenticated user, throwing NotAuthenticated if not logged in.
--
-- Usage:
--
-- @
-- handler cookie = handleErrors $ do
--   (user, userMeta) <- requireAuth cookie
--   -- ... rest of handler with guaranteed authenticated user
-- @
requireAuth :: Maybe Cookie -> ExceptT HandlerError AppM (User.Model, UserMetadata.Model)
requireAuth cookie =
  lift (getUserInfo cookie) >>= \case
    Nothing -> throwNotAuthenticated
    Just userInfo -> pure userInfo

--------------------------------------------------------------------------------

-- | Require host role and non-suspended status, throwing NotAuthorized if not met.
--
-- Usage:
--
-- @
-- handler cookie = handleErrors $ do
--   (user, userMeta) <- requireAuth cookie
--   requireHostNotSuspended "You need host permissions to do this." userMeta
--   -- ... rest of handler with guaranteed host user
-- @
requireHostNotSuspended ::
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  ExceptT HandlerError AppM ()
requireHostNotSuspended msg userMetadata =
  unless (UserMetadata.isHostOrHigher userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg (Just userMetadata.mUserRole)

--------------------------------------------------------------------------------

-- | Require staff role and non-suspended status, throwing NotAuthorized if not met.
--
-- Usage:
--
-- @
-- handler cookie = handleErrors $ do
--   (user, userMeta) <- requireAuth cookie
--   requireStaffNotSuspended "You need staff permissions to do this." userMeta
--   -- ... rest of handler with guaranteed staff user
-- @
requireStaffNotSuspended ::
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  ExceptT HandlerError AppM ()
requireStaffNotSuspended msg userMetadata =
  unless (UserMetadata.isStaffOrHigher userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg (Just userMetadata.mUserRole)

--------------------------------------------------------------------------------

-- | Require admin role and non-suspended status, throwing NotAuthorized if not met.
--
-- Usage:
--
-- @
-- handler cookie = handleErrors $ do
--   (user, userMeta) <- requireAuth cookie
--   requireAdminNotSuspended "You need admin permissions to do this." userMeta
--   -- ... rest of handler with guaranteed admin user
-- @
requireAdminNotSuspended ::
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  ExceptT HandlerError AppM ()
requireAdminNotSuspended msg userMetadata =
  unless (UserMetadata.isAdmin userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg (Just userMetadata.mUserRole)

--------------------------------------------------------------------------------

-- | Require user to be either a host of the show OR staff+, and not suspended.
--
-- This is the common pattern for show-related actions where hosts can manage
-- their own shows, but staff/admins can manage any show.
--
-- Usage:
--
-- @
-- handler showSlug cookie = handleErrors $ do
--   (user, userMeta) <- requireAuth cookie
--   requireShowHostOrStaff user.mId showSlug userMeta
--   -- ... rest of handler
-- @
requireShowHostOrStaff ::
  -- | User ID to check
  User.Id ->
  -- | Show slug to check host membership
  Slug ->
  -- | User metadata to check role and suspension
  UserMetadata.Model ->
  ExceptT HandlerError AppM ()
requireShowHostOrStaff userId showSlug userMetadata = do
  -- Suspended users can never proceed
  when (UserMetadata.isSuspended userMetadata) $
    throwNotAuthorized msg (Just userMetadata.mUserRole)

  -- Staff+ can always proceed (they're not suspended at this point)
  let isStaffOrHigher = UserMetadata.isStaffOrHigher userMetadata.mUserRole
  unless isStaffOrHigher $ do
    -- Not staff, check if they're a host of this show
    isHost <-
      fromRightM throwDatabaseError $
        execQuery (ShowHost.isUserHostOfShowSlug userId showSlug)
    unless isHost $
      throwNotAuthorized msg (Just userMetadata.mUserRole)
  where
    msg = "You don't have permission to access this show."

--------------------------------------------------------------------------------

-- | Require a Just value, throwing ValidationError if Nothing.
--
-- Usage:
--
-- @
-- parsedStatus <- requireJust "Invalid status value." (parseStatus rawStatus)
-- @
requireJust ::
  -- | Error message if Nothing
  Text ->
  -- | Value to check
  Maybe a ->
  ExceptT HandlerError AppM a
requireJust msg = \case
  Nothing -> throwValidationError msg
  Just a -> pure a

--------------------------------------------------------------------------------

-- | Require a Right value, throwing ValidationError if Left.
--
-- Usage:
--
-- @
-- validTitle <- requireRight displayError (validateContentLength 200 title)
-- @
requireRight ::
  -- | Function to convert Left error to message
  (e -> Text) ->
  -- | Value to check
  Either e a ->
  ExceptT HandlerError AppM a
requireRight toMsg = \case
  Left err -> throwValidationError (toMsg err)
  Right a -> pure a

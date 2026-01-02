-- | Reusable handler combinators for common patterns.
--
-- These combinators use the exception-based error handling from "App.Handler.Error"
-- to provide early-exit semantics that work with MonadUnliftIO.
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
import App.Handler.Error (throwDatabaseError, throwNotAuthenticated, throwNotAuthorized, throwValidationError)
import Control.Monad (unless, when)
import Control.Monad.Catch (MonadThrow)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import OpenTelemetry.Trace (Tracer)

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
requireAuth ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m,
    MonadThrow m
  ) =>
  Maybe Cookie ->
  m (User.Model, UserMetadata.Model)
requireAuth cookie =
  getUserInfo cookie >>= \case
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
  (MonadThrow m) =>
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  m ()
requireHostNotSuspended msg userMetadata =
  unless (UserMetadata.isHostOrHigher userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg

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
  (MonadThrow m) =>
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  m ()
requireStaffNotSuspended msg userMetadata =
  unless (UserMetadata.isStaffOrHigher userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg

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
  (MonadThrow m) =>
  -- | Error message if not authorized
  Text ->
  -- | User metadata to check
  UserMetadata.Model ->
  m ()
requireAdminNotSuspended msg userMetadata =
  unless (UserMetadata.isAdmin userMetadata.mUserRole && not (UserMetadata.isSuspended userMetadata)) $
    throwNotAuthorized msg

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
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadThrow m,
    Has Tracer env
  ) =>
  -- | User ID to check
  User.Id ->
  -- | Show slug to check host membership
  Slug ->
  -- | User metadata to check role and suspension
  UserMetadata.Model ->
  m ()
requireShowHostOrStaff userId showSlug userMetadata = do
  -- Suspended users can never proceed
  when (UserMetadata.isSuspended userMetadata) $
    throwNotAuthorized msg

  -- Staff+ can always proceed (they're not suspended at this point)
  let isStaffOrHigher = UserMetadata.isStaffOrHigher userMetadata.mUserRole
  unless isStaffOrHigher $ do
    -- Not staff, check if they're a host of this show
    isHostResult <- execQuerySpan (ShowHost.isUserHostOfShowSlug userId showSlug)
    case isHostResult of
      Left err -> throwDatabaseError err
      Right isHost ->
        unless isHost $
          throwNotAuthorized msg
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
  (MonadThrow m) =>
  -- | Error message if Nothing
  Text ->
  -- | Value to check
  Maybe a ->
  m a
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
  (MonadThrow m) =>
  -- | Function to convert Left error to message
  (e -> Text) ->
  -- | Value to check
  Either e a ->
  m a
requireRight toMsg = \case
  Left err -> throwValidationError (toMsg err)
  Right a -> pure a

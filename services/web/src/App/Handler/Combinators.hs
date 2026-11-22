-- | Reusable handler combinators for common patterns.
--
-- These combinators use ExceptT-based error handling from "App.Handler.Error"
-- to provide early-exit semantics in handler pipelines.
--
-- Add new combinators here only when they are used in 2+ handlers.
module App.Handler.Combinators
  ( -- * Authentication
    requirePlayoutSecret,

    -- * Authorization
    authorizeShow,

    -- * Validation Helpers
    requireJust,
    requireRight,
  )
where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.CustomContext (PlayoutSecret (..), secretsMatch)
import App.Handler.Error (HandlerError, throwDatabaseError, throwNotAuthenticated, throwNotAuthorized, throwNotFound, throwValidationError)
import App.Monad (AppM)
import Control.Monad (unless)
import Control.Monad.Reader (asks)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Text (Text)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Require a valid @X-Playout-Secret@ header, for the endpoints Liquidsoap calls.
--
-- These endpoints carry no session cookie, so the shared secret is the only
-- thing that separates Liquidsoap from the open internet. Nginx proxies every
-- path to the web service, so an ungated playout route answers any caller.
--
-- An unset @PLAYOUT_SECRET@ rejects every request. Accepting one instead would
-- open the endpoint whenever the environment loses the variable.
--
-- Usage:
--
-- @
-- handler mSecret = do
--   result <- runExceptT $ requirePlayoutSecret mSecret
--   case result of
--     Left _ -> throwM err401
--     Right () -> -- ... rest of handler
-- @
requirePlayoutSecret :: Maybe Text -> ExceptT HandlerError AppM ()
requirePlayoutSecret mSecret = do
  mExpectedSecret <- asks (Has.getter @PlayoutSecret)
  case mExpectedSecret.unPlayoutSecret of
    Nothing -> do
      Log.logAttention "PLAYOUT_SECRET not configured, rejecting request" ()
      throwNotAuthenticated
    Just expected -> case mSecret of
      Nothing -> do
        Log.logAttention "Missing X-Playout-Secret header" ()
        throwNotAuthenticated
      Just given ->
        unless (secretsMatch given expected) $ do
          Log.logAttention "Invalid X-Playout-Secret header" ()
          throwNotAuthenticated

--------------------------------------------------------------------------------

-- | Load the show in the path, and check that the user may manage it.
--
-- Staff and Admin may manage every show. A Host must have a row in
-- @show_hosts@ for the show. The 'Satisfies' argument comes from a @'Host@
-- gate, which already refuses a suspended user.
--
-- Usage:
--
-- @
-- handler showSlug host =
--   handleHtmlErrors "Show edit" apiLinks.rootGet $ do
--     showModel <- authorizeShow host showSlug
--     -- ... rest of handler
-- @
authorizeShow ::
  Satisfies 'UserMetadata.Host RoleAuth ->
  -- | The slug in the path
  Slug ->
  ExceptT HandlerError AppM Shows.Model
authorizeShow (Satisfies _ auth) showSlug = do
  showModel <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowBySlug showSlug)
  unless (UserMetadata.isStaffOrHigher auth.raMetadata.mUserRole) $ do
    isHost <-
      fromRightM throwDatabaseError $
        execQuery (ShowHost.isUserHostOfShowSlug auth.raUser.mId showSlug)
    unless isHost $
      throwNotAuthorized "You don't have permission to access this show." (Just auth.raMetadata.mUserRole)
  pure showModel

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

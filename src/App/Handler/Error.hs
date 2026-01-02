-- | Handler-level error types for early-exit control flow.
--
-- This module provides a unified error type for handler failures that can be
-- thrown as exceptions (via MonadThrow) and caught at handler boundaries
-- (via MonadCatch). This pattern works with MonadUnliftIO unlike ExceptT.
--
-- Usage pattern:
--
-- @
-- handler cookie hxRequest =
--   handleErrors $ do
--     (user, meta) <- requireAuth cookie  -- throws NotAuthenticated
--     episode <- fetchOrNotFound "Episode" query  -- throws NotFound
--     ...
--
-- handleErrors action =
--   action \`catch\` \\case
--     NotAuthenticated -> redirectToLogin
--     NotFound msg -> redirect404 msg
--     ...
-- @
module App.Handler.Error
  ( -- * Handler Error Type
    HandlerError (..),

    -- * Throwing Helpers
    throwNotAuthenticated,
    throwNotAuthorized,
    throwNotFound,
    throwDatabaseError,
    throwValidationError,
    throwUserSuspended,
    throwHandlerFailure,

    -- * Catching Helpers
    catchHandlerError,

    -- * Redirect Error Handlers
    handleRedirectErrors,
    logHandlerError,

    -- * Banner Error Handlers (OOB swap pattern)
    handleBannerErrors,

    -- * HTML Error Handlers (GET handlers with client-side redirect)
    handleHtmlErrors,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, userLinks)
import API.Types (UserRoutes (..))
import Component.Banner (BannerType (..), renderBanner)
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Exception (Exception)
import Control.Monad.Catch (MonadCatch, MonadThrow, catch, throwM)
import Data.Text (Text)
import Hasql.Pool (UsageError)
import Log qualified
import Lucid qualified
import Servant qualified
import Servant.Links (Link)

--------------------------------------------------------------------------------

-- | Unified error type for handler-level failures.
--
-- These errors represent expected failure modes during request processing.
-- They are thrown as exceptions for early-exit and caught at handler boundaries
-- to produce appropriate HTTP responses or HTMX banners.
data HandlerError
  = -- | User is not authenticated (no valid session)
    NotAuthenticated
  | -- | User is authenticated but lacks permission for this action
    NotAuthorized Text
  | -- | Requested resource does not exist
    NotFound Text
  | -- | Database query or transaction failed
    DatabaseError UsageError
  | -- | User input validation failed
    ValidationError Text
  | -- | User account is suspended
    UserSuspended
  | -- | Generic handler error with message
    HandlerFailure Text
  deriving stock (Show)

instance Exception HandlerError

--------------------------------------------------------------------------------
-- Throwing Helpers

-- | Throw NotAuthenticated error.
throwNotAuthenticated :: (MonadThrow m) => m a
throwNotAuthenticated = throwM NotAuthenticated

-- | Throw NotAuthorized error with a message.
throwNotAuthorized :: (MonadThrow m) => Text -> m a
throwNotAuthorized = throwM . NotAuthorized

-- | Throw NotFound error with the resource name.
throwNotFound :: (MonadThrow m) => Text -> m a
throwNotFound = throwM . NotFound

-- | Throw DatabaseError from a UsageError.
throwDatabaseError :: (MonadThrow m) => UsageError -> m a
throwDatabaseError = throwM . DatabaseError

-- | Throw ValidationError with a message.
throwValidationError :: (MonadThrow m) => Text -> m a
throwValidationError = throwM . ValidationError

-- | Throw UserSuspended error.
throwUserSuspended :: (MonadThrow m) => m a
throwUserSuspended = throwM UserSuspended

-- | Throw a generic HandlerFailure with a message.
throwHandlerFailure :: (MonadThrow m) => Text -> m a
throwHandlerFailure = throwM . HandlerFailure

--------------------------------------------------------------------------------
-- Catching Helpers

-- | Catch a HandlerError and handle it.
--
-- Usage:
--
-- @
-- action \`catchHandlerError\` \\case
--   NotAuthenticated -> redirectToLogin
--   NotFound msg -> show404 msg
--   _ -> showGenericError
-- @
catchHandlerError :: (MonadCatch m) => m a -> (HandlerError -> m a) -> m a
catchHandlerError = catch

--------------------------------------------------------------------------------
-- Redirect Error Handlers

-- | Standard redirect error handler for handlers that redirect on errors.
--
-- Redirects to login page for NotAuthenticated, defaultUrl for everything else.
-- Logs all errors with the provided action name.
--
-- Usage:
--
-- @
-- handler cookie editForm =
--   handleRedirectErrors "Episode edit" (dashboardEpisodesLinks.list showSlug Nothing) $ do
--     (user, meta) <- requireAuth cookie
--     ...
-- @
handleRedirectErrors ::
  (MonadCatch m, Log.MonadLog m) =>
  -- | Action name for logging (e.g., "Episode edit")
  Text ->
  -- | Default redirect URL (for other errors)
  Link ->
  -- | The action to run
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ())) ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handleRedirectErrors actionName defaultUrl action =
  action `catchHandlerError` \err -> do
    logHandlerError actionName err
    pure $ mkErrorRedirect defaultUrl err

-- | Log a handler error with structured context.
logHandlerError :: (Log.MonadLog m) => Text -> HandlerError -> m ()
logHandlerError actionName = \case
  NotAuthenticated -> Log.logInfo_ $ actionName <> ": not authenticated"
  NotAuthorized msg -> Log.logInfo (actionName <> ": not authorized") msg
  NotFound resource -> Log.logInfo (actionName <> ": not found") resource
  DatabaseError dbErr -> Log.logAttention (actionName <> ": database error") (show dbErr)
  UserSuspended -> Log.logInfo_ $ actionName <> ": user suspended"
  ValidationError msg -> Log.logInfo (actionName <> ": validation error") msg
  HandlerFailure msg -> Log.logAttention (actionName <> ": handler failure") msg

-- | Login link for authentication errors.
loginLink :: Link
loginLink = userLinks.loginGet Nothing Nothing

-- | Create redirect response based on error type.
mkErrorRedirect ::
  -- | Default URL
  Link ->
  -- | The error to handle
  HandlerError ->
  Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ())
mkErrorRedirect defaultLink = \case
  NotAuthenticated ->
    let url = rootLink loginLink
        banner = BannerParams Error "Login Required" "Please log in to continue."
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  NotAuthorized msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Access Denied" msg
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  NotFound resource ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Not Found" (resource <> " not found.")
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  DatabaseError _ ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Error" "A database error occurred. Please try again."
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  UserSuspended ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Account Suspended" "Your account is suspended."
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  ValidationError msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Validation Error" msg
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner
  HandlerFailure msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Error" msg
     in Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner

--------------------------------------------------------------------------------
-- Banner Error Handlers (OOB swap pattern)

-- | Error handler for handlers that return OOB banners on errors.
--
-- Used for actions like delete, suspend, etc. that show inline error banners
-- rather than redirecting.
--
-- Usage:
--
-- @
-- handler cookie =
--   handleBannerErrors "User suspend" $ do
--     (user, meta) <- requireAuth cookie
--     ...
-- @
handleBannerErrors ::
  (MonadCatch m, Log.MonadLog m) =>
  -- | Action name for logging (e.g., "User suspend", "Blog delete")
  Text ->
  -- | The action to run
  m (Lucid.Html ()) ->
  m (Lucid.Html ())
handleBannerErrors actionName action =
  action `catchHandlerError` \err -> do
    logHandlerError actionName err
    pure $ mkErrorBanner err

-- | Create an OOB error banner based on error type.
mkErrorBanner :: HandlerError -> Lucid.Html ()
mkErrorBanner = \case
  NotAuthenticated ->
    renderBanner Error "Authentication Required" "You must be logged in to perform this action."
  NotAuthorized msg ->
    renderBanner Error "Access Denied" msg
  NotFound resource ->
    renderBanner Error "Not Found" (resource <> " not found.")
  DatabaseError _ ->
    renderBanner Error "Error" "A database error occurred. Please try again."
  UserSuspended ->
    renderBanner Error "Account Suspended" "Your account is suspended."
  ValidationError msg ->
    renderBanner Error "Validation Error" msg
  HandlerFailure msg ->
    renderBanner Error "Error" msg

--------------------------------------------------------------------------------
-- HTML Error Handlers (GET handlers with client-side redirect)

-- | Error handler for GET handlers that return plain Html ().
--
-- Uses client-side (meta refresh) redirects for errors. Useful for GET handlers
-- that need to redirect to login or show error pages.
--
-- Usage:
--
-- @
-- handler cookie hxRequest =
--   handleHtmlErrors "Profile edit" dashboardLinks.home $ do
--     (user, meta) <- requireAuth cookie
--     ...
--     pure someHtmlTemplate
-- @
handleHtmlErrors ::
  (MonadCatch m, Log.MonadLog m) =>
  -- | Action name for logging (e.g., "Profile edit")
  Text ->
  -- | Default redirect URL (for other errors)
  Link ->
  -- | The action to run
  m (Lucid.Html ()) ->
  m (Lucid.Html ())
handleHtmlErrors actionName defaultUrl action =
  action `catchHandlerError` \err -> do
    logHandlerError actionName err
    pure $ mkHtmlErrorRedirect defaultUrl err

-- | Create client-side redirect HTML based on error type.
mkHtmlErrorRedirect ::
  -- | Default URL
  Link ->
  -- | The error to handle
  HandlerError ->
  Lucid.Html ()
mkHtmlErrorRedirect defaultLink = \case
  NotAuthenticated ->
    let url = rootLink loginLink
        banner = BannerParams Error "Login Required" "Please log in to continue."
     in redirectWithBanner url banner
  NotAuthorized msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Access Denied" msg
     in redirectWithBanner url banner
  NotFound resource ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Not Found" (resource <> " not found.")
     in redirectWithBanner url banner
  DatabaseError _ ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Error" "A database error occurred. Please try again."
     in redirectWithBanner url banner
  UserSuspended ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Account Suspended" "Your account is suspended."
     in redirectWithBanner url banner
  ValidationError msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Validation Error" msg
     in redirectWithBanner url banner
  HandlerFailure msg ->
    let url = rootLink defaultLink
        banner = BannerParams Error "Error" msg
     in redirectWithBanner url banner

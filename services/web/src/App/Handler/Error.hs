-- | Handler-level error types and ExceptT-based error handling.
--
-- Provides a unified error type for handler failures, ExceptT-based throw
-- helpers, and error handler wrappers that convert errors into appropriate
-- HTTP responses (redirects, banners, or HTML error pages).
--
-- Usage pattern:
--
-- @
-- handler cookie hxRequest =
--   handleHtmlErrors "Shows list" apiLinks.rootGet $ do
--     viewData <- action ...
--     lift $ renderTemplate hxRequest mUserInfo (template viewData)
--
-- action :: ... -> ExceptT HandlerError AppM ViewData
-- action = do
--   tags <- fromRightM throwDatabaseError $ execQuery ...
--   ...
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

    -- * Error Handlers
    handleHtmlErrors,
    handlePublicErrors,
    handleRedirectErrors,
    handleBannerErrors,

    -- * Logging
    logHandlerError,

    -- * Error Response Builders
    errorRedirectParams,
    notFoundContent,
    errorContent,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardLinks, rootLink, userLinks)
import API.Types (DashboardRoutes (..), Routes (..), UserRoutes (..))
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Component.Redirect (BannerParams (..), buildRedirectUrl, redirectWithBanner)
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Hasql.Pool (UsageError)
import Log qualified
import Lucid qualified
import Servant qualified
import Servant.Links (Link)

--------------------------------------------------------------------------------

-- | Unified error type for handler-level failures.
--
-- These errors represent expected failure modes during request processing.
-- They are thrown via ExceptT for early-exit and handled at handler boundaries
-- to produce appropriate HTTP responses or HTMX banners.
data HandlerError
  = -- | User is not authenticated (no valid session)
    NotAuthenticated
  | -- | User is authenticated but lacks permission for this action.
    -- Includes the user's role so error handlers can redirect appropriately:
    -- - Users with no dashboard access (User role) go to public site
    -- - Users with dashboard access (Host+) go to dashboard home
    NotAuthorized Text (Maybe UserRole)
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

--------------------------------------------------------------------------------
-- Throwing Helpers

-- | Throw NotAuthenticated error.
throwNotAuthenticated :: ExceptT HandlerError AppM a
throwNotAuthenticated = throwE NotAuthenticated

-- | Throw NotAuthorized error with a message and user role.
--
-- The role is used by error handlers to redirect appropriately:
-- - Users with no dashboard access (User role) go to public site
-- - Users with dashboard access (Host+) go to dashboard home
throwNotAuthorized :: Text -> Maybe UserRole -> ExceptT HandlerError AppM a
throwNotAuthorized msg role = throwE $ NotAuthorized msg role

-- | Throw NotFound error with the resource name.
throwNotFound :: Text -> ExceptT HandlerError AppM a
throwNotFound = throwE . NotFound

-- | Throw DatabaseError from a UsageError.
throwDatabaseError :: UsageError -> ExceptT HandlerError AppM a
throwDatabaseError = throwE . DatabaseError

-- | Throw ValidationError with a message.
throwValidationError :: Text -> ExceptT HandlerError AppM a
throwValidationError = throwE . ValidationError

-- | Throw UserSuspended error.
throwUserSuspended :: ExceptT HandlerError AppM a
throwUserSuspended = throwE UserSuspended

-- | Throw a generic HandlerFailure with a message.
throwHandlerFailure :: Text -> ExceptT HandlerError AppM a
throwHandlerFailure = throwE . HandlerFailure

--------------------------------------------------------------------------------
-- Error Handlers

-- | Error handler for GET handlers that return plain @Html ()@.
--
-- Runs the ExceptT action, logging and rendering errors as client-side redirects.
--
-- @
-- handler cookie hxRequest =
--   handleHtmlErrors "Shows list" apiLinks.rootGet $ do
--     viewData <- action ...
--     lift $ renderTemplate hxRequest mUserInfo (template viewData)
-- @
handleHtmlErrors ::
  -- | Action name for logging
  Text ->
  -- | Default redirect URL (for non-auth errors)
  Link ->
  -- | The ExceptT action to run
  ExceptT HandlerError AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleHtmlErrors actionName defaultUrl action =
  runExceptT action >>= \case
    Right html -> pure html
    Left err -> do
      logHandlerError actionName err
      let (url, banner) = errorRedirectParams defaultUrl err
      pure $ redirectWithBanner url banner

-- | Error handler for public-facing GET handlers.
--
-- Renders not-found and other non-auth errors inline instead of redirecting,
-- so visitors see a contextual error page at the same URL. Auth errors
-- (NotAuthenticated, NotAuthorized) still redirect to login or home.
--
-- The caller provides a render function that wraps error content in the
-- page layout (via renderTemplate).
--
-- @
-- handler slug cookie (foldHxReq -> hxRequest) =
--   handlePublicErrors "Show detail" renderError $ do
--     mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
--     vd <- action slug
--     lift $ renderTemplate hxRequest mUserInfo (template vd)
--  where
--   renderError content = do
--     mUserInfo <- getUserInfo cookie <&> fmap snd
--     renderTemplate hxReq mUser content
-- @
handlePublicErrors ::
  -- | Action name for logging
  Text ->
  -- | Render inline error content wrapped in page layout
  (Lucid.Html () -> AppM (Lucid.Html ())) ->
  -- | The ExceptT action to run
  ExceptT HandlerError AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handlePublicErrors actionName renderInline action =
  runExceptT action >>= \case
    Right html -> pure html
    Left err -> do
      logHandlerError actionName err
      case err of
        -- Auth errors still redirect
        NotAuthenticated ->
          let (url, banner) = errorRedirectParams (apiLinks.rootGet) err
           in pure $ redirectWithBanner url banner
        NotAuthorized _ _ ->
          let (url, banner) = errorRedirectParams (apiLinks.rootGet) err
           in pure $ redirectWithBanner url banner
        -- Content errors render inline
        NotFound resource -> renderInline (notFoundContent resource)
        DatabaseError _ -> renderInline (errorContent "Something went wrong. Please try again.")
        UserSuspended -> renderInline (errorContent "Your account is suspended.")
        ValidationError msg -> renderInline (errorContent msg)
        HandlerFailure msg -> renderInline (errorContent msg)

-- | Error handler for handlers that redirect on errors (POST handlers).
--
-- Runs the ExceptT action, logging and rendering errors as HX-Redirect responses.
--
-- @
-- handler cookie editForm =
--   handleRedirectErrors "Episode edit" (dashboardEpisodesLinks.list showSlug Nothing) $ do
--     vd <- action ...
--     html <- lift $ renderDashboardTemplate ...
--     pure $ Servant.noHeader html
-- @
handleRedirectErrors ::
  -- | Action name for logging
  Text ->
  -- | Default redirect URL (for non-auth errors)
  Link ->
  -- | The ExceptT action to run
  ExceptT HandlerError AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ())) ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handleRedirectErrors actionName defaultUrl action =
  runExceptT action >>= \case
    Right result -> pure result
    Left err -> do
      logHandlerError actionName err
      let (url, banner) = errorRedirectParams defaultUrl err
      pure $
        Servant.addHeader (buildRedirectUrl url banner) $
          redirectWithBanner url banner

-- | Error handler for handlers that return OOB banners on errors.
--
-- Runs the ExceptT action, logging and rendering errors as OOB banner swaps.
--
-- @
-- handler cookie =
--   handleBannerErrors "User suspend" $ do
--     ...
-- @
handleBannerErrors ::
  -- | Action name for logging
  Text ->
  -- | The ExceptT action to run
  ExceptT HandlerError AppM (Lucid.Html ()) ->
  AppM (Lucid.Html ())
handleBannerErrors actionName action =
  runExceptT action >>= \case
    Right html -> pure html
    Left err -> do
      logHandlerError actionName err
      pure $ errorBanner err

--------------------------------------------------------------------------------
-- Logging

-- | Log a handler error with structured context.
logHandlerError :: (Log.MonadLog m) => Text -> HandlerError -> m ()
logHandlerError actionName = \case
  NotAuthenticated -> Log.logInfo_ $ actionName <> ": not authenticated"
  NotAuthorized msg mRole -> Log.logInfo (actionName <> ": not authorized") (msg, fmap show mRole)
  NotFound resource -> Log.logInfo (actionName <> ": not found") resource
  DatabaseError dbErr -> Log.logAttention (actionName <> ": database error") (show dbErr)
  UserSuspended -> Log.logInfo_ $ actionName <> ": user suspended"
  ValidationError msg -> Log.logInfo (actionName <> ": validation error") msg
  HandlerFailure msg -> Log.logAttention (actionName <> ": handler failure") msg

--------------------------------------------------------------------------------
-- Error Response Builders

-- | Login link for authentication errors.
loginLink :: Link
loginLink = userLinks.loginGet Nothing Nothing

-- | Determine redirect URL for NotAuthorized based on user role.
--
-- - Users with dashboard access (Host, Staff, Admin) go to dashboard home
-- - Users without dashboard access (User role or Nothing) go to public home
notAuthorizedRedirect :: Maybe UserRole -> Link
notAuthorizedRedirect = \case
  Just Host -> dashboardLinks.home
  Just Staff -> dashboardLinks.home
  Just Admin -> dashboardLinks.home
  Just User -> apiLinks.rootGet
  Nothing -> apiLinks.rootGet

-- | Map a handler error to a redirect URL and banner params.
--
-- Single source of truth for error-to-redirect mapping, used by both
-- 'handleHtmlErrors' (client-side redirect HTML) and 'handleRedirectErrors'
-- (HX-Redirect header).
errorRedirectParams :: Link -> HandlerError -> (Text, BannerParams)
errorRedirectParams defaultLink = \case
  NotAuthenticated ->
    (rootLink loginLink, BannerParams Error "Login Required" "Please log in to continue.")
  NotAuthorized msg mRole ->
    (rootLink (notAuthorizedRedirect mRole), BannerParams Error "Access Denied" msg)
  NotFound resource ->
    (rootLink defaultLink, BannerParams Error "Not Found" (resource <> " not found."))
  DatabaseError _ ->
    (rootLink defaultLink, BannerParams Error "Error" "A database error occurred. Please try again.")
  UserSuspended ->
    (rootLink defaultLink, BannerParams Error "Account Suspended" "Your account is suspended.")
  ValidationError msg ->
    (rootLink defaultLink, BannerParams Error "Validation Error" msg)
  HandlerFailure msg ->
    (rootLink defaultLink, BannerParams Error "Error" msg)

-- | Map a handler error to an OOB error banner.
errorBanner :: HandlerError -> Lucid.Html ()
errorBanner = \case
  NotAuthenticated ->
    renderBanner Error "Authentication Required" "You must be logged in to perform this action."
  NotAuthorized msg _ ->
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
-- Public Error Pages

-- | Inline not-found content for public pages.
--
-- Renders a "not found" message that can be wrapped in the site layout
-- via 'renderTemplate'. Used by 'handlePublicErrors' for 'NotFound' errors.
notFoundContent :: Text -> Lucid.Html ()
notFoundContent resource =
  Lucid.div_ [class_ $ base [Tokens.containerWidth, "py-16", "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.heading2xl, Tokens.mb4]] $
      Lucid.toHtml (resource <> " not found")
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb8]] "The page you're looking for doesn't exist or has been removed."
    Lucid.a_ [Lucid.href_ homeUrl, class_ $ base [Tokens.linkText]] "Back to home"
  where
    homeUrl = rootLink apiLinks.rootGet

-- | Inline error content for public pages.
--
-- Renders a generic error message for non-404 failures (database errors,
-- suspended accounts, etc.). Used by 'handlePublicErrors'.
errorContent :: Text -> Lucid.Html ()
errorContent msg =
  Lucid.div_ [class_ $ base [Tokens.containerWidth, "py-16", "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.heading2xl, Tokens.mb4]] "Error"
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb8]] $ Lucid.toHtml msg
    Lucid.a_ [Lucid.href_ homeUrl, class_ $ base [Tokens.linkText]] "Back to home"
  where
    homeUrl = rootLink apiLinks.rootGet

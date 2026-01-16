-- | CSRF (Cross-Site Request Forgery) protection.
--
-- This module provides origin-based CSRF protection for XHR requests.
-- It validates that the Origin header matches the expected origin(s).
--
-- == How It Works
--
-- When a browser makes a cross-origin XHR request, it includes an Origin header.
-- By checking this header, we can reject requests that don't originate from
-- our trusted origins.
--
-- == Limitations
--
-- - Origin header may be omitted in some same-origin requests
-- - Older browsers may not send Origin headers
-- - For maximum security, combine with SameSite cookies
--
-- == Usage
--
-- @
-- handler mOrigin form = do
--   validateOrigin mOrigin allowedOrigins >>= \\case
--     Left err -> pure $ UploadError err
--     Right () -> processUpload form
-- @
module Effects.CSRF
  ( -- * Origin Validation
    validateOrigin,
    validateOriginStrict,

    -- * Configuration
    allowedOrigins,
  )
where

--------------------------------------------------------------------------------

import App.Config (Hostname)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Reader (MonadReader, asks)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Origin (Origin (..), isAllowedOrigin, mkOrigin)
import Log qualified

--------------------------------------------------------------------------------
-- Validation

-- | Validate the Origin header against a list of allowed origins.
--
-- Permissive mode: Allows requests without an Origin header (same-origin).
--
-- Returns:
-- - 'Right ()' if origin is valid or missing (same-origin)
-- - 'Left errorMessage' if origin is present but not allowed
validateOrigin ::
  ( MonadIO m,
    Log.MonadLog m,
    MonadReader env m,
    Has Hostname env
  ) =>
  Maybe Origin ->
  m (Either Text ())
validateOrigin mOrigin = do
  hostname <- asks (Has.getter @Hostname)
  case mOrigin of
    Nothing -> do
      -- No Origin header - likely same-origin request
      -- This is OK for POST requests from the same origin
      pure $ Right ()
    Just origin -> do
      if isAllowedOrigin origin hostname
        then pure $ Right ()
        else do
          Log.logInfo "CSRF: Origin not allowed" (Aeson.object ["origin" .= display origin])
          pure $ Left "Invalid request origin"

-- | Strict validation: requires Origin header to be present.
--
-- Use this for endpoints that should only accept cross-origin XHR requests.
--
-- Returns:
-- - 'Right ()' if origin is valid
-- - 'Left errorMessage' if origin is missing or not allowed
validateOriginStrict ::
  ( MonadIO m,
    Log.MonadLog m,
    MonadReader env m,
    Has Hostname env
  ) =>
  Maybe Origin ->
  m (Either Text ())
validateOriginStrict mOrigin = do
  hostname <- asks (Has.getter @Hostname)
  case mOrigin of
    Nothing -> do
      Log.logInfo "CSRF: Origin header missing (strict mode)" ("" :: Text)
      pure $ Left "Origin header required"
    Just origin -> do
      if isAllowedOrigin origin hostname
        then pure $ Right ()
        else do
          Log.logInfo "CSRF: Origin not allowed" (Aeson.object ["origin" .= display origin])
          pure $ Left "Invalid request origin"

--------------------------------------------------------------------------------
-- Configuration

-- | Build the list of allowed origins from site URLs.
--
-- Pass all origins that should be trusted (e.g., production URL,
-- staging URL, etc.). Same-origin requests without an Origin header
-- are always allowed by 'validateOrigin'.
allowedOrigins :: [Text] -> [Origin]
allowedOrigins = map mkOrigin

-- | WAI middleware that replaces Servant's non-HTML 404 responses
-- with a styled HTML page.
--
-- Servant's router returns 404 responses when no route matches (either
-- empty body with no content type, or @text/plain@). This middleware
-- intercepts those and replaces them with a pre-rendered HTML page that
-- includes the full site frame (header, nav, footer, music player).
--
-- Only intercepts 404 responses that are NOT @text/html@, so handler-level
-- 404s that already return styled HTML are passed through unchanged.
module Middleware.NotFound
  ( notFoundMiddleware,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Network.HTTP.Types.Header qualified as Header
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Middleware that replaces non-HTML 404 responses with a styled HTML page.
--
-- Takes a pre-rendered HTML page as a lazy 'LBS.ByteString' (rendered once at
-- startup) and substitutes it for any 404 response that does not already have
-- a @text/html@ content type (to avoid replacing handler-level 404s).
notFoundMiddleware :: LBS.ByteString -> Wai.Middleware
notFoundMiddleware htmlBody app req respond =
  app req $ \response ->
    if isNonHtml404 response
      then respond $ Wai.responseLBS Status.status404 htmlHeaders htmlBody
      else respond response

-- | Check if a response is a non-HTML 404 (Servant routing miss).
--
-- Returns 'True' for 404 responses that don't have a @text/html@ content type,
-- which covers both Servant's empty-body routing misses (no content type) and
-- plain-text error responses.
isNonHtml404 :: Wai.Response -> Bool
isNonHtml404 response =
  Status.statusCode (Wai.responseStatus response) == 404
    && not (isHtmlContentType (Wai.responseHeaders response))

-- | Check if the Content-Type header indicates HTML.
isHtmlContentType :: [Header.Header] -> Bool
isHtmlContentType headers =
  case lookup Header.hContentType headers of
    Just ct -> "text/html" `BS.isPrefixOf` ct
    Nothing -> False

-- | Response headers for the HTML 404 page.
htmlHeaders :: [Header.Header]
htmlHeaders =
  [ (Header.hContentType, "text/html; charset=utf-8")
  ]

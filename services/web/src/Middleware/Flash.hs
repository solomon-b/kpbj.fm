-- | WAI middleware that deletes the flash cookie after a page shows it.
--
-- A page reads the flash message with 'App.Flash.ReadFlash' and its frame
-- renders the banner. A handler returns HTML with no headers, so this
-- middleware sends the @Set-Cookie@ that deletes the cookie.
module Middleware.Flash
  ( flashMiddleware,
    consumesFlash,
  )
where

--------------------------------------------------------------------------------

import Component.Flash (clearFlashCookie, flashCookieName, lookupFlashCookie)
import Data.ByteString qualified as ByteString
import Data.Maybe (isJust)
import Data.Text.Encoding qualified as Text.Encoding
import Domain.Types.HxRequest (HxRequest (..), hxRequestFromHeaders)
import Network.HTTP.Types.Header qualified as Header
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Delete the flash cookie when the response is a page that showed it.
flashMiddleware :: Wai.Middleware
flashMiddleware app req respond =
  app req $ \response ->
    respond $
      if consumesFlash req response
        then Wai.mapResponseHeaders ((Header.hSetCookie, Text.Encoding.encodeUtf8 clearFlashCookie) :) response
        else response

-- | Whether a response shows the flash message of its request.
--
-- Only a full page renders the frame, and so the banner:
--
-- * The request has a flash cookie.
-- * The request is not an HTMX request. An HTMX response is a fragment.
-- * The response is HTML. The status does not matter, because an error page
--   such as a 404 also renders the frame.
-- * The response is not a redirect. A redirect can be one hop of a chain,
--   and the page at the end of the chain shows the message.
-- * The response sets no flash cookie of its own, for example a redirect
--   with a new message.
consumesFlash :: Wai.Request -> Wai.Response -> Bool
consumesFlash req response =
  isJust (lookupFlashCookie requestHeaders)
    && hxRequestFromHeaders requestHeaders == IsNotHxRequest
    && any (ByteString.isPrefixOf "text/html" . snd) (headersNamed Header.hContentType)
    && null (headersNamed "HX-Redirect")
    && not (any (ByteString.isPrefixOf (flashCookieName <> "=") . snd) (headersNamed Header.hSetCookie))
  where
    requestHeaders = Wai.requestHeaders req
    headersNamed name = filter ((== name) . fst) (Wai.responseHeaders response)

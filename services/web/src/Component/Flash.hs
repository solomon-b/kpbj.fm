{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Component.Flash
  ( -- * Types
    FlashMessage (..),

    -- * Redirects
    throwHxRedirect,
    hxRedirectError,
    throwHxLocation,
    hxLocationValue,
    jsRedirectBody,

    -- * Flash cookie
    flashCookie,
    clearFlashCookie,
  )
where

--------------------------------------------------------------------------------

import Component.Banner (BannerType (..))
import Control.Monad.Catch (MonadThrow, throwM)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as LBS
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Lucid qualified
import Network.HTTP.Types.URI (urlEncode)
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Orphan instance: Servant needs to render NoContent through our HTML type
-- for POST handlers that return NoContent with headers.
instance Servant.MimeRender HTML Servant.NoContent where
  mimeRender _ _ = ""

--------------------------------------------------------------------------------

-- | Flash message data stored in a cookie for display after redirect.
data FlashMessage = FlashMessage
  { fmType :: BannerType,
    fmTitle :: Text,
    fmMessage :: Text
  }

--------------------------------------------------------------------------------

-- | Redirect via HX-Redirect header (for HTMX) with JS fallback (for direct navigation).
--
-- Returns a 200 response with:
-- - @HX-Redirect@ header: HTMX reads this and does @window.location.href@
-- - @Set-Cookie@: flash cookie with optional banner message
-- - HTML body: @\<script\>window.location.href='...'\<\/script\>@ fallback for non-HTMX
--
-- Uses @throwM@ so it works from any handler regardless of return type.
throwHxRedirect :: (MonadThrow m) => Text -> Maybe FlashMessage -> m a
throwHxRedirect url = throwM . hxRedirectError url

-- | The response that 'throwHxRedirect' throws, as a value.
--
-- Code outside a handler uses it, for example an auth handler in the
-- Servant context.
hxRedirectError :: Text -> Maybe FlashMessage -> Servant.ServerError
hxRedirectError url mFlash =
  Servant.ServerError
    { errHTTPCode = 200,
      errReasonPhrase = "OK",
      errBody = Lucid.renderBS $ jsRedirectBody url,
      errHeaders =
        [ ("Content-Type", "text/html; charset=utf-8"),
          ("HX-Redirect", Text.Encoding.encodeUtf8 url),
          ("Set-Cookie", Text.Encoding.encodeUtf8 $ flashCookie mFlash)
        ]
    }

-- | Send an HTMX client to a page by swap. The response has no body.
--
-- HTMX reads the @HX-Location@ header and loads the path with a new request.
-- It swaps the response into the target and pushes the path to the browser
-- history. @HX-Redirect@ causes a full page load.
--
-- A client that is not HTMX ignores the header. Use 'throwHxRedirect' for it.
throwHxLocation ::
  (MonadThrow m) =>
  -- | Path
  Text ->
  -- | CSS selector of the swap target
  Text ->
  -- | Swap specification, in the syntax of @hx-swap@
  Text ->
  m a
throwHxLocation path target swap =
  throwM $
    Servant.ServerError
      { errHTTPCode = 200,
        errReasonPhrase = "OK",
        errBody = "",
        errHeaders =
          [ ("Content-Type", "text/html; charset=utf-8"),
            ("HX-Location", Text.Encoding.encodeUtf8 $ hxLocationValue path target swap)
          ]
      }

-- | The value of an @HX-Location@ header. HTMX loads the path by swap into
-- the target. The default target of the header is @body@.
--
-- The new request does not come from the element that the user clicked, so
-- it does not get the @hx-swap@ of that element. The header carries the swap
-- specification.
--
-- See <https://htmx.org/headers/hx-location/>.
hxLocationValue ::
  -- | Path
  Text ->
  -- | CSS selector of the swap target
  Text ->
  -- | Swap specification, in the syntax of @hx-swap@
  Text ->
  Text
hxLocationValue path target swap =
  Text.Encoding.decodeUtf8 . LBS.toStrict . Aeson.encode $
    Aeson.object
      [ "path" Aeson..= path,
        "target" Aeson..= target,
        "swap" Aeson..= swap
      ]

-- | Minimal HTML body with JS redirect for non-HTMX requests.
--
-- The URL is escaped to prevent breaking out of the JS string literal.
jsRedirectBody :: Text -> Lucid.Html ()
jsRedirectBody url =
  let escaped = Text.replace "'" "\\'" $ Text.replace "\\" "\\\\" url
   in Lucid.script_ [i|window.location.href='#{escaped}';|]

--------------------------------------------------------------------------------

-- | Build the Set-Cookie value for a flash message.
--
-- When Nothing, clears any stale flash cookie.
flashCookie :: Maybe FlashMessage -> Text
flashCookie Nothing = clearFlashCookie
flashCookie (Just flash) =
  let json = Text.Encoding.decodeUtf8 $ LBS.toStrict $ Aeson.encode $ flashToJSON flash
      encoded = Text.Encoding.decodeUtf8 $ urlEncode True $ Text.Encoding.encodeUtf8 json
   in [i|_flash=#{encoded}; Path=/; SameSite=Strict|]

-- | Set-Cookie value that clears the flash cookie.
clearFlashCookie :: Text
clearFlashCookie = "_flash=; Max-Age=0; Path=/; SameSite=Strict"

--------------------------------------------------------------------------------

-- | Encode flash message as JSON object.
flashToJSON :: FlashMessage -> Aeson.Value
flashToJSON FlashMessage {..} =
  Aeson.object
    [ "type" Aeson..= bannerTypeText fmType,
      "title" Aeson..= fmTitle,
      "msg" Aeson..= fmMessage
    ]

bannerTypeText :: BannerType -> Text
bannerTypeText = \case
  Success -> "success"
  Error -> "error"
  Warning -> "warning"
  Info -> "info"

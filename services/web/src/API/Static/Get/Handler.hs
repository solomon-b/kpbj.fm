{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Static.Get.Handler where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile)
import Data.Kind (Type)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Tagged (Tagged (..))
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Serve an embedded static asset as a raw WAI response.
--
-- Using 'Servant.Raw' gives us full control over the response,
-- so each asset gets exactly one correct Content-Type header.
handler :: Text -> Tagged (m :: Type -> Type) Wai.Application
handler filename = Tagged $ \_req respond ->
  case Map.lookup filename staticAssets of
    Just (content, contentType) ->
      respond $
        Wai.responseLBS
          HTTP.status200
          [(HTTP.hContentType, Text.encodeUtf8 contentType)]
          (LBS.fromStrict content)
    Nothing ->
      respond $
        Wai.responseLBS
          HTTP.status404
          [(HTTP.hContentType, "text/plain")]
          "Not found"

--------------------------------------------------------------------------------

-- | Map of filename to (content, content-type).
--
-- All assets are embedded at compile time via Template Haskell.
staticAssets :: Map Text (ByteString, Text)
staticAssets =
  Map.fromList
    [ ("range.png", ($(embedFile "static/range.png"), "image/png")),
      ("alpine.min.js", ($(embedFile "static/alpine.min.js"), "application/javascript")),
      ("htmx.min.js", ($(embedFile "static/htmx.min.js"), "application/javascript")),
      ("cropper.min.js", ($(embedFile "static/cropper.min.js"), "application/javascript")),
      ("cropper.min.css", ($(embedFile "static/cropper.min.css"), "text/css"))
    ]

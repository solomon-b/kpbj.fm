{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Static.Get.Handler where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.ByteString.Lazy qualified as LBS
import Data.FileEmbed (embedFile, makeRelativeToProject)
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
    [ ("range.png", ($(makeRelativeToProject "static/range.png" >>= embedFile), "image/png")),
      ("alpine.min.js", ($(makeRelativeToProject "static/alpine.min.js" >>= embedFile), "application/javascript")),
      ("htmx.min.js", ($(makeRelativeToProject "static/htmx.min.js" >>= embedFile), "application/javascript")),
      ("cropper.min.js", ($(makeRelativeToProject "static/cropper.min.js" >>= embedFile), "application/javascript")),
      ("chart.min.js", ($(makeRelativeToProject "static/chart.min.js" >>= embedFile), "application/javascript")),
      ("cropper.min.css", ($(makeRelativeToProject "static/cropper.min.css" >>= embedFile), "text/css"))
    ]

{-# LANGUAGE OverloadedRecordDot #-}

-- | Handler for GET /api/stream/metadata.
module API.Stream.Metadata.Get.Handler
  ( handler,
  )
where

--------------------------------------------------------------------------------

import App.CustomContext (StreamConfig (..))
import App.Monad (AppM)
import Control.Exception (try)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)
import Data.Aeson (Value, object, (.=))
import Data.Has (getter)
import Data.Text qualified as Text
import Network.HTTP.Simple qualified as HTTP

--------------------------------------------------------------------------------

-- | Handler for GET /api/stream/metadata.
--
-- Proxies the icecast metadata endpoint to avoid browser CORS issues.
-- Returns the icecast JSON response directly, or a fallback on error.
handler :: AppM Value
handler = do
  streamCfg <- asks (getter @StreamConfig)

  -- Fetch from icecast
  result <- liftIO $ try @HTTP.HttpException $ do
    request <- HTTP.parseRequest (Text.unpack streamCfg.scMetadataUrl)
    response <- HTTP.httpJSON request
    pure (HTTP.getResponseBody response :: Value)

  case result of
    Left _err -> pure fallbackResponse
    Right json -> pure json

-- | Fallback response when icecast is unavailable.
fallbackResponse :: Value
fallbackResponse =
  object
    [ "icestats"
        .= object
          [ "source"
              .= object
                [ "title" .= ("KPBJ 95.9 FM" :: String),
                  "server_name" .= ("KPBJ 95.9 FM" :: String)
                ]
          ]
    ]

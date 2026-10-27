-- | Warp request logger that removes secrets before it writes a request.
--
-- The logger in @web-server-core@ writes every header and the full query
-- string. That put the playout secret, session cookies, and password-reset
-- tokens in the journal and in Loki. This logger writes the same fields, but
-- it replaces each secret value with 'redacted'.
module App.RequestLog
  ( requestLogger,
    redactHeaders,
    redactQuery,
    redacted,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Aeson.KeyMap qualified as KeyMap
import Data.Bifunctor (bimap)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as Char8
import Data.CaseInsensitive qualified as CI
import Data.Maybe (catMaybes)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Log qualified
import Network.HTTP.Types qualified as HTTP
import Network.HTTP.Types.Header qualified as Header
import Network.HTTP.Types.Status qualified as Status
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | The value that takes the place of a secret in the log.
redacted :: ByteString
redacted = "<redacted>"

-- | Request headers whose values never go to the log.
secretHeaders :: [Header.HeaderName]
secretHeaders =
  [ "authorization",
    "cookie",
    "stripe-signature",
    "x-playout-secret"
  ]

-- | Request headers that hold a full URL. The browser sends the URL of the
-- page, so a request from the password-reset page carries its token here.
urlHeaders :: [Header.HeaderName]
urlHeaders =
  [ "hx-current-url",
    "referer"
  ]

-- | Query parameters whose values never go to the log. The password-reset and
-- email-verification links carry a @token@.
secretParams :: [ByteString]
secretParams = ["token"]

-- | Replace the value of each secret header. In a URL header, replace the
-- value of each secret query parameter.
redactHeaders :: [Header.Header] -> [Header.Header]
redactHeaders = fmap redactHeader
  where
    redactHeader (name, value)
      | name `elem` secretHeaders = (name, redacted)
      | name `elem` urlHeaders = (name, redactUrl value)
      | otherwise = (name, value)

-- | Replace the value of each secret query parameter.
redactQuery :: HTTP.Query -> HTTP.Query
redactQuery = fmap redactParam
  where
    redactParam (key, value)
      | key `elem` secretParams = (key, redacted <$ value)
      | otherwise = (key, value)

-- | Replace the secret query parameters in a URL. A URL with no query does
-- not change.
redactUrl :: ByteString -> ByteString
redactUrl url =
  case Char8.break (== '?') url of
    (_, "") -> url
    (base, query) -> base <> HTTP.renderQuery True (redactQuery (HTTP.parseQuery query))

-- | Log one request. Pass it to 'Network.Wai.Handler.Warp.setLogger'.
requestLogger :: Log.LoggerEnv -> Wai.Request -> Status.Status -> Maybe Integer -> IO ()
requestLogger logEnv req s sz = do
  time <- getCurrentTime
  reqBody <- Wai.getRequestBodyChunk req
  Log.logMessageIO logEnv time Log.LogInfo "Request" $
    Aeson.toJSON $
      KeyMap.fromList $
        [ "statusCode" .= Status.statusCode s,
          "method" .= Text.Encoding.decodeUtf8 (Wai.requestMethod req),
          "httpVersion" .= show (Wai.httpVersion req),
          "path" .= Text.Encoding.decodeUtf8 (Wai.rawPathInfo req),
          "queryString" .= fmap (bimap Text.Encoding.decodeUtf8 (fmap Text.Encoding.decodeUtf8)) (redactQuery (Wai.queryString req)),
          "headers" .= fmap (bimap (Text.Encoding.decodeUtf8 . CI.foldedCase) Text.Encoding.decodeUtf8) (redactHeaders (Wai.requestHeaders req)),
          "isSecure" .= Wai.isSecure req,
          "remoteHost" .= show (Wai.remoteHost req),
          "requestBody" .= Text.Encoding.decodeUtf8 reqBody
        ]
          <> catMaybes [fmap ("requestSize" .=) sz]

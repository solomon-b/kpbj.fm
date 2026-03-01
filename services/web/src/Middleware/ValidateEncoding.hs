-- | WAI middleware that rejects requests with invalid UTF-8 in the
-- path or query string.
--
-- Exploit scanners send requests with invalid UTF-8 byte sequences
-- (e.g. @%AD@ → @\\xad@) which crash downstream decoding with
-- @Data.Text.Encoding: Invalid UTF-8 stream@. This middleware
-- percent-decodes the raw path and query, validates UTF-8, and
-- returns 400 Bad Request for any malformed input before the
-- request reaches Servant routing.
module Middleware.ValidateEncoding
  ( validateEncodingMiddleware,
    isValidUtf8,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.Text.Encoding qualified as Text
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Middleware that rejects requests containing invalid UTF-8 in the
-- path or query string with a 400 Bad Request response.
validateEncodingMiddleware :: Wai.Middleware
validateEncodingMiddleware app req respond =
  if isValidUtf8 (Wai.rawPathInfo req) && isValidUtf8 (Wai.rawQueryString req)
    then app req respond
    else respond $ Wai.responseLBS HTTP.status400 [] "Bad Request"

-- | Check whether a percent-encoded 'ByteString' decodes to valid UTF-8.
--
-- Percent-decodes the input (without plus-as-space conversion) and
-- then attempts a strict UTF-8 decode. Returns 'True' when the
-- decoded bytes form a valid UTF-8 sequence, 'False' otherwise.
isValidUtf8 :: ByteString -> Bool
isValidUtf8 bs =
  case Text.decodeUtf8' (HTTP.urlDecode False bs) of
    Right _ -> True
    Left _ -> False

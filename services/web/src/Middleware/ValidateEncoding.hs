-- | WAI middleware that catches 'UnicodeException' from any strict
-- UTF-8 decode in the request pipeline and returns 400 Bad Request.
--
-- Exploit scanners send requests with invalid UTF-8 byte sequences
-- (e.g. @%AD@ → @\\xad@) in paths, query strings, or headers. Rather
-- than validating each component individually, this middleware wraps
-- the inner application and catches any 'UnicodeException' that
-- escapes from strict 'Data.Text.Encoding.decodeUtf8' calls.
module Middleware.ValidateEncoding
  ( validateEncodingMiddleware,
  )
where

--------------------------------------------------------------------------------

import Control.Exception (catch)
import Data.Text.Encoding.Error (UnicodeException)
import Network.HTTP.Types qualified as HTTP
import Network.Wai qualified as Wai

--------------------------------------------------------------------------------

-- | Middleware that catches 'UnicodeException' from strict UTF-8
-- decoding anywhere in the request pipeline and returns 400.
validateEncodingMiddleware :: Wai.Middleware
validateEncodingMiddleware app req respond =
  app req respond `catch` handleUnicodeException
  where
    handleUnicodeException :: UnicodeException -> IO Wai.ResponseReceived
    handleUnicodeException _ = respond $ Wai.responseLBS HTTP.status400 [] "Bad Request"

-- | Custom Servant content type for capturing raw request bodies.
--
-- Stripe webhook signature verification requires access to the raw,
-- unparsed request body. This module provides a 'RawBody' content type
-- that accepts @application/json@ and @application/octet-stream@ and
-- yields the raw bytes without any parsing.
module Stripe.Servant.RawBody
  ( RawBody,
  )
where

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import Network.HTTP.Media qualified as Media
import Servant.API (Accept (..), MimeUnrender (..))

-- | A Servant content type that captures the raw request body.
--
-- Accepts both @application/json@ and @application/octet-stream@ so
-- that Stripe webhook payloads (which arrive as JSON) can be captured
-- without Servant attempting to decode them.
data RawBody
  deriving (Typeable)

instance Accept RawBody where
  contentTypes _ =
    ("application" Media.// "json" Media./: ("charset", "utf-8"))
      :| ["application" Media.// "octet-stream"]

instance MimeUnrender RawBody LBS.ByteString where
  mimeUnrender _ = Right

instance MimeUnrender RawBody BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict

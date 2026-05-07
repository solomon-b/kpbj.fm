-- | Custom Servant content type for capturing raw @x-www-form-urlencoded@
-- request bodies.
--
-- Mailchimp posts webhook events as @application/x-www-form-urlencoded@
-- with bracket-indexed keys (for example @data[email]@). The form decoder
-- in @http-api-data@ does not handle this nesting; we capture the raw bytes
-- here and parse them with "Mailchimp.Webhook" instead.
module Mailchimp.Servant.FormBody
  ( FormBody,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import Network.HTTP.Media qualified as Media
import Servant.API (Accept (..), MimeUnrender (..))

--------------------------------------------------------------------------------

-- | A Servant content type that captures the raw request body of a
-- @x-www-form-urlencoded@ POST.
--
-- Accepts @application/x-www-form-urlencoded@ as well as
-- @application/octet-stream@ so misconfigured clients are not silently
-- rejected at the transport layer.
data FormBody
  deriving (Typeable)

instance Accept FormBody where
  contentTypes _ =
    ("application" Media.// "x-www-form-urlencoded")
      :| ["application" Media.// "octet-stream"]

instance MimeUnrender FormBody LBS.ByteString where
  mimeUnrender _ = Right

instance MimeUnrender FormBody BS.ByteString where
  mimeUnrender _ = Right . LBS.toStrict

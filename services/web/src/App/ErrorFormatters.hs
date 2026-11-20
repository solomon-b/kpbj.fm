-- | The Servant error formatters of the web service.
--
-- Servant calls a formatter when it rejects a request before the handler
-- runs. The default answer is a plain text 400. HTMX shows the user only a
-- status code for it, and the JSON clients of this app cannot read it.
module App.ErrorFormatters
  ( errorFormatters,
  )
where

--------------------------------------------------------------------------------

import App.Handler.Error (HandlerError (..), inlineBannerError)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.Text qualified as Text
import Network.HTTP.Types.Header qualified as Header
import Network.Wai qualified as Wai
import Servant qualified

--------------------------------------------------------------------------------

-- | The default formatters, with a readable answer for a request body that
-- does not decode.
errorFormatters :: Servant.ErrorFormatters
errorFormatters =
  Servant.defaultErrorFormatters
    { Servant.bodyParserErrorFormatter = bodyParserErrorFormatter
    }

-- | Format the error of a form or JSON body that does not decode.
--
-- An HTMX request gets the message of the decoder in an out-of-band error
-- banner, and the form stays on the page. This covers URL-encoded and
-- multipart forms.
--
-- A JSON request gets a 400 with the message in an @error@ field. This is
-- the error shape that the JSON handlers of the store send, and the checkout
-- page reads it.
--
-- Every other request gets the default plain text 400.
bodyParserErrorFormatter :: Servant.ErrorFormatter
bodyParserErrorFormatter typeRep request message
  | isHxRequest request = inlineBannerError (ValidationError (Text.pack message))
  | isJsonRequest request =
      Servant.err400
        { Servant.errBody = Aeson.encode (Aeson.object ["error" .= message]),
          Servant.errHeaders = [(Header.hContentType, "application/json")]
        }
  | otherwise = Servant.bodyParserErrorFormatter Servant.defaultErrorFormatters typeRep request message

isHxRequest :: Wai.Request -> Bool
isHxRequest request = lookup "HX-Request" (Wai.requestHeaders request) == Just "true"

isJsonRequest :: Wai.Request -> Bool
isJsonRequest request =
  maybe False ("application/json" `BS.isPrefixOf`) $
    lookup Header.hContentType (Wai.requestHeaders request)

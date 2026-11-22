module Domain.Types.HxRequest where

--------------------------------------------------------------------------------

import Data.Maybe (fromMaybe)
import Network.HTTP.Types.Header (RequestHeaders)
import Servant qualified

--------------------------------------------------------------------------------

data HxRequest = IsHxRequest | IsNotHxRequest
  deriving (Show, Eq)

instance Servant.FromHttpApiData HxRequest where
  parseQueryParam = \case
    "true" -> Right IsHxRequest
    _ -> Right IsNotHxRequest

foldHxReq :: Maybe HxRequest -> HxRequest
foldHxReq = fromMaybe IsNotHxRequest

-- | The 'HxRequest' of a request, from its headers.
--
-- It gives the same answer as the @HX-Request@ header of a route, for code
-- that has the raw headers, such as a middleware.
hxRequestFromHeaders :: RequestHeaders -> HxRequest
hxRequestFromHeaders headers =
  foldHxReq (either (const Nothing) Just . Servant.parseHeader =<< lookup "HX-Request" headers)

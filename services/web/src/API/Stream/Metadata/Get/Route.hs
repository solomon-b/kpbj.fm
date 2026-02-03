-- | Route definition for GET /api/stream/metadata.
module API.Stream.Metadata.Get.Route
  ( Route,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Servant qualified

--------------------------------------------------------------------------------

-- | Proxy route for icecast metadata to avoid CORS issues.
--
-- Fetches metadata from icecast and returns it to the browser.
type Route = "api" Servant.:> "stream" Servant.:> "metadata" Servant.:> Servant.Get '[Servant.JSON] Value

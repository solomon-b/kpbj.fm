module Domain.Types.HxRedirect where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Data.Text (Text)
import Servant qualified

--------------------------------------------------------------------------------

-- | Newtype for HX-Redirect response headers.
--
-- Used for HTMX redirect responses that trigger full page navigation.
newtype HxRedirect = HxRedirect Text
  deriving stock (Show)
  deriving newtype (Eq, IsString, Servant.ToHttpApiData)

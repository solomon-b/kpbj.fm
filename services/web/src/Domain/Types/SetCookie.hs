module Domain.Types.SetCookie where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Data.Text (Text)
import Servant qualified

--------------------------------------------------------------------------------

-- | Newtype for Set-Cookie response headers.
--
-- Used for type-safe cookie handling in authentication flows.
newtype SetCookie = SetCookie Text
  deriving stock (Show)
  deriving newtype (Eq, IsString, Servant.ToHttpApiData)

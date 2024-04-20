module Domain.Types.Email where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import GHC.Generics
import Servant qualified

--------------------------------------------------------------------------------

-- TODO: Use case-insensitive:
newtype EmailAddress = EmailAddress {emailAddress :: Text}
  deriving stock (Show, Generic, Eq)
  deriving newtype (Servant.FromHttpApiData, FromJSON, ToJSON)

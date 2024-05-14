module Domain.Types.Email where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), ToJSON, (.:))
import Data.Aeson qualified as Aeson
import Data.CaseInsensitive (CI)
import Data.CaseInsensitive qualified as CI
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Text.Internal.Builder qualified as Text
import GHC.Generics
import Servant qualified

--------------------------------------------------------------------------------

newtype EmailAddress = EmailAddress {emailAddress :: CI Text}
  deriving stock (Show, Generic, Eq)

instance FromJSON EmailAddress where
  parseJSON = Aeson.withObject "EmailAddress" $ \o -> do
    email <- o .: "emailAddress"
    pure $ EmailAddress $ CI.mk email

instance ToJSON EmailAddress where
  toJSON EmailAddress {..} = Aeson.object [("emailAddress", Aeson.String (CI.original emailAddress))]

instance Display EmailAddress where
  displayBuilder EmailAddress {..} = Text.fromText (CI.original emailAddress)

instance Servant.FromHttpApiData EmailAddress where
  parseUrlPiece = Right . EmailAddress . CI.mk
  parseQueryParam = Right . EmailAddress . CI.mk

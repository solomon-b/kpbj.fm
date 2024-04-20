module Domain.Types.AdminStatus where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..))
import Data.Aeson.Types qualified as Aeson
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

data AdminStatus = IsAdmin | IsNotAdmin
  deriving stock (Show, Generic, Eq)

instance FromJSON AdminStatus where
  parseJSON = Aeson.withBool "IsAdmin" $ \case
    True -> pure IsAdmin
    False -> pure IsNotAdmin

isAdmin :: AdminStatus -> Bool
isAdmin = \case
  IsAdmin -> True
  IsNotAdmin -> False

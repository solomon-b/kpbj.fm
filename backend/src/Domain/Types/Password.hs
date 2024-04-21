module Domain.Types.Password where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.Text.Display (Display)
import GHC.Generics

--------------------------------------------------------------------------------

newtype Password = Password Text
  deriving stock (Show, Generic, Eq)
  deriving newtype (FromJSON, ToJSON, Display)

module Domain.Types.Genre where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Display (Display)
import Hasql.Interpolate (EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

newtype Genre = Genre Text
  deriving stock (Show)
  deriving newtype (Eq, Display, Servant.ToHttpApiData, Servant.FromHttpApiData, IsString, EncodeValue)

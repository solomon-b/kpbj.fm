module Domain.Types.Search where

--------------------------------------------------------------------------------

import Data.String (IsString)
import Data.Text (Text)
import Data.Text.Display (Display)
import Hasql.Interpolate (EncodeValue)
import Servant qualified

--------------------------------------------------------------------------------

newtype Search = Search Text
  deriving stock (Show)
  deriving newtype (Eq, Display, Semigroup, Servant.ToHttpApiData, Servant.FromHttpApiData, IsString, EncodeValue)

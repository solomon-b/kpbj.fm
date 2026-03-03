module Domain.Types.PageNumber where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text.Display (Display)
import Servant qualified

--------------------------------------------------------------------------------

newtype PageNumber = PageNumber Int64
  deriving stock (Show)
  deriving newtype (Display, Eq, Ord, Num, Servant.ToHttpApiData, Servant.FromHttpApiData)

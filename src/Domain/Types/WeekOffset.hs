module Domain.Types.WeekOffset where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text.Display (Display)
import Servant qualified

--------------------------------------------------------------------------------

newtype WeekOffset = WeekOffset Int64
  deriving stock (Show)
  deriving newtype (Display, Eq, Ord, Num, Servant.ToHttpApiData, Servant.FromHttpApiData)

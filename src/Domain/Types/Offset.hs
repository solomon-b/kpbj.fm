module Domain.Types.Offset (Offset) where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text.Display (Display)
import Hasql.Interpolate (DecodeValue, EncodeValue)

--------------------------------------------------------------------------------

newtype Offset = Offset Int64
  deriving stock (Show)
  deriving newtype (Display, Eq, Ord, Num, Enum, Real, Integral, DecodeValue, EncodeValue)

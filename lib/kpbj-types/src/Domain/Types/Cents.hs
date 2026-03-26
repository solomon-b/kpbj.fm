module Domain.Types.Cents
  ( Cents (..),
    dollarsToCents,
    centsToDollars,
    formatDisplay,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Int (Int64)
import Data.Scientific (FPFormat (..), formatScientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (Display (..))
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
import Rel8 (DBEq, DBOrd, DBType)
import Servant qualified

--------------------------------------------------------------------------------

-- | Price in cents. Stored as integer cents to avoid floating-point issues.
--
-- Note: The 'Num' instance allows negative values (useful for refunds or
-- adjustments). Non-negativity for price columns is enforced at the database
-- level via @CHECK (... >= 0)@ constraints.
newtype Cents = Cents {unCents :: Int64}
  deriving stock (Generic)
  deriving newtype (Show, Eq, Ord, Num, DBType, DBEq, DBOrd)
  deriving newtype (DecodeValue, EncodeValue)
  deriving newtype (ToJSON, FromJSON, Display)
  deriving newtype (Servant.FromHttpApiData, Servant.ToHttpApiData)

-- | Convert a dollar-denominated price to whole cents.
dollarsToCents :: RealFrac a => a -> Cents
dollarsToCents = Cents . round . (* 100)

-- | Get a cents price as a fraction of dollars.
centsToDollars :: Fractional a => Cents -> a
centsToDollars = (/ 100) . fromIntegral . unCents

-- | Format 'Cents' for display with "$" prefix (e.g. "$24.99").
formatDisplay :: Cents -> Text
formatDisplay c =
  "$" <> Text.pack (formatScientific Fixed (Just 2) (centsToDollars c))

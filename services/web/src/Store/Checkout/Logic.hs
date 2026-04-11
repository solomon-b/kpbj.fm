module Store.Checkout.Logic
  ( computeSubtotal,
    computeTax,
    computeTotal,
    maskEmail,
    formatOrderNumber,
    easypostRateToCents,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cents (Cents (..))

--------------------------------------------------------------------------------

-- | Compute subtotal from a list of (unit_price, quantity) pairs.
computeSubtotal :: [(Cents, Int64)] -> Cents
computeSubtotal = Cents . sum . fmap (\(Cents p, q) -> p * q)


-- | Compute tax as round(subtotal_cents * tax_rate).
--
-- tax_rate is a decimal fraction — e.g. 0.095 means 9.5%. Do NOT pass a
-- percentage integer here.
computeTax :: Cents -> Scientific -> Cents
computeTax (Cents subtotal) taxRate =
  Cents $ round (fromIntegral subtotal * taxRate :: Scientific)


-- | Sum subtotal + shipping + tax to produce the order total.
computeTotal :: Cents -> Cents -> Cents -> Cents
computeTotal (Cents sub) (Cents ship) (Cents tax) = Cents (sub + ship + tax)


-- | Mask an email address for safe public display.
--
-- Shows only the first character of the local part, hiding the rest.
-- Examples:
--   "jane@example.com" → "j***@example.com"
--   "a@b.com"          → "a***@b.com"
--   "noatsign"         → "n***"
maskEmail :: Text -> Text
maskEmail email =
  case Text.breakOn "@" email of
    (local, domain)
      | Text.null domain -> Text.take 1 local <> "***"
      | otherwise -> Text.take 1 local <> "***" <> domain


-- | Format an order number from a sequence value.
--
-- Pads to at least 4 digits with leading zeros.
-- Examples:
--   1     → "KPBJ-0001"
--   9999  → "KPBJ-9999"
--   10000 → "KPBJ-10000"
formatOrderNumber :: Int64 -> Text
formatOrderNumber n =
  let padded = Text.pack (show n)
      width = max 4 (Text.length padded)
      zeroPadded = Text.justifyRight width '0' padded
  in "KPBJ-" <> zeroPadded


-- | Parse an EasyPost rate string (e.g. "7.58") to whole cents (758).
--
-- Returns 'Nothing' if the string is not a valid decimal number.
easypostRateToCents :: Text -> Maybe Cents
easypostRateToCents rateText =
  case reads (Text.unpack rateText) :: [(Double, String)] of
    [(d, "")] -> Just $ Cents $ round (d * 100)
    _ -> Nothing

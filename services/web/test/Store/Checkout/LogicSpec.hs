module Store.Checkout.LogicSpec (spec) where

--------------------------------------------------------------------------------

import Data.Scientific (Scientific)
import Domain.Types.Cents (Cents (..))
import Store.Checkout.Logic
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Store.Checkout.Logic" $ do
  describe "computeSubtotal" $ do
    it "returns zero for an empty list" $
      computeSubtotal [] `shouldBe` Cents 0

    it "computes price * quantity for a single item" $
      -- $25.00 * 2 = $50.00
      computeSubtotal [(Cents 2500, 2)] `shouldBe` Cents 5000

    it "sums multiple items correctly" $
      -- $10.00 * 3 = $30.00, $5.99 * 1 = $5.99, total = $35.99
      computeSubtotal [(Cents 1000, 3), (Cents 599, 1)] `shouldBe` Cents 3599

  describe "computeTax" $ do
    it "computes tax by rounding subtotal * rate" $
      -- $35.00 * 0.095 = $3.325 → banker's rounds to $3.32 (rounds to even)
      computeTax (Cents 3500) (0.095 :: Scientific) `shouldBe` Cents 332

    it "returns zero tax for a zero rate" $
      computeTax (Cents 3500) (0.0 :: Scientific) `shouldBe` Cents 0

    it "returns zero tax on a zero subtotal" $
      computeTax (Cents 0) (0.095 :: Scientific) `shouldBe` Cents 0

  describe "computeTotal" $ do
    it "sums subtotal, shipping, and tax" $
      -- $35.99 + $7.58 + $3.33 = $46.90
      computeTotal (Cents 3599) (Cents 758) (Cents 333) `shouldBe` Cents 4690

    it "works when shipping and tax are zero" $
      computeTotal (Cents 5000) (Cents 0) (Cents 0) `shouldBe` Cents 5000

  describe "maskEmail" $ do
    it "masks all but first char of local part" $
      maskEmail "jane@example.com" `shouldBe` "j***@example.com"

    it "handles a single-character local part" $
      maskEmail "a@b.com" `shouldBe` "a***@b.com"

    it "handles input with no @ sign" $
      maskEmail "noatsign" `shouldBe` "n***"

  describe "formatOrderNumber" $ do
    it "pads single-digit numbers to 4 digits" $
      formatOrderNumber 1 `shouldBe` "KPBJ-0001"

    it "pads three-digit numbers to 4 digits" $
      formatOrderNumber 42 `shouldBe` "KPBJ-0042"

    it "does not truncate a 4-digit number" $
      formatOrderNumber 9999 `shouldBe` "KPBJ-9999"

    it "allows growth beyond 4 digits without truncation" $
      formatOrderNumber 10000 `shouldBe` "KPBJ-10000"

  describe "easypostRateToCents" $ do
    it "parses a decimal rate string to cents" $
      easypostRateToCents "7.58" `shouldBe` Just (Cents 758)

    it "parses a round dollar amount" $
      easypostRateToCents "5.00" `shouldBe` Just (Cents 500)

    it "returns Nothing for a non-numeric string" $
      easypostRateToCents "abc" `shouldBe` Nothing

    it "returns Nothing for an empty string" $
      easypostRateToCents "" `shouldBe` Nothing

    it "returns Nothing for a string with trailing garbage" $
      easypostRateToCents "7.58abc" `shouldBe` Nothing

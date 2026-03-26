module Domain.Types.CentsSpec (spec) where

--------------------------------------------------------------------------------

import Data.Scientific (Scientific)
import Domain.Types.Cents (Cents (..), centsToDollars, dollarsToCents, formatDisplay)
import Hedgehog (assert, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Domain.Types.Cents" $ do
  describe "dollarsToCents" $ do
    it "converts whole dollars" $ do
      dollarsToCents (10 :: Scientific) `shouldBe` Cents 1000
      dollarsToCents (0 :: Scientific) `shouldBe` Cents 0
      dollarsToCents (1 :: Scientific) `shouldBe` Cents 100

    it "converts dollars and cents" $ do
      dollarsToCents (24.99 :: Scientific) `shouldBe` Cents 2499
      dollarsToCents (0.01 :: Scientific) `shouldBe` Cents 1
      dollarsToCents (0.50 :: Scientific) `shouldBe` Cents 50
      dollarsToCents (99.99 :: Scientific) `shouldBe` Cents 9999

    it "rounds half-cents to nearest" $ do
      dollarsToCents (10.075 :: Scientific) `shouldBe` Cents 1008
      dollarsToCents (10.005 :: Scientific) `shouldBe` Cents 1000
      dollarsToCents (10.995 :: Scientific) `shouldBe` Cents 1100

    it "handles exact decimal values that are tricky for Double" $ do
      -- Scientific represents these exactly, unlike IEEE 754
      dollarsToCents (0.10 :: Scientific) `shouldBe` Cents 10
      dollarsToCents (0.20 :: Scientific) `shouldBe` Cents 20
      dollarsToCents (0.30 :: Scientific) `shouldBe` Cents 30

  describe "centsToDollars" $ do
    it "converts to exact Scientific values" $ do
      (centsToDollars (Cents 2499) :: Scientific) `shouldBe` 24.99
      (centsToDollars (Cents 0) :: Scientific) `shouldBe` 0
      (centsToDollars (Cents 1) :: Scientific) `shouldBe` 0.01
      (centsToDollars (Cents 50) :: Scientific) `shouldBe` 0.50
      (centsToDollars (Cents 100) :: Scientific) `shouldBe` 1.00

  describe "formatDisplay" $ do
    it "formats with $ prefix and two decimal places" $ do
      formatDisplay (Cents 2499) `shouldBe` "$24.99"
      formatDisplay (Cents 0) `shouldBe` "$0.00"
      formatDisplay (Cents 1) `shouldBe` "$0.01"
      formatDisplay (Cents 100) `shouldBe` "$1.00"
      formatDisplay (Cents 50) `shouldBe` "$0.50"
      formatDisplay (Cents 1000) `shouldBe` "$10.00"

  describe "round-trip: dollarsToCents . centsToDollars" $ do
    it "is identity for non-negative cents" $ hedgehog $ do
      cents <- forAll $ Gen.integral (Range.linear 0 10000000)
      let c = Cents cents
          roundTripped = dollarsToCents (centsToDollars c :: Scientific)
      roundTripped === c

  describe "round-trip: centsToDollars . dollarsToCents" $ do
    it "preserves two-decimal-place Scientific values" $ hedgehog $ do
      -- Generate cents values and convert to Scientific dollars
      cents <- forAll $ Gen.integral (Range.linear 0 10000000)
      let dollars = centsToDollars (Cents cents) :: Scientific
          roundTripped = centsToDollars (dollarsToCents dollars) :: Scientific
      roundTripped === dollars

  describe "Num instance" $ do
    it "supports addition" $ do
      Cents 100 + Cents 250 `shouldBe` Cents 350

    it "supports multiplication" $ do
      Cents 500 * 3 `shouldBe` Cents 1500

    it "addition is commutative" $ hedgehog $ do
      a <- forAll $ Gen.integral (Range.linear 0 10000000)
      b <- forAll $ Gen.integral (Range.linear 0 10000000)
      Cents a + Cents b === Cents b + Cents a

    it "addition is associative" $ hedgehog $ do
      a <- forAll $ Gen.integral (Range.linear 0 1000000)
      b <- forAll $ Gen.integral (Range.linear 0 1000000)
      c <- forAll $ Gen.integral (Range.linear 0 1000000)
      (Cents a + Cents b) + Cents c === Cents a + (Cents b + Cents c)

    it "zero is additive identity" $ hedgehog $ do
      a <- forAll $ Gen.integral (Range.linear 0 10000000)
      Cents a + Cents 0 === Cents a
      Cents 0 + Cents a === Cents a

  describe "dollarsToCents properties" $ do
    it "result scales linearly with input" $ hedgehog $ do
      -- dollarsToCents (a + b) == dollarsToCents a + dollarsToCents b
      -- (for exact Scientific values that are multiples of 0.01)
      a <- forAll $ Gen.integral (Range.linear 0 1000000)
      b <- forAll $ Gen.integral (Range.linear 0 1000000)
      let aDollars = centsToDollars (Cents a) :: Scientific
          bDollars = centsToDollars (Cents b) :: Scientific
      dollarsToCents (aDollars + bDollars) === dollarsToCents aDollars + dollarsToCents bDollars

    it "multiplying dollars by 100 gives cents" $ hedgehog $ do
      cents <- forAll $ Gen.integral (Range.linear 0 10000000)
      let dollars = centsToDollars (Cents cents) :: Scientific
      -- dollarsToCents is round . (* 100), so for exact values:
      dollarsToCents dollars === Cents cents

    it "is non-negative for non-negative input" $ hedgehog $ do
      dollars <- forAll $ Gen.double (Range.linearFrac 0 100000)
      let Cents c = dollarsToCents dollars
      assert (c >= 0)

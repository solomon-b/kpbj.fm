-- | Tests for the count of missed slots per host.
module Domain.AttendanceSpec where

{- HLINT ignore "Monoid law, left identity" -}
{- HLINT ignore "Monoid law, right identity" -}

--------------------------------------------------------------------------------

import Data.List (sortOn)
import Data.Ord (Down (..))
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime, addUTCTime, fromGregorian, nominalDay)
import Domain.Attendance
import Domain.Types.DisplayName (mkDisplayNameUnsafe)
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Tables.ShowSchedule (PastSlot (..), PastSlotHost (..), SlotOutcome (..), TemplateId (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hedgehog (Gen, forAll, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Servant (parseUrlPiece, toUrlPiece)
import Test.Gen.DomainTypes (genDisplayName)
import Test.Hspec
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------
-- Generators

-- | 14:00 Pacific on January 6, 2025.
slotStart :: UTCTime
slotStart = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay 14 0 0))

genTally :: Gen Tally
genTally = do
  total <- Gen.int (Range.linear 0 500)
  missed <- Gen.int (Range.linear 0 total)
  pure (Tally missed total)

-- | A pool of hosts with different ids.
genHosts :: Gen [PastSlotHost]
genHosts = do
  count <- Gen.int (Range.linear 1 6)
  traverse (\n -> PastSlotHost (User.Id (fromIntegral n)) <$> genDisplayName) [1 .. count]

-- | A slot of one of three shows, with some hosts from the pool.
genSlot :: [PastSlotHost] -> Gen PastSlot
genSlot hosts = do
  (showId, title) <- Gen.element [(1, "Show A"), (2, "Show B"), (3, "Show C")]
  outcome <- Gen.element [Aired, Missed]
  slotHosts <- Gen.subsequence hosts
  pure (slotOf showId title outcome slotHosts)

slotOf :: Integer -> Text -> SlotOutcome -> [PastSlotHost] -> PastSlot
slotOf showId title outcome hosts =
  PastSlot
    { psTemplateId = TemplateId (fromInteger showId),
      psShowId = Shows.Id (fromInteger showId),
      psShowTitle = title,
      psShowSlug = Slug "test-show",
      psAirDate = fromGregorian 2025 1 6,
      psStart = slotStart,
      psEnd = addUTCTime 7200 slotStart,
      psOutcome = outcome,
      psHosts = hosts
    }

genSlots :: Gen [PastSlot]
genSlots = do
  hosts <- genHosts
  Gen.list (Range.linear 0 40) (genSlot hosts)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Domain.Attendance" $ do
    describe "AttendanceRange" $ do
      it "reads back the URL piece of each range" $
        mapM_ (\range -> parseUrlPiece (toUrlPiece range) `shouldBe` Right range) [minBound .. maxBound :: AttendanceRange]

      it "starts 90 days before now" $
        rangeStart slotStart Range90d `shouldBe` Just (addUTCTime (negate (90 * nominalDay)) slotStart)

      it "has no start for the range of all time" $
        rangeStart slotStart RangeAll `shouldBe` Nothing

    describe "Tally" $ do
      it "is associative" $ hedgehog $ do
        a <- forAll genTally
        b <- forAll genTally
        c <- forAll genTally
        (a <> b) <> c === a <> (b <> c)

      it "has mempty as its identity" $ hedgehog $ do
        a <- forAll genTally
        mempty <> a === a
        a <> mempty === a

      it "counts a missed slot as 1 of 1 and an aired slot as 0 of 1" $ do
        tallySlot (slotOf 1 "Show A" Missed []) `shouldBe` Tally 1 1
        tallySlot (slotOf 1 "Show A" Aired []) `shouldBe` Tally 0 1

    describe "missedPercent" $ do
      it "is Nothing for no slots" $
        missedPercent mempty `shouldBe` Nothing

      it "rounds to a whole percent" $ do
        missedPercent (Tally 3 20) `shouldBe` Just 15
        missedPercent (Tally 1 3) `shouldBe` Just 33
        missedPercent (Tally 0 4) `shouldBe` Just 0

    describe "hostAttendance" $ do
      it "counts each slot one time for each of its hosts" $ hedgehog $ do
        slots <- forAll genSlots
        foldMap (.haTally) (hostAttendance slots)
          === foldMap (\slot -> mconcat (tallySlot slot <$ slot.psHosts)) slots

      it "gives each host the tally of the slots of that host" $ hedgehog $ do
        slots <- forAll genSlots
        let hosts = hostAttendance slots
        fmap (.haTally) hosts
          === fmap (\host -> foldMap tallySlot (slotsOfHost host.haUserId slots)) hosts

      it "puts the host with the most missed slots first" $ hedgehog $ do
        slots <- forAll genSlots
        let missed = fmap (.haTally.tMissed) (hostAttendance slots)
        missed === sortOn Down missed

      it "names the shows of the slots of a host" $ do
        let host = PastSlotHost (User.Id 1) (mkDisplayNameUnsafe "Host One")
            other = PastSlotHost (User.Id 2) (mkDisplayNameUnsafe "Host Two")
            slots =
              [ slotOf 1 "Show A" Missed [host, other],
                slotOf 2 "Show B" Aired [host],
                slotOf 3 "Show C" Aired []
              ]
        hostAttendance slots
          `shouldBe` [ HostAttendance (User.Id 1) (mkDisplayNameUnsafe "Host One") (Set.fromList ["Show A", "Show B"]) (Tally 1 2),
                       HostAttendance (User.Id 2) (mkDisplayNameUnsafe "Host Two") (Set.fromList ["Show A"]) (Tally 1 1)
                     ]

    describe "missedSlots" $
      it "keeps the missed slots only" $
        hedgehog $ do
          slots <- forAll genSlots
          let total = foldMap tallySlot slots
              missed = total.tMissed
          foldMap tallySlot (missedSlots slots) === Tally missed missed

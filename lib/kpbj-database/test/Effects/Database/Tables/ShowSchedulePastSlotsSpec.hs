{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.ShowSchedulePastSlotsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek (..), LocalTime (..), NominalDiffTime, TimeOfDay (..), UTCTime, addDays, addUTCTime, fromGregorian)
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.ShowSchedule qualified as UUT
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (interp, sql)
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (addTestShowHost, insertTestEpisode, insertTestShowWithSchedule, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.EmailAddress (mkUniqueEmail)
import Test.Gen.Tables.Episodes (episodeInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ShowSchedule.getPastSlots" $ do
      runs 10 . it "a slot is Aired when an episode of the show started in it" $ hedgehog . prop_airedSlot
      runs 10 . it "a slot with no airing, or with a rerun, is Missed" $ hedgehog . prop_missedSlot
      runs 10 . it "an episode of another show does not make a slot Aired" $ hedgehog . prop_otherShowEpisode
      runs 10 . it "a slot holds only the hosts of the show when it opened" $ hedgehog . prop_hostsWhenTheSlotOpened
      runs 10 . it "no slot comes from before the first episode air record" $ hedgehog . prop_recordStart
      runs 10 . it "a slot that is not finished gives no row, and a replay gives no row" $ hedgehog . prop_unfinishedAndReplay
      runs 10 . it "a slot with no air record in its window gives no row" $ hedgehog . prop_noAirRecordInWindow
      runs 10 . it "a deleted show gives no row" $ hedgehog . prop_deletedShow

--------------------------------------------------------------------------------
-- Fixtures

-- | Monday, January 6, 2025. Every test uses the slot of this date.
slotTestDay :: Day
slotTestDay = fromGregorian 2025 1 6

-- | A Pacific clock time on 'slotTestDay', or on a later or earlier date.
slotTestTime :: Integer -> TimeOfDay -> UTCTime
slotTestTime daysLater time = pacificToUtc (LocalTime (addDays daysLater slotTestDay) time)

-- | The start of 'slotTestDay'.
dayStart :: UTCTime
dayStart = slotTestTime 0 (TimeOfDay 0 0 0)

-- | The end of 'slotTestDay'.
dayEnd :: UTCTime
dayEnd = slotTestTime 1 (TimeOfDay 0 0 0)

data Fixture = Fixture
  { fUserId :: User.Id,
    fShowId :: Shows.Id,
    fTemplateId :: UUT.TemplateId,
    -- | The episode of the show for 'slotTestDay'.
    fEpisodeId :: Episodes.Id
  }

-- | Insert a show with a Monday 14:00 to 16:00 slot, one host, and an episode
-- for 'slotTestDay'. The slot is valid and the host is on the show from
-- December 1, 2024.
--
-- The fixture also starts the air record: the episode has an @episode@ row at
-- 15:00 on the Monday before 'slotTestDay'. And it puts fill in the slot of
-- 'slotTestDay': an @ephemeral@ row at the start of the window.
insertFixture ::
  UserMetadata.UserWithMetadataInsert ->
  Shows.Insert ->
  Episodes.Insert ->
  -- | The replay start time of the slot
  Maybe TimeOfDay ->
  TRX.Transaction Fixture
insertFixture userWithMetadata showInsert episodeTemplate replayStart = do
  userId <- insertTestUser userWithMetadata
  let template =
        UUT.ScheduleTemplateInsert
          { stiShowId = Shows.Id 1,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 16 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = replayStart
          }
  (showId, templateId) <- insertTestShowWithSchedule showInsert {Shows.siStatus = Shows.Active} template
  _ <- unwrapInsert $ UUT.insertValidity $ UUT.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
  addTestShowHost showId userId
  setHostDates showId userId (slotTestTime (-36) (TimeOfDay 0 0 0)) Nothing
  episodeId <-
    insertTestEpisode
      episodeTemplate
        { Episodes.eiId = showId,
          Episodes.eiScheduleTemplateId = Just templateId,
          Episodes.eiAirDate = Just slotTestDay,
          Episodes.eiCreatedBy = userId,
          Episodes.eiAudioFilePath = Just "audio/episodes/2025/01/06/past-slot.mp3",
          Episodes.eiDurationSeconds = Just 7080
        }
  insertAiring "episode" episodeId (slotTestTime (-7) (TimeOfDay 15 0 0))
  insertFill (slotTestTime 0 (TimeOfDay 14 0 0))
  pure Fixture {fUserId = userId, fShowId = showId, fTemplateId = templateId, fEpisodeId = episodeId}

-- | Set when a host joined a show and when the host left it.
setHostDates :: Shows.Id -> User.Id -> UTCTime -> Maybe UTCTime -> TRX.Transaction ()
setHostDates showId userId joinedAt leftAt =
  TRX.statement () $
    interp
      False
      [sql|
        UPDATE show_hosts
        SET joined_at = #{joinedAt}, left_at = #{leftAt}
        WHERE show_id = #{showId} AND user_id = #{userId}
      |]

-- | Record that an episode started on air, with the given source type.
insertAiring :: Text -> Episodes.Id -> UTCTime -> TRX.Transaction ()
insertAiring sourceType episodeId startedAt =
  TRX.statement () $
    PlaybackHistory.insertPlayback
      PlaybackHistory.Insert
        { piTitle = "Past slot test",
          piArtist = Nothing,
          piSourceType = sourceType,
          piSourceUrl = "audio/episodes/2025/01/06/past-slot.mp3",
          piEpisodeId = Just (Episodes.unId episodeId),
          piBreakItemId = Nothing,
          piStartedAt = startedAt,
          piPlannedStart = Nothing
        }

-- | Record that fill started on air.
insertFill :: UTCTime -> TRX.Transaction ()
insertFill startedAt =
  TRX.statement () $
    PlaybackHistory.insertPlayback
      PlaybackHistory.Insert
        { piTitle = "Past slot test fill",
          piArtist = Nothing,
          piSourceType = "ephemeral",
          piSourceUrl = "audio/ephemeral/past-slot.mp3",
          piEpisodeId = Nothing,
          piBreakItemId = Nothing,
          piStartedAt = startedAt,
          piPlannedStart = Nothing
        }

-- | Delete every @ephemeral@ row of @playback_history@.
clearFill :: TRX.Transaction ()
clearFill = TRX.statement () statement
  where
    statement :: Hasql.Statement () ()
    statement = interp False [sql|DELETE FROM playback_history WHERE source_type = 'ephemeral'|]

-- | Delete every @playback_history@ row.
clearAirRecord :: TRX.Transaction ()
clearAirRecord = TRX.statement () statement
  where
    statement :: Hasql.Statement () ()
    statement = interp False [sql|DELETE FROM playback_history|]

-- | The slots of one show on 'slotTestDay'.
slotsOfShow :: Shows.Id -> TRX.Transaction [UUT.PastSlot]
slotsOfShow showId =
  filter ((== showId) . UUT.psShowId) <$> TRX.statement () (UUT.getPastSlots (Just dayStart) dayEnd)

-- | The slot of the fixture on 'slotTestDay'.
expectedSlot :: Fixture -> Shows.Insert -> UUT.SlotOutcome -> [UUT.PastSlotHost] -> UUT.PastSlot
expectedSlot fixture showInsert outcome hosts =
  UUT.PastSlot
    { psTemplateId = fixture.fTemplateId,
      psShowId = fixture.fShowId,
      psShowTitle = Shows.siTitle showInsert,
      psShowSlug = Shows.siSlug showInsert,
      psAirDate = slotTestDay,
      psStart = slotTestTime 0 (TimeOfDay 14 0 0),
      psEnd = slotTestTime 0 (TimeOfDay 16 0 0),
      psOutcome = outcome,
      psHosts = hosts
    }

hostOf :: Fixture -> UserMetadata.UserWithMetadataInsert -> UUT.PastSlotHost
hostOf fixture userWithMetadata =
  UUT.PastSlotHost fixture.fUserId (UserMetadata.uwmiDisplayName userWithMetadata)

--------------------------------------------------------------------------------

-- | An @episode@ row of the show that starts in the window makes the slot
-- Aired. The window opens one minute early for this test.
prop_airedSlot :: TestDBConfig -> PropertyT IO ()
prop_airedSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)
    -- Seconds from the slot start to the start of the episode.
    offset <- forAllT $ Gen.integral (Range.linear (-60) 7199)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        insertAiring "episode" fixture.fEpisodeId (secondsAfterOpen offset)
        slots <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure (fixture, slots)

      assert $ do
        (fixture, slots) <- assertRight result
        slots === [expectedSlot fixture showInsert UUT.Aired [hostOf fixture userWithMetadata]]

-- | An instant some seconds after the slot of 'slotTestDay' opens.
secondsAfterOpen :: Integer -> UTCTime
secondsAfterOpen offset =
  addUTCTime (fromInteger offset :: NominalDiffTime) (slotTestTime 0 (TimeOfDay 14 0 0))

-- | A slot is Missed when nothing of the show aired in it, when a rerun aired
-- in it, and when the episode started before the window or at its end.
prop_missedSlot :: TestDBConfig -> PropertyT IO ()
prop_missedSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)
    airings <-
      forAllT $
        Gen.subsequence
          [ ("rerun", 0),
            ("episode", -61),
            ("episode", 7200)
          ]

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        mapM_ (\(sourceType, offset) -> insertAiring sourceType fixture.fEpisodeId (secondsAfterOpen offset)) airings
        slots <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure (fixture, slots)

      assert $ do
        (fixture, slots) <- assertRight result
        slots === [expectedSlot fixture showInsert UUT.Missed [hostOf fixture userWithMetadata]]

-- | An @episode@ row of a different show in the window leaves the slot Missed.
prop_otherShowEpisode :: TestDBConfig -> PropertyT IO ()
prop_otherShowEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        let Slug slug = Shows.siSlug showInsert
        otherShowId <- unwrapInsert $ Shows.insertShow showInsert {Shows.siSlug = Slug (slug <> "-other")}
        otherEpisodeId <-
          insertTestEpisode
            episodeTemplate
              { Episodes.eiId = otherShowId,
                Episodes.eiScheduleTemplateId = Nothing,
                Episodes.eiAirDate = Nothing,
                Episodes.eiCreatedBy = fixture.fUserId
              }
        insertAiring "episode" otherEpisodeId (secondsAfterOpen 0)
        slots <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure (fixture, slots)

      assert $ do
        (fixture, slots) <- assertRight result
        slots === [expectedSlot fixture showInsert UUT.Missed [hostOf fixture userWithMetadata]]

-- | A host who joined after the slot opened is not a host of the slot. A host
-- who left before the slot opened is not one. A host who left after it opened
-- is one.
prop_hostsWhenTheSlotOpened :: TestDBConfig -> PropertyT IO ()
prop_hostsWhenTheSlotOpened cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)
    joinedLate <- uniqueUser
    leftEarly <- uniqueUser
    leftLate <- uniqueUser

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        let addHost user joinedAt leftAt = do
              userId <- insertTestUser user
              addTestShowHost fixture.fShowId userId
              setHostDates fixture.fShowId userId joinedAt leftAt
              pure userId
            before = slotTestTime (-1) (TimeOfDay 12 0 0)
            after = slotTestTime 0 (TimeOfDay 15 0 0)
        _ <- addHost joinedLate after Nothing
        _ <- addHost leftEarly (slotTestTime (-2) (TimeOfDay 12 0 0)) (Just before)
        leftLateId <- addHost leftLate before (Just after)
        slots <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure (fixture, leftLateId, slots)

      assert $ do
        (fixture, leftLateId, slots) <- assertRight result
        slots
          === [ expectedSlot
                  fixture
                  showInsert
                  UUT.Missed
                  [ hostOf fixture userWithMetadata,
                    UUT.PastSlotHost leftLateId (UserMetadata.uwmiDisplayName leftLate)
                  ]
              ]
  where
    uniqueUser = do
      user <- forAllT userWithMetadataInsertGen
      email <- liftIO mkUniqueEmail
      pure user {UserMetadata.uwmiEmail = email}

-- | The air record of the fixture starts at 15:00 on the Monday before
-- 'slotTestDay'. The slot of that Monday opened at 14:00, so it gives no row.
-- With no air record, no slot gives a row.
prop_recordStart :: TestDBConfig -> PropertyT IO ()
prop_recordStart cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        let ofShow = filter ((== fixture.fShowId) . UUT.psShowId)
        withRecord <- ofShow <$> TRX.statement () (UUT.getPastSlots Nothing dayEnd)
        clearAirRecord
        withNoRecord <- ofShow <$> TRX.statement () (UUT.getPastSlots Nothing dayEnd)
        TRX.condemn
        pure (fixture, withRecord, withNoRecord)

      assert $ do
        (fixture, withRecord, withNoRecord) <- assertRight result
        withRecord === [expectedSlot fixture showInsert UUT.Missed [hostOf fixture userWithMetadata]]
        withNoRecord === []

-- | A range that ends before the slot closes gives no row. A range that holds
-- the primary window and the replay window gives one row.
prop_unfinishedAndReplay :: TestDBConfig -> PropertyT IO ()
prop_unfinishedAndReplay cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate (Just (TimeOfDay 22 0 0))
        let ofShow = filter ((== fixture.fShowId) . UUT.psShowId)
        unfinished <- ofShow <$> TRX.statement () (UUT.getPastSlots (Just dayStart) (slotTestTime 0 (TimeOfDay 15 59 0)))
        finished <- ofShow <$> TRX.statement () (UUT.getPastSlots (Just dayStart) (slotTestTime 1 (TimeOfDay 6 0 0)))
        TRX.condemn
        pure (fixture, unfinished, finished)

      assert $ do
        (fixture, unfinished, finished) <- assertRight result
        unfinished === []
        finished === [expectedSlot fixture showInsert UUT.Missed [hostOf fixture userWithMetadata]]

-- | The fixture has fill in the window, so the slot gives a row. With the fill
-- gone, the air record holds nothing in the window, and the slot gives no row.
prop_noAirRecordInWindow :: TestDBConfig -> PropertyT IO ()
prop_noAirRecordInWindow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        withFill <- slotsOfShow fixture.fShowId
        clearFill
        withNoFill <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure (fixture, withFill, withNoFill)

      assert $ do
        (fixture, withFill, withNoFill) <- assertRight result
        withFill === [expectedSlot fixture showInsert UUT.Missed [hostOf fixture userWithMetadata]]
        withNoFill === []

-- | A soft-deleted show gives no row.
prop_deletedShow :: TestDBConfig -> PropertyT IO ()
prop_deletedShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (UUT.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        fixture <- insertFixture userWithMetadata showInsert episodeTemplate Nothing
        _ <- TRX.statement () (Shows.softDeleteShow fixture.fShowId)
        slots <- slotsOfShow fixture.fShowId
        TRX.condemn
        pure slots

      assert $ do
        slots <- assertRight result
        slots === []

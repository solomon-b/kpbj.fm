module Effects.Database.Tables.PlaybackHistorySpec where

--------------------------------------------------------------------------------

import Data.Time (fromGregorian)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.PlaybackHistory qualified as UUT
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, assertSingleton)
import Test.Gen.Tables.PlaybackHistory (playbackInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.PlaybackHistory" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert then getRecentPlayback returns the entry" $
          hedgehog . prop_insertPlayback

      describe "Queries" $ do
        runs 10 . it "getRecentPlayback: respects limit and orders DESC" $
          hedgehog . prop_getRecentPlayback
        runs 10 . it "getRecentPlayback: returns empty list when table is empty" $
          hedgehog . prop_getRecentPlayback_empty
        runs 10 . it "insertPlayback: skips a second airing with the same planned start" $
          hedgehog . prop_insertPlayback_duplicateAiring
        runs 10 . it "insertPlayback: skips a resumed episode or ephemeral airing" $
          hedgehog . prop_insertPlayback_duplicateNonBreakAiring
        runs 10 . it "insertPlayback: keeps two items at the same planned start" $
          hedgehog . prop_insertPlayback_otherItemSamePlannedStart
        runs 10 . it "insertPlayback: keeps every post with no planned start" $
          hedgehog . prop_insertPlayback_noPlannedStart

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert a playback entry, then retrieve and verify.
prop_insertPlayback :: TestDBConfig -> PropertyT IO ()
prop_insertPlayback cfg = do
  arrange (bracketConn cfg) $ do
    playbackInsert <- forAllT playbackInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.statement () (UUT.insertPlayback playbackInsert)

        recent <- TRX.statement () (UUT.getRecentPlayback 1)
        TRX.condemn
        pure (playbackInsert, recent)

      assert $ do
        (insert, recent) <- assertRight result
        entry <- assertSingleton recent
        UUT.phTitle entry === UUT.piTitle insert
        UUT.phArtist entry === UUT.piArtist insert
        UUT.phSourceType entry === UUT.piSourceType insert
        UUT.phSourceUrl entry === UUT.piSourceUrl insert
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getRecentPlayback: respects limit and orders by started_at DESC.
prop_getRecentPlayback :: TestDBConfig -> PropertyT IO ()
prop_getRecentPlayback cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT playbackInsertGen

    let t1 = posixSecondsToUTCTime 1000
        t2 = posixSecondsToUTCTime 2000
        t3 = posixSecondsToUTCTime 3000
        playback1 = template {UUT.piStartedAt = t1}
        playback2 = template {UUT.piStartedAt = t2}
        playback3 = template {UUT.piStartedAt = t3}

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        TRX.statement () (UUT.insertPlayback playback1)
        TRX.statement () (UUT.insertPlayback playback2)
        TRX.statement () (UUT.insertPlayback playback3)

        allEntries <- TRX.statement () (UUT.getRecentPlayback 10)
        limited <- TRX.statement () (UUT.getRecentPlayback 2)
        TRX.condemn
        pure (allEntries, limited)

      assert $ do
        (allEntries, limited) <- assertRight result
        length allEntries === 3
        length limited === 2
        -- Verify DESC ordering by started_at
        map UUT.phStartedAt allEntries === [t3, t2, t1]
        pure ()

-- | getRecentPlayback: returns empty list when no entries exist.
prop_getRecentPlayback_empty :: TestDBConfig -> PropertyT IO ()
prop_getRecentPlayback_empty cfg = do
  arrange (bracketConn cfg) $ do
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Read $ do
        recent <- TRX.statement () (UUT.getRecentPlayback 10)
        TRX.condemn
        pure recent

      assert $ do
        recent <- assertRight result
        length recent === 0
        pure ()

-- | insertPlayback: a break item airing with the same planned start is stored once.
prop_insertPlayback_duplicateAiring :: TestDBConfig -> PropertyT IO ()
prop_insertPlayback_duplicateAiring cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT playbackInsertGen
    userWithMetadata <- forAllT userWithMetadataInsertGen

    let planned = posixSecondsToUTCTime 5000
        airingOf breakItemId =
          template
            { UUT.piBreakItemId = Just (BreakItems.unId breakItemId),
              UUT.piPlannedStart = Just planned,
              UUT.piStartedAt = posixSecondsToUTCTime 5000
            }

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        breakItemId <- unwrapInsert (BreakItems.insertBreakItem (psaInsert userId))
        let first = airingOf breakItemId
            -- The same airing, posted again after a restart.
            resumed = first {UUT.piStartedAt = posixSecondsToUTCTime 5100}
        TRX.statement () (UUT.insertPlayback first)
        TRX.statement () (UUT.insertPlayback resumed)

        entries <- TRX.statement () (UUT.getRecentPlayback 10)
        TRX.condemn
        pure entries

      assert $ do
        entries <- assertRight result
        length entries === 1
        pure ()

-- | Insert the rows in order, and count the rows that the table then holds.
insertAndCount :: [UUT.Insert] -> TRX.Transaction Int
insertAndCount rows = do
  mapM_ (TRX.statement () . UUT.insertPlayback) rows
  entries <- TRX.statement () (UUT.getRecentPlayback 10)
  TRX.condemn
  pure (length entries)

-- | insertPlayback: an episode or ephemeral airing with the same source URL
-- and planned start is stored once. Liquidsoap posts it again after a restart.
prop_insertPlayback_duplicateNonBreakAiring :: TestDBConfig -> PropertyT IO ()
prop_insertPlayback_duplicateNonBreakAiring cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT playbackInsertGen
    let first =
          template
            { UUT.piPlannedStart = Just (posixSecondsToUTCTime 5000),
              UUT.piStartedAt = posixSecondsToUTCTime 5000
            }
        resumed = first {UUT.piStartedAt = posixSecondsToUTCTime 5600}

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ insertAndCount [first, resumed]

      assert $ do
        count <- assertRight result
        count === 1
        pure ()

-- | insertPlayback: two items with the same planned start and different source
-- URLs get two rows. A schedule change can put a new item at the planned start
-- of the one on air.
prop_insertPlayback_otherItemSamePlannedStart :: TestDBConfig -> PropertyT IO ()
prop_insertPlayback_otherItemSamePlannedStart cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT playbackInsertGen
    let first = template {UUT.piPlannedStart = Just (posixSecondsToUTCTime 5000)}
        replacement =
          first
            { UUT.piSourceUrl = UUT.piSourceUrl first <> "/replacement",
              UUT.piStartedAt = posixSecondsToUTCTime 5600
            }

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ insertAndCount [first, replacement]

      assert $ do
        count <- assertRight result
        count === 2
        pure ()

-- | insertPlayback: posts with no planned start always insert. An older client
-- sends none.
prop_insertPlayback_noPlannedStart :: TestDBConfig -> PropertyT IO ()
prop_insertPlayback_noPlannedStart cfg = do
  arrange (bracketConn cfg) $ do
    template <- forAllT playbackInsertGen
    let first = template {UUT.piPlannedStart = Nothing}

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ insertAndCount [first, first]

      assert $ do
        count <- assertRight result
        count === 2
        pure ()

-- | A 45 second PSA. A break item airing needs a real one for its foreign key.
psaInsert :: User.Id -> BreakItems.Insert
psaInsert creatorId =
  BreakItems.Insert
    { biiTitle = "Library Hours",
      biiCategory = BreakItems.Psa,
      biiUnderwriterId = Nothing,
      biiShowId = Nothing,
      biiReviewStatus = BreakItems.Approved,
      biiAudioFilePath = "audio/break-items/2025/01/01/library-hours_2025-01-01_abc.mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 720000,
      biiDurationSeconds = 45,
      biiStartsOn = fromGregorian 2025 1 1,
      biiEndsOn = Nothing,
      biiCreatorId = creatorId
    }

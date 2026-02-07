module Effects.Database.Tables.PlaybackHistorySpec where

--------------------------------------------------------------------------------

import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.PlaybackHistory qualified as UUT
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, assertSingleton)
import Test.Gen.Tables.PlaybackHistory (playbackInsertGen)
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

-- | Tests for the listener heatmap and bucket queries.
--
-- Both queries group snapshots in station time. A
-- snapshot near midnight UTC is on the previous day in Los Angeles, and the
-- offset changes at the end of daylight saving time on 2026-11-01.
module Effects.Database.Tables.ListenerSnapshotsSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Time (UTCTime (..), fromGregorian)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.ListenerSnapshots qualified as UUT
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldNotSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.ListenerSnapshots" $ do
      describe "getHourlyHeatmap" $ do
        it "averages snapshots in one Pacific hour after the end of DST" standardTime
        it "puts a snapshot near midnight UTC in the Pacific day before DST ends" daylightTime
        it "leaves out snapshots after the end of the window" windowEnd
      describe "getSnapshotBuckets" $
        it "starts each day bucket at midnight Pacific" dailyBuckets

--------------------------------------------------------------------------------

-- | Two snapshots at 03:30 and 03:45 UTC on Tuesday 2026-11-17 are at 19:30
-- and 19:45 PST on Monday.
standardTime :: TestDBConfig -> IO ()
standardTime cfg = do
  rows <- heatmap cfg (utc 2026 10 1) (utc 2026 12 1)
  rows `shouldContain` [UUT.HeatmapRow 1 19 5.0]

-- | A snapshot at 06:10 UTC on Saturday 2026-10-31 is at 23:10 PDT on
-- Friday.
daylightTime :: TestDBConfig -> IO ()
daylightTime cfg = do
  rows <- heatmap cfg (utc 2026 10 1) (utc 2026 12 1)
  rows `shouldContain` [UUT.HeatmapRow 5 23 10.0]

-- | A window that ends before 2026-11-17 has no Monday 19:00 cell.
windowEnd :: TestDBConfig -> IO ()
windowEnd cfg = do
  rows <- heatmap cfg (utc 2026 10 1) (utc 2026 11 17)
  rows `shouldNotSatisfy` any (\r -> r.weekday == 1 && r.hour == 19)

-- | The snapshot at 23:10 PDT on 2026-10-30 is in the bucket that starts at
-- 07:00 UTC. The two snapshots on the evening of 2026-11-16 PST are in the
-- bucket that starts at 08:00 UTC.
dailyBuckets :: TestDBConfig -> IO ()
dailyBuckets cfg = do
  rows <- withSnapshots cfg (UUT.getSnapshotBuckets UUT.Daily (utc 2026 10 1) (utc 2026 12 1))
  rows `shouldBe` [(10.0, utcAt 2026 10 30 7), (5.0, utcAt 2026 11 16 8)]

--------------------------------------------------------------------------------

-- | Run the heatmap query on the fixture snapshots.
heatmap :: TestDBConfig -> UTCTime -> UTCTime -> IO [UUT.HeatmapRow]
heatmap cfg start end = withSnapshots cfg (UUT.getHourlyHeatmap start end)

-- | Insert the fixture snapshots, run the query, and roll back.
withSnapshots :: TestDBConfig -> Hasql.Statement () a -> IO a
withSnapshots cfg statement = bracketConn cfg $ do
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    TRX.sql
      "INSERT INTO listener_snapshots (listener_count, recorded_at) VALUES \
      \(4, '2026-11-17 03:30:00+00'), \
      \(6, '2026-11-17 03:45:00+00'), \
      \(10, '2026-10-31 06:10:00+00')"
    rows <- TRX.statement () statement
    TRX.condemn
    pure rows
  either (\err -> error $ "DB error: " <> show err) (liftIO . pure) result

utc :: Integer -> Int -> Int -> UTCTime
utc y m d = utcAt y m d 0

-- | A time on the hour in UTC.
utcAt :: Integer -> Int -> Int -> Int -> UTCTime
utcAt y m d h = UTCTime (fromGregorian y m d) (fromIntegral (h * 3600))

{-# LANGUAGE QuasiQuotes #-}

-- | Tests for the top shows query.
module Effects.Database.Tables.EpisodePlayEventsSpec where

--------------------------------------------------------------------------------

import Control.Monad (replicateM_)
import Data.Time (UTCTime (..), fromGregorian)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EpisodePlayEvents qualified as UUT
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight)
import Test.Gen.Tables.Episodes (episodeInsertGen)
import Test.Gen.Tables.ShowSchedule (genRecurringScheduleInsert)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.EpisodePlayEvents.getTopShows" $
      runs 3 . it "ranks shows by plays in the window and keeps the limit" $
        hedgehog . prop_topShows

--------------------------------------------------------------------------------

-- | Show A has 3 plays in the window. Show B has 1 play in the window and 5
-- plays after it.
prop_topShows :: TestDBConfig -> PropertyT IO ()
prop_topShows cfg = do
  arrange (bracketConn cfg) $ do
    userInsert <- forAllT userWithMetadataInsertGen
    showA <- forAllT showInsertGen
    showB <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        let addShow title slug showInsert = do
              (showId, templateId) <-
                insertTestShowWithSchedule showInsert {Shows.siTitle = title, Shows.siSlug = Slug slug} scheduleTemplate
              insertTestEpisode
                episodeTemplate
                  { Episodes.eiId = showId,
                    Episodes.eiScheduleTemplateId = Just templateId,
                    Episodes.eiCreatedBy = userId
                  }
        episodeA <- addShow "Show A" "top-shows-test-a" showA
        episodeB <- addShow "Show B" "top-shows-test-b" showB
        replicateM_ 3 $ addPlay episodeA (utc 2026 6 10)
        addPlay episodeB (utc 2026 6 11)
        replicateM_ 5 $ addPlay episodeB (utc 2026 7 2)
        rows <- TRX.statement () $ UUT.getTopShows (utc 2026 6 1) (utc 2026 7 1) 10
        limited <- TRX.statement () $ UUT.getTopShows (utc 2026 6 1) (utc 2026 7 1) 1
        TRX.condemn
        pure (rows, limited)

      assert $ do
        (rows, limited) <- assertRight result
        map (\r -> (r.showTitle, r.showSlug, r.playCount)) rows
          === [("Show A", "top-shows-test-a", 3), ("Show B", "top-shows-test-b", 1)]
        map (.showTitle) limited === ["Show A"]

--------------------------------------------------------------------------------

addPlay :: Episodes.Id -> UTCTime -> TRX.Transaction ()
addPlay episodeId playedAt =
  TRX.statement () $
    interp
      False
      [sql| INSERT INTO episode_play_events (episode_id, played_at) VALUES (#{episodeId}, #{playedAt}) |]

utc :: Integer -> Int -> Int -> UTCTime
utc y m d = UTCTime (fromGregorian y m d) 0

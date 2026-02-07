{-# LANGUAGE OverloadedRecordDot #-}

module Effects.Database.Tables.EpisodeTrackSpec where

--------------------------------------------------------------------------------

import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EpisodeTrack qualified as UUT
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertRight, assertSingleton)
import Test.Gen.Tables.EpisodeTrack (episodeTrackInsertGen)
import Test.Gen.Tables.Episodes (episodeInsertGen)
import Test.Gen.Tables.ShowSchedule (scheduleTemplateInsertGen)
import Test.Gen.Tables.Shows (showInsertGen)
import Test.Gen.Tables.UserMetadata (userWithMetadataInsertGen)
import Test.Hspec (Spec, describe, it)
import Test.Hspec.Hedgehog (hedgehog)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "Effects.Database.Tables.EpisodeTrack" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect

      describe "Queries" $ do
        runs 10 . it "getTracksForEpisode: returns tracks ordered by track_number" $
          hedgehog . prop_getTracksForEpisode

      describe "Mutations" $ do
        runs 10 . it "deleteAllTracksForEpisode: removes all tracks" $
          hedgehog . prop_deleteAllTracksForEpisode

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert a track, retrieve tracks for the episode, verify match.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    trackTemplate <- forAllT $ episodeTrackInsertGen (Episodes.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {Episodes.eiId = showId, Episodes.eiScheduleTemplateId = templateId, Episodes.eiCreatedBy = userId}
        episodeId <- TRX.statement () (Episodes.insertEpisode episodeInsert)

        let trackInsert = trackTemplate {UUT.etiEpisodeId = episodeId, UUT.etiTrackNumber = 1}
        trackId <- TRX.statement () (UUT.insertEpisodeTrack trackInsert)

        tracks <- TRX.statement () (UUT.getTracksForEpisode episodeId)
        TRX.condemn
        pure (trackId, trackInsert, tracks)

      assert $ do
        (trackId, trackInsert, tracks) <- assertRight result
        track <- assertSingleton tracks
        track.id === trackId
        track.title === UUT.etiTitle trackInsert
        track.artist === UUT.etiArtist trackInsert
        track.trackNumber === UUT.etiTrackNumber trackInsert
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getTracksForEpisode: returns tracks ordered by track_number.
prop_getTracksForEpisode :: TestDBConfig -> PropertyT IO ()
prop_getTracksForEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    track1Template <- forAllT $ episodeTrackInsertGen (Episodes.Id 1)
    track2Template <- forAllT $ episodeTrackInsertGen (Episodes.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {Episodes.eiId = showId, Episodes.eiScheduleTemplateId = templateId, Episodes.eiCreatedBy = userId}
        episodeId <- TRX.statement () (Episodes.insertEpisode episodeInsert)

        -- Insert track 2 first, then track 1, to verify ordering
        let track2 = track2Template {UUT.etiEpisodeId = episodeId, UUT.etiTrackNumber = 2}
        let track1 = track1Template {UUT.etiEpisodeId = episodeId, UUT.etiTrackNumber = 1}

        _ <- TRX.statement () (UUT.insertEpisodeTrack track2)
        _ <- TRX.statement () (UUT.insertEpisodeTrack track1)

        tracks <- TRX.statement () (UUT.getTracksForEpisode episodeId)
        TRX.condemn
        pure tracks

      assert $ do
        tracks <- assertRight result
        length tracks === 2
        -- Should be ordered by track_number ASC
        let trackNumbers = map (.trackNumber) tracks
        trackNumbers === [1, 2]
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteAllTracksForEpisode: removes all tracks, returns count.
prop_deleteAllTracksForEpisode :: TestDBConfig -> PropertyT IO ()
prop_deleteAllTracksForEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    track1Template <- forAllT $ episodeTrackInsertGen (Episodes.Id 1)
    track2Template <- forAllT $ episodeTrackInsertGen (Episodes.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {Episodes.eiId = showId, Episodes.eiScheduleTemplateId = templateId, Episodes.eiCreatedBy = userId}
        episodeId <- TRX.statement () (Episodes.insertEpisode episodeInsert)

        let track1 = track1Template {UUT.etiEpisodeId = episodeId, UUT.etiTrackNumber = 1}
        let track2 = track2Template {UUT.etiEpisodeId = episodeId, UUT.etiTrackNumber = 2}
        _ <- TRX.statement () (UUT.insertEpisodeTrack track1)
        _ <- TRX.statement () (UUT.insertEpisodeTrack track2)

        -- Delete all
        deleted <- TRX.statement () (UUT.deleteAllTracksForEpisode episodeId)

        -- Verify none remain
        afterDelete <- TRX.statement () (UUT.getTracksForEpisode episodeId)

        TRX.condemn
        pure (deleted, afterDelete)

      assert $ do
        (deleted, afterDelete) <- assertRight result
        deleted === 2
        length afterDelete === 0
        pure ()

module Effects.Database.Tables.EpisodesSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
import Data.Time.Calendar (addDays)
import Data.Time.Clock (NominalDiffTime, UTCTime (..), addUTCTime, getCurrentTime, secondsToDiffTime, utctDay)
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.Episodes qualified as UUT
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (PropertyT, (===))
import Hedgehog.Internal.Property (forAllT)
import Test.Database.Helpers (insertTestShowWithSchedule, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertRight, assertSingleton)
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
    describe "Effects.Database.Tables.Episodes" $ do
      describe "Lens Laws" $ do
        runs 10 . it "insert-select: inserted fields preserved on select" $
          hedgehog . prop_insertSelect
        runs 10 . it "update-select: updated fields overwrite original on select" $
          hedgehog . prop_updateSelect
        runs 10 . it "update-update: second update fully overwrites first" $
          hedgehog . prop_updateUpdate

      describe "Queries" $ do
        runs 10 . it "getEpisodesForShow: returns episodes for a specific show" $
          hedgehog . prop_getEpisodesForShow
        runs 10 . it "getPublishedEpisodesForShow: filters by non-deleted" $
          hedgehog . prop_getPublishedEpisodesForShow
        runs 10 . it "getEpisodeByShowAndNumber: looks up by show slug + episode number" $
          hedgehog . prop_getEpisodeByShowAndNumber

      describe "Mutations" $ do
        runs 10 . it "deleteEpisode: soft delete sets deleted_at" $
          hedgehog . prop_deleteEpisode
        runs 10 . it "deleteEpisode: second delete is idempotent" $
          hedgehog . prop_deleteEpisode_idempotent

      describe "File Updates" $ do
        runs 10 . it "updateEpisodeFiles: updates audio/artwork paths" $
          hedgehog . prop_updateEpisodeFiles

      describe "User Queries" $ do
        runs 10 . it "getEpisodesByUser: returns non-deleted episodes by creator" $
          hedgehog . prop_getEpisodesByUser

      describe "Schedule Updates" $ do
        runs 10 . it "updateScheduledSlot: changes template and scheduled_at" $
          hedgehog . prop_updateScheduledSlot

      describe "Unscheduled Episodes" $ do
        runs 10 . it "clearTemplateForUpcomingEpisodes: nulls schedule fields for future episodes" $
          hedgehog . prop_clearTemplateForUpcomingEpisodes
        runs 10 . it "clearTemplateForUpcomingEpisodes: only clears episodes on/after the change date" $
          hedgehog . prop_clearTemplateForUpcomingEpisodes_dateGate
        runs 10 . it "getEpisodesForShow: unscheduled episodes sort last" $
          hedgehog . prop_unscheduledEpisodesSortLast
        runs 10 . it "getPublishedEpisodesForShow: excludes unscheduled episodes" $
          hedgehog . prop_publishedExcludesUnscheduled

      describe "Releasing a slot on deactivate" $ do
        runs 10 . it "closeSchedulesAndDetachEpisodes: closes an active window on the given date" $
          hedgehog . prop_closeSchedules_closesActiveWindow
        runs 10 . it "closeSchedulesAndDetachEpisodes: a pending window becomes empty, never inverted" $
          hedgehog . prop_closeSchedules_pendingWindowNeverInverted
        runs 10 . it "closeSchedulesAndDetachEpisodes: leaves a past episode attached" $
          hedgehog . prop_closeSchedules_keepsPastEpisode
        runs 10 . it "closeSchedulesAndDetachEpisodes: does not move an already-closed window" $
          hedgehog . prop_closeSchedules_leavesClosedWindow

      describe "Template Blocking" $ do
        runs 10 . it "getUpcomingEpisodesForTemplates: returns an upcoming attached episode" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_returnsUpcoming
        runs 10 . it "getUpcomingEpisodesForTemplates: excludes episodes scheduled in the past" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_excludesPast
        runs 10 . it "getUpcomingEpisodesForTemplates: excludes episodes scheduled before the change date" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_dateGate
        runs 10 . it "getUpcomingEpisodesForTemplates: excludes soft-deleted episodes" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_excludesDeleted
        runs 10 . it "getUpcomingEpisodesForTemplates: excludes episodes on other templates" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_excludesOtherTemplate
        runs 10 . it "getUpcomingEpisodesForTemplates: empty template list returns no episodes" $
          hedgehog . prop_getUpcomingEpisodesForTemplates_emptyList

      describe "Tag Operations" $ do
        runs 10 . it "getTagsForEpisode: returns tags for episode" $
          hedgehog . prop_getTagsForEpisode
        runs 10 . it "replaceEpisodeTags: atomically replaces tags" $
          hedgehog . prop_replaceEpisodeTags

--------------------------------------------------------------------------------
-- Helpers

-- | Assert all user-provided fields in an Insert match the corresponding Model fields.
assertInsertFieldsMatch :: UUT.Insert -> UUT.Model -> PropertyT IO ()
assertInsertFieldsMatch insert model = do
  UUT.eiId insert === UUT.showId model
  UUT.eiDescription insert === UUT.description model
  UUT.eiScheduleTemplateId insert === UUT.scheduleTemplateId model
  UUT.eiCreatedBy insert === UUT.createdBy model
  UUT.eiScheduledAt insert === UUT.scheduledAt model
  UUT.eiAudioFilePath insert === UUT.audioFilePath model
  UUT.eiAudioFileSize insert === UUT.audioFileSize model
  UUT.eiAudioMimeType insert === UUT.audioMimeType model
  UUT.eiDurationSeconds insert === UUT.durationSeconds model
  UUT.eiArtworkUrl insert === UUT.artworkUrl model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select returns what we inserted.
prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}

        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        selected <- TRX.statement () (UUT.getEpisodeById episodeId)
        TRX.condemn
        pure (episodeId, episodeInsert, selected)

      assert $ do
        (episodeId, episodeInsert, mSelected) <- assertRight result
        selected <- assertJust mSelected
        episodeId === UUT.id selected
        assertInsertFieldsMatch episodeInsert selected

-- | Update-Select: updateEpisode then getById returns updated fields.
prop_updateSelect :: TestDBConfig -> PropertyT IO ()
prop_updateSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateEpisodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        let update = UUT.Update {UUT.euId = episodeId, UUT.euDescription = UUT.eiDescription updateEpisodeTemplate}
        updateResult <- TRX.statement () (UUT.updateEpisode update)

        selected <- TRX.statement () (UUT.getEpisodeById episodeId)
        TRX.condemn
        pure (episodeId, update, updateResult, selected)

      assert $ do
        (episodeId, update, updateResult, mSelected) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === episodeId

        selected <- assertJust mSelected
        UUT.euDescription update === UUT.description selected
        UUT.id selected === episodeId
        pure ()

-- | Update-Update: second update overwrites first.
prop_updateUpdate :: TestDBConfig -> PropertyT IO ()
prop_updateUpdate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateATemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateBTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        let updateA = UUT.Update {UUT.euId = episodeId, UUT.euDescription = UUT.eiDescription updateATemplate}
        _ <- TRX.statement () (UUT.updateEpisode updateA)

        let updateB = UUT.Update {UUT.euId = episodeId, UUT.euDescription = UUT.eiDescription updateBTemplate}
        _ <- TRX.statement () (UUT.updateEpisode updateB)

        selected <- TRX.statement () (UUT.getEpisodeById episodeId)
        TRX.condemn
        pure (episodeId, updateB, selected)

      assert $ do
        (episodeId, updateB, mSelected) <- assertRight result
        selected <- assertJust mSelected
        UUT.euDescription updateB === UUT.description selected
        UUT.id selected === episodeId
        pure ()

--------------------------------------------------------------------------------
-- Query tests

-- | getEpisodesForShow: returns episodes for a specific show (non-deleted).
prop_getEpisodesForShow :: TestDBConfig -> PropertyT IO ()
prop_getEpisodesForShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Offset scheduledAt to avoid unique constraint on (show_id, scheduled_at)
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = fmap (addUTCTime (3600 :: NominalDiffTime)) (UUT.eiScheduledAt ep2Template)}

        id1 <- unwrapInsert (UUT.insertEpisode ep1)
        id2 <- unwrapInsert (UUT.insertEpisode ep2)

        -- Soft-delete one episode
        _ <- TRX.statement () (UUT.deleteEpisode id2)

        episodes <- TRX.statement () (UUT.getEpisodesForShow showId (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, episodes)

      assert $ do
        (id1, episodes) <- assertRight result
        -- Only non-deleted episode should be returned
        ep <- assertSingleton episodes
        UUT.id ep === id1
        pure ()

-- | getPublishedEpisodesForShow: filters by non-deleted and past schedule.
prop_getPublishedEpisodesForShow :: TestDBConfig -> PropertyT IO ()
prop_getPublishedEpisodesForShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Offset scheduledAt to avoid unique constraint on (show_id, scheduled_at)
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = fmap (addUTCTime (3600 :: NominalDiffTime)) (UUT.eiScheduledAt ep2Template)}

        id1 <- unwrapInsert (UUT.insertEpisode ep1)
        id2 <- unwrapInsert (UUT.insertEpisode ep2)

        -- Soft-delete one episode
        _ <- TRX.statement () (UUT.deleteEpisode id2)

        -- Published episodes should exclude deleted ones
        published <- TRX.statement () (UUT.getPublishedEpisodesForShow now showId (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, published)

      assert $ do
        (id1, published) <- assertRight result
        -- Deleted episode should never appear in published list.
        -- Non-deleted episode may or may not appear depending on scheduledAt vs now.
        case published of
          [] -> pure () -- scheduledAt was in the future, OK
          [ep] -> UUT.id ep === id1 -- If returned, it must be the non-deleted one
          _ -> do
            -- Should never have more than 1 result with only 1 non-deleted episode
            length published === 1
            pure ()

-- | getEpisodeByShowAndNumber: looks up by show slug + episode number.
prop_getEpisodeByShowAndNumber :: TestDBConfig -> PropertyT IO ()
prop_getEpisodeByShowAndNumber cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Get the episode to find its number
        mEpisode <- TRX.statement () (UUT.getEpisodeById episodeId)

        lookupResult <- case mEpisode of
          Nothing -> pure (episodeId, Nothing, Nothing)
          Just ep -> do
            let showSlug = Shows.siSlug showInsert
            let epNumber = UUT.episodeNumber ep
            byShowAndNumber <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber)
            pure (episodeId, Just ep, byShowAndNumber)
        TRX.condemn
        pure lookupResult

      assert $ do
        (episodeId, mEpisode, mByShowAndNumber) <- assertRight result
        _ <- assertJust mEpisode
        found <- assertJust mByShowAndNumber
        UUT.id found === episodeId
        pure ()

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteEpisode: soft delete sets deleted_at.
prop_deleteEpisode :: TestDBConfig -> PropertyT IO ()
prop_deleteEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        deleteResult <- TRX.statement () (UUT.deleteEpisode episodeId)

        -- getEpisodeById filters on deleted_at IS NULL, so it should return Nothing
        afterDelete <- TRX.statement () (UUT.getEpisodeById episodeId)

        -- getEpisodesForShow should also exclude it
        episodesForShow <- TRX.statement () (UUT.getEpisodesForShow showId (Limit 10) (Offset 0))

        TRX.condemn
        pure (episodeId, deleteResult, afterDelete, episodesForShow)

      assert $ do
        (episodeId, deleteResult, mAfterDelete, episodesForShow) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === episodeId

        -- Episode no longer visible via getById (soft-delete filter excludes it)
        mAfterDelete === Nothing

        -- Also excluded from getEpisodesForShow
        length episodesForShow === 0
        pure ()

-- | deleteEpisode: second delete is idempotent (always returns id).
prop_deleteEpisode_idempotent :: TestDBConfig -> PropertyT IO ()
prop_deleteEpisode_idempotent cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        firstDelete <- TRX.statement () (UUT.deleteEpisode episodeId)
        secondDelete <- TRX.statement () (UUT.deleteEpisode episodeId)

        TRX.condemn
        pure (episodeId, firstDelete, secondDelete)

      assert $ do
        (episodeId, firstDelete, secondDelete) <- assertRight result
        -- Both deletes return the id (no WHERE deleted_at IS NULL)
        firstDeleteId <- assertJust firstDelete
        firstDeleteId === episodeId
        secondDeleteId <- assertJust secondDelete
        secondDeleteId === episodeId
        pure ()

--------------------------------------------------------------------------------
-- File Update tests

-- | updateEpisodeFiles: updates audio/artwork paths.
prop_updateEpisodeFiles :: TestDBConfig -> PropertyT IO ()
prop_updateEpisodeFiles cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert with no audio/artwork
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiAudioFilePath = Nothing, UUT.eiArtworkUrl = Nothing}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Update with audio file
        let fileUpdate =
              UUT.FileUpdate
                { UUT.efuId = episodeId,
                  UUT.efuAudioFilePath = Just "/audio/test.mp3",
                  UUT.efuArtworkUrl = Just "/images/artwork.jpg",
                  UUT.efuDurationSeconds = Just 300,
                  UUT.efuClearAudio = False,
                  UUT.efuClearArtwork = False
                }
        updateResult <- TRX.statement () (UUT.updateEpisodeFiles fileUpdate)

        afterUpdate <- TRX.statement () (UUT.getEpisodeById episodeId)

        -- Clear audio
        let clearUpdate =
              UUT.FileUpdate
                { UUT.efuId = episodeId,
                  UUT.efuAudioFilePath = Nothing,
                  UUT.efuArtworkUrl = Nothing,
                  UUT.efuDurationSeconds = Nothing,
                  UUT.efuClearAudio = True,
                  UUT.efuClearArtwork = False
                }
        _ <- TRX.statement () (UUT.updateEpisodeFiles clearUpdate)
        afterClear <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (episodeId, updateResult, afterUpdate, afterClear)

      assert $ do
        (episodeId, updateResult, mAfterUpdate, mAfterClear) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === episodeId

        afterUpdate <- assertJust mAfterUpdate
        UUT.audioFilePath afterUpdate === Just "/audio/test.mp3"
        UUT.artworkUrl afterUpdate === Just "/images/artwork.jpg"
        UUT.durationSeconds afterUpdate === Just 300

        afterClear <- assertJust mAfterClear
        UUT.audioFilePath afterClear === Nothing
        -- Artwork should still be there (only audio was cleared)
        UUT.artworkUrl afterClear === Just "/images/artwork.jpg"
        pure ()

--------------------------------------------------------------------------------
-- User Query tests

-- | getEpisodesByUser: returns non-deleted episodes created by a user.
prop_getEpisodesByUser :: TestDBConfig -> PropertyT IO ()
prop_getEpisodesByUser cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = fmap (addUTCTime (3600 :: NominalDiffTime)) (UUT.eiScheduledAt ep2Template)}

        id1 <- unwrapInsert (UUT.insertEpisode ep1)
        id2 <- unwrapInsert (UUT.insertEpisode ep2)

        -- Delete one episode
        _ <- TRX.statement () (UUT.deleteEpisode id2)

        episodes <- TRX.statement () (UUT.getEpisodesByUser userId (Limit 10) (Offset 0))

        -- Limit/Offset respected
        limited <- TRX.statement () (UUT.getEpisodesByUser userId (Limit 1) (Offset 0))

        TRX.condemn
        pure (id1, episodes, limited)

      assert $ do
        (id1, episodes, limited) <- assertRight result
        -- Only non-deleted episode returned
        ep <- assertSingleton episodes
        UUT.id ep === id1
        -- Limit respected
        length limited === 1
        pure ()

--------------------------------------------------------------------------------
-- Schedule Update tests

-- | updateScheduledSlot: changes template and scheduled_at.
prop_updateScheduledSlot :: TestDBConfig -> PropertyT IO ()
prop_updateScheduledSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate1 <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    scheduleTemplate2 <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId1) <- insertTestShowWithSchedule showInsert scheduleTemplate1

        -- Create a second schedule template
        let template2WithShowId = scheduleTemplate2 {ShowSchedule.stiShowId = showId}
        templateId2 <- TRX.statement () (ShowSchedule.insertScheduleTemplate template2WithShowId)

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId1, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Use a clean timestamp without sub-microsecond precision (PostgreSQL truncates to microseconds)
        let newScheduledAt = UTCTime (utctDay now) (secondsToDiffTime 3600)
        let slotUpdate = UUT.ScheduleSlotUpdate {UUT.essuId = episodeId, UUT.essuScheduleTemplateId = templateId2, UUT.essuScheduledAt = newScheduledAt}
        updateResult <- TRX.statement () (UUT.updateScheduledSlot slotUpdate)

        afterUpdate <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (episodeId, templateId2, newScheduledAt, updateResult, afterUpdate)

      assert $ do
        (episodeId, expectedTemplateId, expectedScheduledAt, updateResult, mAfterUpdate) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === episodeId

        afterUpdate <- assertJust mAfterUpdate
        UUT.scheduleTemplateId afterUpdate === Just expectedTemplateId
        UUT.scheduledAt afterUpdate === Just expectedScheduledAt
        pure ()

--------------------------------------------------------------------------------
-- Tag tests

-- | getTagsForEpisode: returns tags for an episode.
prop_getTagsForEpisode :: TestDBConfig -> PropertyT IO ()
prop_getTagsForEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Add tags
        TRX.statement () (UUT.replaceEpisodeTags episodeId ["rock", "jazz"])

        tags <- TRX.statement () (UUT.getTagsForEpisode episodeId)

        TRX.condemn
        pure tags

      assert $ do
        tags <- assertRight result
        length tags === 2
        let tagNames = map EpisodeTags.etName tags
        elem "rock" tagNames === True
        elem "jazz" tagNames === True
        pure ()

-- | replaceEpisodeTags: replaces tags atomically (second set replaces first).
prop_replaceEpisodeTags :: TestDBConfig -> PropertyT IO ()
prop_replaceEpisodeTags cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- First set of tags
        TRX.statement () (UUT.replaceEpisodeTags episodeId ["rock", "jazz"])
        tagsAfterFirst <- TRX.statement () (UUT.getTagsForEpisode episodeId)

        -- Replace with second set
        TRX.statement () (UUT.replaceEpisodeTags episodeId ["electronic", "ambient", "chill"])
        tagsAfterSecond <- TRX.statement () (UUT.getTagsForEpisode episodeId)

        -- Replace with empty list
        TRX.statement () (UUT.replaceEpisodeTags episodeId [])
        tagsAfterEmpty <- TRX.statement () (UUT.getTagsForEpisode episodeId)

        TRX.condemn
        pure (tagsAfterFirst, tagsAfterSecond, tagsAfterEmpty)

      assert $ do
        (tagsAfterFirst, tagsAfterSecond, tagsAfterEmpty) <- assertRight result
        -- First set has 2 tags
        length tagsAfterFirst === 2
        -- Second set has 3 tags (completely replaced)
        length tagsAfterSecond === 3
        let secondNames = map EpisodeTags.etName tagsAfterSecond
        elem "electronic" secondNames === True
        elem "ambient" secondNames === True
        elem "chill" secondNames === True
        -- Old tags removed
        elem "rock" secondNames === False
        -- Empty list removes all tags
        length tagsAfterEmpty === 0
        pure ()

--------------------------------------------------------------------------------
-- Unscheduled Episode tests

-- | clearTemplateForUpcomingEpisodes: nulls both schedule fields for future episodes.
prop_clearTemplateForUpcomingEpisodes :: TestDBConfig -> PropertyT IO ()
prop_clearTemplateForUpcomingEpisodes cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert an episode scheduled in the future
        let futureTime = addUTCTime (86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Clear template for upcoming episodes. Pass today's date as the change
        -- date: the episode is scheduled tomorrow, so it is on/after and clears.
        clearedIds <- TRX.statement () (UUT.clearTemplateForUpcomingEpisodes templateId (utctDay now))

        -- Re-fetch the episode
        afterClear <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (episodeId, clearedIds, afterClear)

      assert $ do
        (episodeId, clearedIds, mAfterClear) <- assertRight result
        -- The episode should have been cleared
        clearedIds === [episodeId]
        afterClear <- assertJust mAfterClear
        UUT.scheduleTemplateId afterClear === Nothing
        UUT.scheduledAt afterClear === Nothing
        pure ()

-- | clearTemplateForUpcomingEpisodes: only nulls episodes whose Pacific air date
-- is on or after the change date. An upcoming episode scheduled before that date
-- keeps its slot; one on/after is detached.
prop_clearTemplateForUpcomingEpisodes_dateGate :: TestDBConfig -> PropertyT IO ()
prop_clearTemplateForUpcomingEpisodes_dateGate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    epBeforeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      -- Both episodes are in the future (so the scheduled_at > NOW() guard keeps
      -- them as candidates), but their Pacific air dates straddle the change date.
      -- Clean noon timestamps avoid sub-microsecond round-trip mismatches.
      let baseDay = utctDay now
          fromDate = addDays 5 baseDay
          beforeTime = UTCTime (addDays 1 baseDay) (secondsToDiffTime 43200)
          afterTime = UTCTime (addDays 10 baseDay) (secondsToDiffTime 43200)
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let epBefore = epBeforeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just beforeTime, UUT.eiCreatedBy = userId}
        let epAfter = epAfterTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just afterTime, UUT.eiCreatedBy = userId}

        beforeId <- unwrapInsert (UUT.insertEpisode epBefore)
        afterId <- unwrapInsert (UUT.insertEpisode epAfter)

        clearedIds <- TRX.statement () (UUT.clearTemplateForUpcomingEpisodes templateId fromDate)

        afterBefore <- TRX.statement () (UUT.getEpisodeById beforeId)
        afterAfter <- TRX.statement () (UUT.getEpisodeById afterId)

        TRX.condemn
        pure (afterId, templateId, clearedIds, afterBefore, afterAfter)

      assert $ do
        (afterId, templateId, clearedIds, mAfterBefore, mAfterAfter) <- assertRight result
        -- Only the on/after episode was cleared
        clearedIds === [afterId]
        -- The before-fromDate episode keeps its schedule fields
        beforeEp <- assertJust mAfterBefore
        UUT.scheduleTemplateId beforeEp === Just templateId
        UUT.scheduledAt beforeEp === Just beforeTime
        -- The on/after episode is detached
        afterEp <- assertJust mAfterAfter
        UUT.scheduleTemplateId afterEp === Nothing
        UUT.scheduledAt afterEp === Nothing
        pure ()

-- | getEpisodesForShow: unscheduled episodes (NULL scheduledAt) sort after scheduled ones.
prop_unscheduledEpisodesSortLast :: TestDBConfig -> PropertyT IO ()
prop_unscheduledEpisodesSortLast cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    epTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert a scheduled episode
        let scheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just now, UUT.eiCreatedBy = userId}
        scheduledId <- unwrapInsert (UUT.insertEpisode scheduledInsert)

        -- Insert an unscheduled episode
        let unscheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Nothing, UUT.eiScheduledAt = Nothing, UUT.eiCreatedBy = userId}
        unscheduledId <- unwrapInsert (UUT.insertEpisode unscheduledInsert)

        episodes <- TRX.statement () (UUT.getEpisodesForShow showId (Limit 10) (Offset 0))

        TRX.condemn
        pure (scheduledId, unscheduledId, episodes)

      assert $ do
        (scheduledId, unscheduledId, episodes) <- assertRight result
        -- Both episodes returned, scheduled first (desc order, nulls last)
        case episodes of
          [first, second] -> do
            UUT.id first === scheduledId
            UUT.id second === unscheduledId
          _ -> length episodes === 2
        pure ()

-- | getPublishedEpisodesForShow: excludes episodes with NULL scheduledAt.
prop_publishedExcludesUnscheduled :: TestDBConfig -> PropertyT IO ()
prop_publishedExcludesUnscheduled cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    epTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert a scheduled episode in the past (should appear in published)
        let pastTime = addUTCTime (-86400 :: NominalDiffTime) now
        let scheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just pastTime, UUT.eiCreatedBy = userId}
        scheduledId <- unwrapInsert (UUT.insertEpisode scheduledInsert)

        -- Insert an unscheduled episode (should NOT appear in published)
        let unscheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Nothing, UUT.eiScheduledAt = Nothing, UUT.eiCreatedBy = userId}
        _unscheduledId <- unwrapInsert (UUT.insertEpisode unscheduledInsert)

        published <- TRX.statement () (UUT.getPublishedEpisodesForShow now showId (Limit 10) (Offset 0))

        TRX.condemn
        pure (scheduledId, published)

      assert $ do
        (scheduledId, published) <- assertRight result
        -- Only the scheduled past episode should appear
        ep <- assertSingleton published
        UUT.id ep === scheduledId
        pure ()

--------------------------------------------------------------------------------
-- Template Blocking tests

-- | getUpcomingEpisodesForTemplates: returns an upcoming episode attached to the template.
prop_getUpcomingEpisodesForTemplates_returnsUpcoming :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_returnsUpcoming cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let futureTime = addUTCTime (86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId] (utctDay now))
        TRX.condemn
        pure (episodeId, refs)

      assert $ do
        (episodeId, refs) <- assertRight result
        ref <- assertSingleton refs
        UUT.uerId ref === episodeId
        pure ()

-- | getUpcomingEpisodesForTemplates: excludes episodes scheduled in the past.
prop_getUpcomingEpisodesForTemplates_excludesPast :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_excludesPast cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let pastTime = addUTCTime (-86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just pastTime, UUT.eiCreatedBy = userId}
        _episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId] (utctDay now))
        TRX.condemn
        pure refs

      assert $ do
        refs <- assertRight result
        map UUT.uerId refs === []
        pure ()

-- | getUpcomingEpisodesForTemplates: excludes episodes whose Pacific air date is
-- before the change date, and includes those on/after it. Mirrors the gate that
-- 'clearTemplateForUpcomingEpisodes' applies so the report equals the detach set.
prop_getUpcomingEpisodesForTemplates_dateGate :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_dateGate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    epBeforeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let baseDay = utctDay now
          fromDate = addDays 5 baseDay
          beforeTime = UTCTime (addDays 1 baseDay) (secondsToDiffTime 43200)
          afterTime = UTCTime (addDays 10 baseDay) (secondsToDiffTime 43200)
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let epBefore = epBeforeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just beforeTime, UUT.eiCreatedBy = userId}
        let epAfter = epAfterTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just afterTime, UUT.eiCreatedBy = userId}

        _beforeId <- unwrapInsert (UUT.insertEpisode epBefore)
        afterId <- unwrapInsert (UUT.insertEpisode epAfter)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId] fromDate)
        TRX.condemn
        pure (afterId, refs)

      assert $ do
        (afterId, refs) <- assertRight result
        -- Only the on/after episode is reported; the before-fromDate one is gated out.
        map UUT.uerId refs === [afterId]
        pure ()

-- | getUpcomingEpisodesForTemplates: excludes soft-deleted episodes.
prop_getUpcomingEpisodesForTemplates_excludesDeleted :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_excludesDeleted cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let futureTime = addUTCTime (86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Soft-delete the episode: it must no longer block
        _ <- TRX.statement () (UUT.deleteEpisode episodeId)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId] (utctDay now))
        TRX.condemn
        pure refs

      assert $ do
        refs <- assertRight result
        map UUT.uerId refs === []
        pure ()

-- | getUpcomingEpisodesForTemplates: excludes episodes attached to a different template.
prop_getUpcomingEpisodesForTemplates_excludesOtherTemplate :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_excludesOtherTemplate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate1 <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    scheduleTemplate2 <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId1) <- insertTestShowWithSchedule showInsert scheduleTemplate1

        -- A second template on the same show, which the episode is NOT attached to
        let template2WithShowId = scheduleTemplate2 {ShowSchedule.stiShowId = showId}
        templateId2 <- TRX.statement () (ShowSchedule.insertScheduleTemplate template2WithShowId)

        let futureTime = addUTCTime (86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId1, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        _episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Query for the other template only
        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId2] (utctDay now))
        TRX.condemn
        pure refs

      assert $ do
        refs <- assertRight result
        map UUT.uerId refs === []
        pure ()

-- | getUpcomingEpisodesForTemplates: an empty template list matches nothing.
prop_getUpcomingEpisodesForTemplates_emptyList :: TestDBConfig -> PropertyT IO ()
prop_getUpcomingEpisodesForTemplates_emptyList cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert an upcoming episode; the empty-list query must still return nothing
        let futureTime = addUTCTime (86400 :: NominalDiffTime) now
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        _episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [] (utctDay now))
        TRX.condemn
        pure refs

      assert $ do
        refs <- assertRight result
        map UUT.uerId refs === []
        pure ()

--------------------------------------------------------------------------------
-- Releasing a slot on deactivate

-- | closeSchedulesAndDetachEpisodes: an open window closes on the given date, and
-- a future episode on it is detached and returned.
--
-- This is the path a deactivation or a soft delete takes. An inactive show must not
-- keep a claim on a time slot, or a later reactivation can put two shows on it.
prop_closeSchedules_closesActiveWindow :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_closesActiveWindow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = utctDay now
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        -- Open-ended window that started 30 days ago.
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing))

        -- Whole seconds only. PostgreSQL truncates to microseconds, so a timestamp
        -- built from getCurrentTime does not round-trip.
        let futureTime = UTCTime (addDays 2 today) (secondsToDiffTime 72000)
            episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just futureTime, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        detached <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId today)
        validities <- TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate templateId)
        afterClose <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (episodeId, detached, validities, afterClose)

      assert $ do
        (episodeId, detached, validities, mAfterClose) <- assertRight result
        map UUT.uerId detached === [episodeId]
        validity <- assertSingleton validities
        ShowSchedule.stvEffectiveUntil validity === Just today
        afterClose <- assertJust mAfterClose
        UUT.scheduleTemplateId afterClose === Nothing
        UUT.scheduledAt afterClose === Nothing

-- | closeSchedulesAndDetachEpisodes: a pending window closes to @[from, from)@.
--
-- The end date is @GREATEST(effective_from, closeDate)@. A pending window starts
-- after the close date, so @closeDate@ alone would write @effective_until@ earlier
-- than @effective_from@. An inverted range makes the show vanish from every query
-- that reads the schedule, and nothing reports it. This test fails if @GREATEST@ is
-- removed.
prop_closeSchedules_pendingWindowNeverInverted :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_pendingWindowNeverInverted cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = utctDay now
          pendingFrom = addDays 30 today
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        -- Pending: the window opens 30 days from now.
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId pendingFrom Nothing))

        _ <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId today)
        validities <- TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate templateId)

        TRX.condemn
        pure validities

      assert $ do
        validities <- assertRight result
        validity <- assertSingleton validities
        -- Empty, not inverted. Equal to effective_from, not to today.
        ShowSchedule.stvEffectiveUntil validity === Just pendingFrom
        ShowSchedule.stvEffectiveFrom validity === pendingFrom

-- | closeSchedulesAndDetachEpisodes: a past episode keeps its slot.
--
-- The detach must never reach backwards. A past episode is the record of a
-- broadcast that happened. Nulling it destroys history and cannot be undone.
prop_closeSchedules_keepsPastEpisode :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_keepsPastEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = utctDay now
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing))

        -- Whole seconds only, for the same reason as above.
        let pastTime = UTCTime (addDays (-7) today) (secondsToDiffTime 72000)
            pastInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiScheduledAt = Just pastTime, UUT.eiCreatedBy = userId}
        pastId <- unwrapInsert (UUT.insertEpisode pastInsert)

        detached <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId today)
        afterClose <- TRX.statement () (UUT.getEpisodeById pastId)

        TRX.condemn
        pure (templateId, pastTime, detached, afterClose)

      assert $ do
        (templateId, pastTime, detached, mAfterClose) <- assertRight result
        detached === []
        afterClose <- assertJust mAfterClose
        UUT.scheduleTemplateId afterClose === Just templateId
        UUT.scheduledAt afterClose === Just pastTime

-- | closeSchedulesAndDetachEpisodes: a window that already closed does not move.
--
-- Without the guards, an old window would jump forward to the close date. That
-- reopens a period the show did not hold, and it can manufacture an overlap with
-- whichever show took the slot afterwards.
prop_closeSchedules_leavesClosedWindow :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_leavesClosedWindow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ scheduleTemplateInsertGen (Shows.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = utctDay now
          oldFrom = addDays (-90) today
          oldUntil = addDays (-30) today
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId oldFrom (Just oldUntil)))

        _ <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId today)
        validities <- TRX.statement () (ShowSchedule.getValidityPeriodsForTemplate templateId)

        TRX.condemn
        pure validities

      assert $ do
        validities <- assertRight result
        validity <- assertSingleton validities
        ShowSchedule.stvEffectiveFrom validity === oldFrom
        ShowSchedule.stvEffectiveUntil validity === Just oldUntil

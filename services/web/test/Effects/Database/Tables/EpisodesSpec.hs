module Effects.Database.Tables.EpisodesSpec where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (liftIO)
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
import Test.Database.Helpers (insertTestShowWithSchedule, insertTestUser)
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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}

        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)
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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = addUTCTime (3600 :: NominalDiffTime) (UUT.eiScheduledAt ep2Template)}

        id1 <- TRX.statement () (UUT.insertEpisode ep1)
        id2 <- TRX.statement () (UUT.insertEpisode ep2)

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
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = addUTCTime (3600 :: NominalDiffTime) (UUT.eiScheduledAt ep2Template)}

        id1 <- TRX.statement () (UUT.insertEpisode ep1)
        id2 <- TRX.statement () (UUT.insertEpisode ep2)

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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

        deleteResult <- TRX.statement () (UUT.deleteEpisode episodeId)

        -- getEpisodeById still returns it (no deleted_at filter), but check deleted_at is set
        afterDelete <- TRX.statement () (UUT.getEpisodeById episodeId)

        -- getEpisodesForShow should exclude it
        episodesForShow <- TRX.statement () (UUT.getEpisodesForShow showId (Limit 10) (Offset 0))

        TRX.condemn
        pure (episodeId, deleteResult, afterDelete, episodesForShow)

      assert $ do
        (episodeId, deleteResult, mAfterDelete, episodesForShow) <- assertRight result
        deletedId <- assertJust deleteResult
        deletedId === episodeId

        -- Episode still exists in getById (no soft-delete filter)
        afterDelete <- assertJust mAfterDelete
        _ <- assertJust (UUT.deletedAt afterDelete)

        -- But excluded from getEpisodesForShow
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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId, UUT.eiAudioFilePath = Nothing, UUT.eiArtworkUrl = Nothing}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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

        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId, UUT.eiScheduledAt = addUTCTime (3600 :: NominalDiffTime) (UUT.eiScheduledAt ep2Template)}

        id1 <- TRX.statement () (UUT.insertEpisode ep1)
        id2 <- TRX.statement () (UUT.insertEpisode ep2)

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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId1, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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
        UUT.scheduleTemplateId afterUpdate === expectedTemplateId
        UUT.scheduledAt afterUpdate === expectedScheduledAt
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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = templateId, UUT.eiCreatedBy = userId}
        episodeId <- TRX.statement () (UUT.insertEpisode episodeInsert)

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

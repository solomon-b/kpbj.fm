{-# LANGUAGE QuasiQuotes #-}

module Effects.Database.Tables.EpisodesSpec where

--------------------------------------------------------------------------------

import Control.Monad (forM)
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor qualified as Bifunctor
import Data.Foldable (traverse_)
import Data.Int (Int64)
import Data.List (find, isInfixOf)
import Data.Maybe (isJust, listToMaybe)
import Data.Text (Text)
import Data.Time.Calendar (Day, DayOfWeek (..), addDays, dayOfWeek, fromGregorian, toGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Time.LocalTime (LocalTime (..), TimeOfDay (..))
import Domain.Types.Limit (Limit (..))
import Domain.Types.Offset (Offset (..))
import Domain.Types.Timezone (pacificDay, pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.EpisodeTags qualified as EpisodeTags
import Effects.Database.Tables.Episodes qualified as UUT
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Hasql.Interpolate (interp, sql)
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Hedgehog (MonadGen, PropertyT, cover, (/==), (===))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Internal.Property (forAllT)
import Hedgehog.Range qualified as Range
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, bracketConn, withTestDB)
import Test.Database.Property (act, arrange, assert, runs)
import Test.Database.Property.Assert (assertJust, assertNothing, assertRight, assertSingleton)
import Test.Gen.Tables.Episodes (episodeInsertGen)
import Test.Gen.Tables.ShowSchedule (airDayForTemplate, airTimeOn, genRecurringScheduleInsert, lastAirDayBefore)
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
        runs 10 . it "published listings split on the air time, not the date" $
          hedgehog . prop_publishedListingsSplitOnAirTime
        runs 10 . it "published listings sort on the air time, not the publish date" $
          hedgehog . prop_publishedListingsSortOnAirTime
        runs 10 . it "isUnaired answers from the template, and agrees with the database" $
          hedgehog . prop_isUnairedAgreesWithTheDatabase
        runs 10 . it "getEpisodeByShowAndNumber: looks up by show slug + episode number" $
          hedgehog . prop_getEpisodeByShowAndNumber
        runs 10 . it "getEpisodeByShowAndNumber: an archived episode is gone" $
          hedgehog . prop_archivedEpisodeIsNotFoundByNumber
        runs 10 . it "IncludeArchived: staff reads see the archived episode" $
          hedgehog . prop_includeArchivedSeesArchivedEpisode

      describe "Unarchive" $ do
        runs 10 . it "restoreEpisode: puts the episode back on the public site" $
          hedgehog . prop_restoreEpisodeClearsDeletedAt
        runs 10 . it "restoreEpisode: leaves a live episode alone" $
          hedgehog . prop_restoreEpisodeIgnoresLiveEpisode
        runs 10 . it "getLiveEpisodeAtAirTime: finds the episode that took the slot" $
          hedgehog . prop_liveEpisodeAtAirTimeFindsTheHolder

      describe "RSS feeds" $ do
        runs 10 . it "feed queries: list aired episodes with audio, newest air time first" $
          hedgehog . prop_feedQueriesListAiredEpisodesWithAudio

      describe "Episode numbering" $ do
        runs 10 . it "set_episode_number: consecutive inserts number 1, 2, 3" $
          hedgehog . prop_episodeNumbersAreConsecutive
        runs 10 . it "unique_episode_number: a repeated number is rejected" $
          hedgehog . prop_duplicateEpisodeNumberRejected

      describe "Slot reuse" $ do
        runs 10 . it "unique_episode_air_date: two live episodes cannot share an air time" $
          hedgehog . prop_twoLiveEpisodesCannotShareAnAirTime
        runs 10 . it "unique_episode_air_date: a soft-deleted episode releases its air time" $
          hedgehog . prop_deletedEpisodeReleasesItsSlot

      describe "Slot windows" $ do
        runs 20 . it "getSlotWindowsBetween: the window at an instant holds the episode getAiringEpisodes airs" $
          hedgehog . prop_slotWindowsAgreeWithAiring
        runs 10 . it "returns a window with no episode" $
          hedgehog . prop_slotWindowWithNoEpisode
        runs 10 . it "returns the primary and the replay window" $
          hedgehog . prop_slotWindowsPrimaryAndReplay
        runs 10 . it "gives no episode when the episode length is unknown" $
          hedgehog . prop_slotWindowUnknownLength
        runs 1 . it "opens a 01:00 slot on the fall-back date when getAiringEpisodes does" $
          hedgehog . prop_slotWindowOnFallBackDate

      describe "Play logging" $ do
        runs 10 . it "getEpisodeByAudioPath: finds a soft-deleted episode" $
          hedgehog . prop_audioPathFindsDeletedEpisode

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
        runs 10 . it "updateScheduledSlot: changes template and air date" $
          hedgehog . prop_updateScheduledSlot
        runs 10 . it "clearScheduledSlot: nulls both halves and frees the slot" $
          hedgehog . prop_clearScheduledSlot

      describe "Unscheduled Episodes" $ do
        runs 10 . it "clearTemplateForUpcomingEpisodes: nulls schedule fields for future episodes" $
          hedgehog . prop_clearTemplateForUpcomingEpisodes
        runs 10 . it "clearTemplateForUpcomingEpisodes: a same-day change splits on the air time" $
          hedgehog . prop_sameDayChangeSplitsOnAirTime
        runs 10 . it "clearTemplateForUpcomingEpisodes: only clears episodes on/after the change date" $
          hedgehog . prop_clearTemplateForUpcomingEpisodes_dateGate
        runs 10 . it "migrateUpcomingEpisodes: moves upcoming episodes and keeps their air times" $
          hedgehog . prop_migrateUpcomingEpisodes
        runs 10 . it "migrateUpcomingEpisodes: only moves episodes on/after the change date" $
          hedgehog . prop_migrateUpcomingEpisodes_dateGate
        runs 10 . it "migrateUpcomingEpisodesAiringOn: moves the dates the new template airs, leaves the rest" $
          hedgehog . prop_migrateUpcomingEpisodesAiringOn
        runs 10 . it "migrateUpcomingEpisodesAiringOn: moves nothing onto a template with no window" $
          hedgehog . prop_migrateUpcomingEpisodesAiringOn_needsAWindow
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
        runs 10 . it "closeSchedulesAndDetachEpisodes: only detaches episodes on/after the close date" $
          hedgehog . prop_closeSchedules_dateGate
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
  UUT.eiAirDate insert === UUT.airDate model
  UUT.eiAudioFilePath insert === UUT.audioFilePath model
  UUT.eiAudioFileSize insert === UUT.audioFileSize model
  UUT.eiAudioMimeType insert === UUT.audioMimeType model
  UUT.eiDurationSeconds insert === UUT.durationSeconds model
  UUT.eiArtworkUrl insert === UUT.artworkUrl model

--------------------------------------------------------------------------------
-- Lens Laws

-- | Insert-Select: insert then select returns what we inserted.
-- | A generated template pinned to a midday airing, with no replay.
--
-- The date-gate tests need the air date the SQL derives to equal the date the
-- test asked for. 'genRecurringScheduleInsert' picks any of five timezones, and
-- the statements read the air date from @air_date@
-- while the fixtures build the instant in Pacific. A midday airing sits far
-- enough from both midnights that every one of those zones gives the same date,
-- so the timezone stays varied without making the date ambiguous.
--
-- The replay goes, because the generator places one after the template's
-- original end time and that no longer holds once the times are pinned. None of
-- the statements under test reads it.
middayTemplate :: ShowSchedule.ScheduleTemplateInsert -> ShowSchedule.ScheduleTemplateInsert
middayTemplate t =
  t
    { ShowSchedule.stiStartTime = TimeOfDay 12 0 0,
      ShowSchedule.stiEndTime = TimeOfDay 13 0 0,
      ShowSchedule.stiReplayStartTime = Nothing
    }

-- | A fixed day for fixtures that never look at when the episode airs.
--
-- 'airDayForTemplate' moves it to the first date on or after this one that the
-- template actually airs, which is what the air-date trigger requires.
fixtureBaseDay :: Day
fixtureBaseDay = fromGregorian 2026 1 1

prop_insertSelect :: TestDBConfig -> PropertyT IO ()
prop_insertSelect cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}

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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateEpisodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateATemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    updateBTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- A different air date, so (show_id, air_date) stays unique
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate (addDays 1 (airDayForTemplate scheduleTemplate fixtureBaseDay)))}

        id1 <- unwrapInsert (UUT.insertEpisode ep1)
        id2 <- unwrapInsert (UUT.insertEpisode ep2)

        -- Soft-delete one episode
        _ <- TRX.statement () (UUT.deleteEpisode id2)

        episodes <- TRX.statement () (UUT.getEpisodesForShow showId UUT.ExcludeArchived (Limit 10) (Offset 0))
        TRX.condemn
        pure (id1, episodes)

      assert $ do
        (id1, episodes) <- assertRight result
        -- Only non-deleted episode should be returned
        ep <- assertSingleton episodes
        UUT.id ep === id1
        pure ()

-- | 'UUT.isUnaired' answers from the template, and agrees with the database.
--
-- The Haskell derivation, 'ShowSchedule.templateAirTime', is a second
-- implementation of the @episode_air_time@ SQL function, for the callers that
-- hold a loaded template rather than a query. Two implementations can drift, so
-- this pins them together: the same show and episode are asked in both places.
--
-- An episode holding no template has not aired, whatever its date says.
prop_isUnairedAgreesWithTheDatabase :: TestDBConfig -> PropertyT IO ()
prop_isUnairedAgreesWithTheDatabase cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    templateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = pacificDay now
          template =
            (middayTemplate templateGen)
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiTimezone = "America/Los_Angeles"
              }
          airTime = today

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert template
        episodeId <-
          unwrapInsert . UUT.insertEpisode $
            epGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just airTime, UUT.eiCreatedBy = userId}
        mEpisode <- TRX.statement () (UUT.getEpisodeById episodeId)
        mTemplate <- TRX.statement () (ShowSchedule.getScheduleTemplateById templateId)
        -- The database's own answer to "has this aired", through the query that
        -- decides what the public sees.
        listed <- TRX.statement () (UUT.getPublishedEpisodesForShow now showId (Limit 10) (Offset 0))
        TRX.condemn
        pure (episodeId, mEpisode, mTemplate, listed)

      assert $ do
        (episodeId, mEpisode, mTemplate, listed) <- assertRight result
        episode <- assertJust mEpisode
        template' <- assertJust mTemplate
        -- Haskell and SQL agree on whether this episode has aired.
        UUT.isAired now (Just template') episode === (map UUT.id listed == [episodeId])
        -- isUnaired is its complement.
        UUT.isUnaired now (Just template') episode === not (UUT.isAired now (Just template') episode)
        -- An episode with no template has not aired, whatever its date.
        UUT.isUnaired now Nothing episode === True

-- | The published listings split on the episode's air time, not on its date.
--
-- These two queries decide what a listener sees on a show page and in the
-- archive. An episode that aired this morning is public; one airing tonight is
-- not, even though both fall on today. Two shows rather than two episodes of one
-- show, because a show holds one episode per date.
--
-- The timezone is pinned to Pacific here. The fixtures build air instants with
-- 'airTimeOn', which is Pacific, while the query reads the air date through
-- @st.timezone@. Those agree only when the template is Pacific, and every
-- template in production is. 'middayTemplate' keeps the timezone varied for the
-- tests that can afford an ambiguous date; this one compares instants, so it
-- cannot.
--
-- The expectations are computed from the same instants the query compares, so
-- this states that Postgres and Haskell agree rather than restating the rule.
prop_publishedListingsSplitOnAirTime :: TestDBConfig -> PropertyT IO ()
prop_publishedListingsSplitOnAirTime cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    earlyShowInsert <- forAllT showInsertGen
    lateShowInsert <- forAllT showInsertGen
    earlyTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    lateTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    earlyEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    lateEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = pacificDay now
          airsTodayAt t start end =
            t
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiStartTime = start,
                ShowSchedule.stiEndTime = end,
                ShowSchedule.stiTimezone = "America/Los_Angeles",
                ShowSchedule.stiReplayStartTime = Nothing
              }
          earlyTemplate = airsTodayAt earlyTemplateGen (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
          lateTemplate = airsTodayAt lateTemplateGen (TimeOfDay 23 0 0) (TimeOfDay 23 30 0)
          -- Both episodes air today. The instants are what the query compares, so
          -- the expectation reads them from the template the same way it does.
          earlyTime = airTimeOn earlyTemplate today
          lateTime = airTimeOn lateTemplate today

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (earlyShowId, earlyTemplateId) <- insertTestShowWithSchedule earlyShowInsert earlyTemplate
        (lateShowId, lateTemplateId) <- insertTestShowWithSchedule lateShowInsert lateTemplate
        earlyId <-
          unwrapInsert . UUT.insertEpisode $
            earlyEpGen {UUT.eiId = earlyShowId, UUT.eiScheduleTemplateId = Just earlyTemplateId, UUT.eiAirDate = Just today, UUT.eiCreatedBy = userId}
        lateId <-
          unwrapInsert . UUT.insertEpisode $
            lateEpGen {UUT.eiId = lateShowId, UUT.eiScheduleTemplateId = Just lateTemplateId, UUT.eiAirDate = Just today, UUT.eiCreatedBy = userId}

        earlyListed <- TRX.statement () (UUT.getPublishedEpisodesForShow now earlyShowId (Limit 10) (Offset 0))
        lateListed <- TRX.statement () (UUT.getPublishedEpisodesForShow now lateShowId (Limit 10) (Offset 0))
        archived <- TRX.statement () (UUT.getPublishedEpisodesWithShows now (Limit 100) (Offset 0))

        TRX.condemn
        pure (earlyId, lateId, earlyListed, lateListed, archived)

      assert $ do
        (earlyId, lateId, earlyListed, lateListed, archived) <- assertRight result
        let listedIf episodeId airTime = [episodeId | airTime <= now]
            archivedIds = map (UUT.id . fst) archived
        map UUT.id earlyListed === listedIf earlyId earlyTime
        map UUT.id lateListed === listedIf lateId lateTime
        -- The archive applies the same rule across every show.
        filter (== earlyId) archivedIds === listedIf earlyId earlyTime
        filter (== lateId) archivedIds === listedIf lateId lateTime
        pure ()

-- | The archive queries list episodes newest first by air time.
--
-- A host can upload an episode days before it airs, so the upload order and
-- the air order can differ. In this fixture the first episode to air has the
-- newer publish date. A sort on @published_at@ puts it first, and a sort on
-- the air time puts it last.
prop_publishedListingsSortOnAirTime :: TestDBConfig -> PropertyT IO ()
prop_publishedListingsSortOnAirTime cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let oneDay = 24 * 60 * 60
          twoDaysAgo = addUTCTime (negate (2 * oneDay)) now
          oneDayAgo = addUTCTime (negate oneDay) now
          -- Two past dates the show airs on. The base is well in the past.
          firstAirDay = airDayForTemplate scheduleTemplate feedFixtureBaseDay
          secondAirDay = airDayForTemplate scheduleTemplate (addDays 1 firstAirDay)

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        let onDay airDay =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just airDay,
                  UUT.eiCreatedBy = userId
                }
        airedFirstId <- unwrapInsert (UUT.insertEpisode (onDay firstAirDay))
        airedSecondId <- unwrapInsert (UUT.insertEpisode (onDay secondAirDay))

        -- The episode that aired first was uploaded last.
        TRX.statement () $
          interp @() False [sql| UPDATE episodes SET published_at = #{oneDayAgo} WHERE id = #{airedFirstId} |]
        TRX.statement () $
          interp @() False [sql| UPDATE episodes SET published_at = #{twoDaysAgo} WHERE id = #{airedSecondId} |]

        showListing <- TRX.statement () (UUT.getPublishedEpisodesForShow now showId (Limit 10) (Offset 0))
        archive <- TRX.statement () (UUT.getPublishedEpisodesWithShows now (Limit 1000) (Offset 0))
        TRX.condemn
        pure (airedFirstId, airedSecondId, showListing, archive)

      assert $ do
        (airedFirstId, airedSecondId, showListing, archive) <- assertRight result
        map UUT.id showListing === [airedSecondId, airedFirstId]
        -- The archive holds rows from other tests too, so look only at the
        -- rows of this fixture.
        let archiveIds = filter (`elem` [airedFirstId, airedSecondId]) (map (UUT.id . fst) archive)
        archiveIds === [airedSecondId, airedFirstId]

-- | getPublishedEpisodesForShow: filters by non-deleted and past schedule.
prop_getPublishedEpisodesForShow :: TestDBConfig -> PropertyT IO ()
prop_getPublishedEpisodesForShow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- A different air date, so (show_id, air_date) stays unique
        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate (addDays 1 (airDayForTemplate scheduleTemplate fixtureBaseDay)))}

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
        -- Non-deleted episode may or may not appear depending on its air date vs now.
        case published of
          [] -> pure () -- the air date is in the future, OK
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- Get the episode to find its number
        mEpisode <- TRX.statement () (UUT.getEpisodeById episodeId)

        lookupResult <- case mEpisode of
          Nothing -> pure (episodeId, Nothing, Nothing)
          Just ep -> do
            let showSlug = Shows.siSlug showInsert
            let epNumber = UUT.episodeNumber ep
            byShowAndNumber <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber UUT.ExcludeArchived)
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
-- RSS feeds

-- | The two feed queries list the aired episodes that a listener can play,
-- newest first by air time.
--
-- The fixture has one show with seven episodes, and a second show that staff
-- deleted. Each excluded episode breaks exactly one rule, so each rule is
-- tested alone:
--
-- * archived: @deleted_at@ is set
-- * no audio: @audio_file_path@ is NULL
-- * not aired yet: the air date is in the future
-- * deleted show: the episode is fine, but its show is deleted
--
-- Four episodes qualify. Two of them show that @published_at@ does not
-- matter: one has no publish date, and one has a publish date in the future.
-- The first episode to air has the newest past publish date, so the order
-- shows that the queries sort on the air time and not on @published_at@.
--
-- The insert stamps @published_at@ with the transaction time, which comes after
-- @now@. So the fixture writes a past publish date on every episode first, and
-- then changes the three that test @published_at@.
--
-- The live template is pinned to Pacific, because the expected air instants
-- come from 'airTimeOn', which is Pacific.
prop_feedQueriesListAiredEpisodesWithAudio :: TestDBConfig -> PropertyT IO ()
prop_feedQueriesListAiredEpisodesWithAudio cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    liveShowInsert <- forAllT showInsertGen
    deletedShowInsert <- forAllT showInsertGen
    liveTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    deletedShowTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let oneDay = 24 * 60 * 60
          twoDaysAgo = addUTCTime (negate (2 * oneDay)) now
          oneDayAgo = addUTCTime (negate oneDay) now
          oneDayAhead = addUTCTime oneDay now
          liveTemplate = liveTemplateGen {ShowSchedule.stiTimezone = "America/Los_Angeles"}

          -- Six past dates the live show airs on, one for each episode that
          -- has aired. The base is far enough back that all six are in the past.
          nextAirDay day = airDayForTemplate liveTemplate (addDays 1 day)
          airDay1 = airDayForTemplate liveTemplate feedFixtureBaseDay
          airDay2 = nextAirDay airDay1
          airDay3 = nextAirDay airDay2
          airDay4 = nextAirDay airDay3
          airDay5 = nextAirDay airDay4
          airDay6 = nextAirDay airDay5
          futureAirDay = airDayForTemplate liveTemplate (addDays 30 (utctDay now))
          deletedShowAirDay = airDayForTemplate deletedShowTemplate feedFixtureBaseDay

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (liveShowId, liveTemplateId) <- insertTestShowWithSchedule liveShowInsert liveTemplate
        (deletedShowId, deletedTemplateId) <- insertTestShowWithSchedule deletedShowInsert deletedShowTemplate

        let withAudio showId templateId airDay =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just airDay,
                  UUT.eiCreatedBy = userId,
                  UUT.eiAudioFilePath = Just "audio/episodes/2025/01/06/feed-fixture_2025-01-06_0b7c.mp3",
                  UUT.eiAudioFileSize = Just 57671680,
                  UUT.eiAudioMimeType = Just "audio/mpeg"
                }
            withoutAudio showId templateId airDay =
              (withAudio showId templateId airDay)
                { UUT.eiAudioFilePath = Nothing,
                  UUT.eiAudioFileSize = Nothing,
                  UUT.eiAudioMimeType = Nothing
                }
            insertOn = unwrapInsert . UUT.insertEpisode

        uploadedLastId <- insertOn (withAudio liveShowId liveTemplateId airDay1)
        uploadedFirstId <- insertOn (withAudio liveShowId liveTemplateId airDay2)
        archivedId <- insertOn (withAudio liveShowId liveTemplateId airDay3)
        noAudioId <- insertOn (withoutAudio liveShowId liveTemplateId airDay4)
        unpublishedId <- insertOn (withAudio liveShowId liveTemplateId airDay5)
        publishedLaterId <- insertOn (withAudio liveShowId liveTemplateId airDay6)
        notAiredId <- insertOn (withAudio liveShowId liveTemplateId futureAirDay)
        deletedShowEpisodeId <- insertOn (withAudio deletedShowId deletedTemplateId deletedShowAirDay)

        -- A past publish date on every episode, then the changes under test.
        TRX.statement () $
          interp @()
            False
            [sql|
          UPDATE episodes SET published_at = #{twoDaysAgo}
          WHERE show_id IN (#{liveShowId}, #{deletedShowId})
        |]
        TRX.statement () $
          interp @() False [sql| UPDATE episodes SET published_at = #{oneDayAgo} WHERE id = #{uploadedLastId} |]
        TRX.statement () $
          interp @() False [sql| UPDATE episodes SET published_at = NULL WHERE id = #{unpublishedId} |]
        TRX.statement () $
          interp @() False [sql| UPDATE episodes SET published_at = #{oneDayAhead} WHERE id = #{publishedLaterId} |]
        _ <- TRX.statement () (UUT.deleteEpisode archivedId)
        _ <- TRX.statement () (Shows.softDeleteShow deletedShowId)

        showFeed <- TRX.statement () (UUT.getFeedEpisodesForShow now liveShowId)
        stationFeed <- TRX.statement () (UUT.getFeedEpisodesWithShows now)
        TRX.condemn

        let fixtureIds =
              [uploadedLastId, uploadedFirstId, archivedId, noAudioId, unpublishedId, publishedLaterId, notAiredId, deletedShowEpisodeId]
            expected =
              [ (publishedLaterId, airTimeOn liveTemplate airDay6),
                (unpublishedId, airTimeOn liveTemplate airDay5),
                (uploadedFirstId, airTimeOn liveTemplate airDay2),
                (uploadedLastId, airTimeOn liveTemplate airDay1)
              ]
        pure (liveShowId, expected, fixtureIds, showFeed, stationFeed)

      assert $ do
        (liveShowId, expected, fixtureIds, showFeed, stationFeed) <- assertRight result
        -- The show feed holds the four qualifying episodes, newest air time
        -- first, each with the instant it aired.
        map (Bifunctor.first UUT.id) showFeed === expected
        -- The station feed holds rows from other tests too, so look only at
        -- the rows of this fixture.
        let stationRows = filter (\row -> UUT.id (UUT.sfrEpisode row) `elem` fixtureIds) stationFeed
        map (\row -> (UUT.id (UUT.sfrEpisode row), UUT.sfrAiredAt row)) stationRows === expected
        map (Shows.id . UUT.sfrShow) stationRows === replicate 4 liveShowId

-- | A base date for the feed fixture, well in the past.
--
-- The fixture needs six past air dates of one template. A template can air on
-- the fifth week of the month only, so six air dates can take more than a year.
feedFixtureBaseDay :: Day
feedFixtureBaseDay = fromGregorian 2024 1 1

--------------------------------------------------------------------------------
-- Episode numbering

-- | Three dates the template airs on, from 'fixtureBaseDay' forward.
--
-- The three dates must differ. @unique_episode_air_date@ permits one episode per
-- show per instant.
threeAirDays :: ShowSchedule.ScheduleTemplateInsert -> [Day]
threeAirDays template =
  take 3 (iterate next (airDayForTemplate template fixtureBaseDay))
  where
    next day = airDayForTemplate template (addDays 1 day)

-- | The trigger gives a show's episodes the numbers 1, 2, and 3 in insert order.
--
-- 'UUT.insertEpisode' sends no number. The column defaults to 1, and
-- @set_episode_number@ replaces it with the show's @MAX + 1@ under a per-show
-- advisory lock.
prop_episodeNumbersAreConsecutive :: TestDBConfig -> PropertyT IO ()
prop_episodeNumbersAreConsecutive cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let insertOn day =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just day,
                  UUT.eiCreatedBy = userId
                }
            numberOf day = do
              episodeId <- unwrapInsert (UUT.insertEpisode (insertOn day))
              mEpisode <- TRX.statement () (UUT.getEpisodeById episodeId)
              pure (fmap UUT.episodeNumber mEpisode)

        numbers <- traverse numberOf (threeAirDays scheduleTemplate)
        TRX.condemn
        pure numbers

      assert $ do
        numbers <- assertRight result
        numbers === [Just 1, Just 2, Just 3]

-- | @unique_episode_number@ rejects a number the show already holds.
--
-- No part of the application sends an explicit number, so the second insert is raw
-- SQL. It reproduces the outcome the advisory lock prevents. Two concurrent uploads
-- read the same @MAX@ and both write the same number.
prop_duplicateEpisodeNumberRejected :: TestDBConfig -> PropertyT IO ()
prop_duplicateEpisodeNumberRejected cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let insertOn day =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just day,
                  UUT.eiCreatedBy = userId
                }
            airDays = threeAirDays scheduleTemplate

        -- Two episodes. The trigger gives them the numbers 1 and 2.
        traverse_ (unwrapInsert . UUT.insertEpisode . insertOn) (take 2 airDays)

        -- Write 2 again, on a third date. A third date stops
        -- unique_episode_air_date from firing first. The trigger keeps an
        -- explicit number unless it is 1, so this reaches unique_episode_number.
        let thirdAirTime = last airDays
        TRX.statement () $
          interp @()
            False
            [sql|
          INSERT INTO episodes (show_id, episode_number, schedule_template_id, air_date, created_by)
          VALUES (#{showId}, 2, #{templateId}, #{thirdAirTime}, #{userId})
        |]

        TRX.condemn

      assert $ do
        let outcome = case result of
              Right () -> "accepted"
              Left err
                | "unique_episode_number" `isInfixOf` show err -> "rejected by unique_episode_number"
                | otherwise -> "rejected for another reason: " <> show err
        outcome === "rejected by unique_episode_number"

--------------------------------------------------------------------------------
-- Slot reuse

-- | Two live episodes of one show cannot hold the same air time.
--
-- This is the half of @unique_episode_air_date@ that stops a double booking. The
-- partial index narrows the rule to the live rows. It does not relax it.
prop_twoLiveEpisodesCannotShareAnAirTime :: TestDBConfig -> PropertyT IO ()
prop_twoLiveEpisodesCannotShareAnAirTime cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let bookSlot =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }

        _ <- unwrapInsert (UUT.insertEpisode bookSlot)
        -- The same air time, with the first episode still live. The trigger gives this
        -- one number 2, so unique_episode_number cannot be what fires.
        _ <- unwrapInsert (UUT.insertEpisode bookSlot)

        TRX.condemn

      assert $ do
        let outcome = case result of
              Right () -> "accepted"
              Left err
                | "unique_episode_air_date" `isInfixOf` show err -> "rejected by unique_episode_air_date"
                | otherwise -> "rejected for another reason: " <> show err
        outcome === "rejected by unique_episode_air_date"

-- | A soft-deleted episode releases its air time, so a new episode can take it.
--
-- @unique_episode_air_date@ is a partial index over the live rows, so a deleted
-- row holds nothing. 'prop_twoLiveEpisodesCannotShareAnAirTime' covers the other
-- direction, which the delete here must not weaken.
prop_deletedEpisodeReleasesItsSlot :: TestDBConfig -> PropertyT IO ()
prop_deletedEpisodeReleasesItsSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let bookSlot =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }

        firstId <- unwrapInsert (UUT.insertEpisode bookSlot)
        _ <- TRX.statement () (UUT.deleteEpisode firstId)
        -- The same air time, now that the first episode no longer holds it.
        secondId <- unwrapInsert (UUT.insertEpisode bookSlot)

        TRX.condemn
        pure (firstId, secondId)

      assert $ do
        let outcome = case result of
              Right (firstId, secondId)
                | firstId /= secondId -> "the freed air time was reusable"
                | otherwise -> "the two inserts returned one id"
              Left err
                | "unique_episode_air_date" `isInfixOf` show err ->
                    "unique_episode_air_date still held the deleted episode's air time"
                | otherwise -> "failed for another reason: " <> show err
        outcome === "the freed air time was reusable"

--------------------------------------------------------------------------------
-- Mutation tests

-- | deleteEpisode: soft delete sets deleted_at.
prop_deleteEpisode :: TestDBConfig -> PropertyT IO ()
prop_deleteEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        deleteResult <- TRX.statement () (UUT.deleteEpisode episodeId)

        -- getEpisodeById filters on deleted_at IS NULL, so it should return Nothing
        afterDelete <- TRX.statement () (UUT.getEpisodeById episodeId)

        -- getEpisodesForShow should also exclude it
        episodesForShow <- TRX.statement () (UUT.getEpisodesForShow showId UUT.ExcludeArchived (Limit 10) (Offset 0))

        TRX.condemn
        pure (episodeId, deleteResult, afterDelete, episodesForShow)

      assert $ do
        (episodeId, deleteResult, mAfterDelete, episodesForShow) <- assertRight result
        deleted <- assertJust deleteResult
        UUT.id deleted === episodeId
        -- deleteEpisode returns the archived row, so deleted_at is set on it.
        UUT.deletedAt deleted /== Nothing

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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        firstDelete <- TRX.statement () (UUT.deleteEpisode episodeId)
        secondDelete <- TRX.statement () (UUT.deleteEpisode episodeId)

        TRX.condemn
        pure (episodeId, firstDelete, secondDelete)

      assert $ do
        (episodeId, firstDelete, secondDelete) <- assertRight result
        -- Both deletes return the row (no WHERE deleted_at IS NULL)
        firstDeleted <- assertJust firstDelete
        UUT.id firstDeleted === episodeId
        secondDeleted <- assertJust secondDelete
        UUT.id secondDeleted === episodeId
        pure ()

--------------------------------------------------------------------------------
-- File Update tests

-- | updateEpisodeFiles: updates audio/artwork paths.
prop_updateEpisodeFiles :: TestDBConfig -> PropertyT IO ()
prop_updateEpisodeFiles cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert with no audio/artwork
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId, UUT.eiAudioFilePath = Nothing, UUT.eiArtworkUrl = Nothing}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    ep1Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    ep2Template <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let ep1 = ep1Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        let ep2 = ep2Template {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiCreatedBy = userId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate (addDays 1 (airDayForTemplate scheduleTemplate fixtureBaseDay)))}

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

-- | updateScheduledSlot: changes template and air date.
prop_updateScheduledSlot :: TestDBConfig -> PropertyT IO ()
prop_updateScheduledSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate1 <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    scheduleTemplate2 <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId1) <- insertTestShowWithSchedule showInsert scheduleTemplate1

        -- Create a second schedule template
        let template2WithShowId = scheduleTemplate2 {ShowSchedule.stiShowId = showId}
        templateId2 <- TRX.statement () (ShowSchedule.insertScheduleTemplate template2WithShowId)

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId1, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate1 fixtureBaseDay), UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        -- The episode moves onto the second template, so the new date has to be one
        -- that template airs on.
        let newAirDate = airDayForTemplate scheduleTemplate2 (pacificDay now)
        let slotUpdate = UUT.ScheduleSlotUpdate {UUT.essuId = episodeId, UUT.essuScheduleTemplateId = templateId2, UUT.essuAirDate = newAirDate}
        updateResult <- TRX.statement () (UUT.updateScheduledSlot slotUpdate)

        afterUpdate <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (episodeId, templateId2, newAirDate, updateResult, afterUpdate)

      assert $ do
        (episodeId, expectedTemplateId, expectedAirDate, updateResult, mAfterUpdate) <- assertRight result
        updatedId <- assertJust updateResult
        updatedId === episodeId

        afterUpdate <- assertJust mAfterUpdate
        UUT.scheduleTemplateId afterUpdate === Just expectedTemplateId
        UUT.airDate afterUpdate === Just expectedAirDate
        pure ()

-- | clearScheduledSlot: both columns become NULL, and the air time is free.
--
-- The free slot is as important as the NULL values. @unique_episode_air_date@
-- covers the live rows. A second episode can take the air time only after the
-- first episode releases it.
prop_clearScheduledSlot :: TestDBConfig -> PropertyT IO ()
prop_clearScheduledSlot cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    secondTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let airTime = airDayForTemplate scheduleTemplate fixtureBaseDay
            episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just airTime, UUT.eiCreatedBy = userId}
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        clearResult <- TRX.statement () (UUT.clearScheduledSlot episodeId)
        afterClear <- TRX.statement () (UUT.getEpisodeById episodeId)

        -- The slot is free, so a second episode can take the same air time.
        let successorInsert = secondTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just airTime, UUT.eiCreatedBy = userId}
        successorId <- unwrapInsert (UUT.insertEpisode successorInsert)

        TRX.condemn
        pure (episodeId, clearResult, afterClear, successorId)

      assert $ do
        (episodeId, clearResult, mAfterClear, _successorId) <- assertRight result
        clearedId <- assertJust clearResult
        clearedId === episodeId

        afterClear <- assertJust mAfterClear
        UUT.scheduleTemplateId afterClear === Nothing
        UUT.airDate afterClear === Nothing

--------------------------------------------------------------------------------
-- Tag tests

-- | getTagsForEpisode: returns tags for an episode.
prop_getTagsForEpisode :: TestDBConfig -> PropertyT IO ()
prop_getTagsForEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
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

-- | A schedule change effective today splits on the episode's own air time.
--
-- The guard asks whether the episode's airing is still ahead, and the air time
-- lives on the template. An episode whose show already aired today keeps its
-- slot, because clearing it would destroy the record of a real airing. One
-- airing later today is detached, because its template's window closes today and
-- it would otherwise never air.
--
-- Two shows rather than two episodes of one show, because a show holds one
-- episode per date. One airs at 00:00 and one at 23:00, so on any run one has
-- aired and the other has not.
--
-- The expectation is computed from the same instant the statement compares
-- against, so this asserts that Postgres and Haskell agree on when an episode
-- airs. It does not restate the rule in a second place.
--
-- The timezone is pinned to Pacific for that reason. The expectation builds its
-- instant with 'airTimeOn', which is Pacific, while the statement reads the
-- start time through @st.timezone@. Those agree only when the template is
-- Pacific, and every template in production is.
prop_sameDayChangeSplitsOnAirTime :: TestDBConfig -> PropertyT IO ()
prop_sameDayChangeSplitsOnAirTime cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    earlyShowInsert <- forAllT showInsertGen
    lateShowInsert <- forAllT showInsertGen
    earlyTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    lateTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    earlyEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    lateEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = pacificDay now
          airsTodayAt template start end =
            template
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiStartTime = start,
                ShowSchedule.stiEndTime = end,
                ShowSchedule.stiTimezone = "America/Los_Angeles"
              }
          earlyTemplate = airsTodayAt earlyTemplateGen (TimeOfDay 0 0 0) (TimeOfDay 1 0 0)
          lateTemplate = airsTodayAt lateTemplateGen (TimeOfDay 23 0 0) (TimeOfDay 23 30 0)

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (earlyShowId, earlyTemplateId) <- insertTestShowWithSchedule earlyShowInsert earlyTemplate
        (lateShowId, lateTemplateId) <- insertTestShowWithSchedule lateShowInsert lateTemplate
        earlyId <-
          unwrapInsert . UUT.insertEpisode $
            earlyEpGen
              { UUT.eiId = earlyShowId,
                UUT.eiScheduleTemplateId = Just earlyTemplateId,
                UUT.eiAirDate = Just today,
                UUT.eiCreatedBy = userId
              }
        lateId <-
          unwrapInsert . UUT.insertEpisode $
            lateEpGen
              { UUT.eiId = lateShowId,
                UUT.eiScheduleTemplateId = Just lateTemplateId,
                UUT.eiAirDate = Just today,
                UUT.eiCreatedBy = userId
              }
        earlyDetached <- TRX.statement () (UUT.clearTemplateForUpcomingEpisodes earlyTemplateId today)
        lateDetached <- TRX.statement () (UUT.clearTemplateForUpcomingEpisodes lateTemplateId today)
        TRX.condemn
        pure (earlyId, lateId, earlyDetached, lateDetached)

      assert $ do
        (earlyId, lateId, earlyDetached, lateDetached) <- assertRight result
        let detachedIf episodeId template =
              [episodeId | airTimeOn template today > now]
        earlyDetached === detachedIf earlyId earlyTemplate
        lateDetached === detachedIf lateId lateTemplate
        pure ()

-- | clearTemplateForUpcomingEpisodes: nulls both schedule fields for future episodes.
prop_clearTemplateForUpcomingEpisodes :: TestDBConfig -> PropertyT IO ()
prop_clearTemplateForUpcomingEpisodes cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert an episode scheduled in the future
        let futureTime = airDayForTemplate scheduleTemplate (addDays 1 (utctDay now))
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
        UUT.airDate afterClear === Nothing
        pure ()

-- | clearTemplateForUpcomingEpisodes: only nulls episodes whose Pacific air date
-- is on or after the change date. An upcoming episode scheduled before that date
-- keeps its slot; one on/after is detached.
prop_clearTemplateForUpcomingEpisodes_dateGate :: TestDBConfig -> PropertyT IO ()
prop_clearTemplateForUpcomingEpisodes_dateGate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epBeforeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      -- Both episodes are in the future, so the air-time guard keeps
      -- them as candidates, and their Pacific air dates straddle the change date.
      -- The dates are the template's next two airings, so the change date is the
      -- boundary itself rather than an arbitrary day between them.
      let baseDay = utctDay now
          beforeDay = airDayForTemplate scheduleTemplate (addDays 1 baseDay)
          afterDay = airDayForTemplate scheduleTemplate (addDays 1 beforeDay)
          fromDate = afterDay
          beforeTime = beforeDay
          afterTime = afterDay
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let epBefore = epBeforeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just beforeTime, UUT.eiCreatedBy = userId}
        let epAfter = epAfterTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just afterTime, UUT.eiCreatedBy = userId}

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
        UUT.airDate beforeEp === Just beforeTime
        -- The on/after episode is detached
        afterEp <- assertJust mAfterAfter
        UUT.scheduleTemplateId afterEp === Nothing
        UUT.airDate afterEp === Nothing
        pure ()

-- | migrateUpcomingEpisodes: moves the upcoming episodes and keeps their air times.
--
-- A deferred replay change writes a second template carrying the new replay time
-- and moves the upcoming episodes onto it. The primary window does not move, so
-- every moved episode still airs at the same instant, and only the template id
-- changes.
--
-- This mirrors the two 'clearTemplateForUpcomingEpisodes' tests, because the two
-- statements apply the same gates and split the same set of episodes. An episode
-- that has already aired stays where it is, and both episodes here sit on or
-- after the change date so that the air-time guard is what separates them.
prop_migrateUpcomingEpisodes :: TestDBConfig -> PropertyT IO ()
prop_migrateUpcomingEpisodes cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    oldTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    newTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    pastEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    futureEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      -- A weekly show airing at 00:00. Today's episode has already gone out and
      -- next week's has not, so both share a date gate and only the air-time
      -- guard separates them. Putting the aired episode on an earlier date would
      -- let the date gate do all the work and leave the guard untested.
      let today = pacificDay now
          airsAtMidnight t =
            t
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiStartTime = TimeOfDay 0 0 0,
                ShowSchedule.stiEndTime = TimeOfDay 1 0 0
              }
          weeklyTemplate = airsAtMidnight oldTemplate
          pastTime = today
          futureTime = addDays 7 today

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, oldTemplateId) <- insertTestShowWithSchedule showInsert weeklyTemplate

        -- The replacement template the episodes move onto.
        newTemplateId <-
          TRX.statement () . ShowSchedule.insertScheduleTemplate $
            newTemplateGen {ShowSchedule.stiShowId = showId}

        pastId <-
          unwrapInsert . UUT.insertEpisode $
            pastEpGen
              { UUT.eiId = showId,
                UUT.eiScheduleTemplateId = Just oldTemplateId,
                UUT.eiAirDate = Just pastTime,
                UUT.eiCreatedBy = userId
              }
        futureId <-
          unwrapInsert . UUT.insertEpisode $
            futureEpGen
              { UUT.eiId = showId,
                UUT.eiScheduleTemplateId = Just oldTemplateId,
                UUT.eiAirDate = Just futureTime,
                UUT.eiCreatedBy = userId
              }

        migrated <- TRX.statement () (UUT.migrateUpcomingEpisodes oldTemplateId newTemplateId today)

        afterPast <- TRX.statement () (UUT.getEpisodeById pastId)
        afterFuture <- TRX.statement () (UUT.getEpisodeById futureId)

        TRX.condemn
        pure (futureId, oldTemplateId, newTemplateId, migrated, afterPast, afterFuture)

      assert $ do
        (futureId, oldTemplateId, newTemplateId, migrated, mPast, mFuture) <- assertRight result
        -- Only the upcoming episode moves.
        migrated === [futureId]
        -- The episode that already aired keeps its template and its air time.
        pastEp <- assertJust mPast
        UUT.scheduleTemplateId pastEp === Just oldTemplateId
        UUT.airDate pastEp === Just pastTime
        -- The upcoming episode changes template and nothing else.
        futureEp <- assertJust mFuture
        UUT.scheduleTemplateId futureEp === Just newTemplateId
        UUT.airDate futureEp === Just futureTime
        pure ()

-- | migrateUpcomingEpisodes: only moves episodes on or after the change date.
--
-- A deferred change keeps the interim episodes on the old template, because the
-- old slot still airs them until the change takes effect. Both episodes here are
-- upcoming, so the air-time guard admits both and only the date gate separates
-- them.
prop_migrateUpcomingEpisodes_dateGate :: TestDBConfig -> PropertyT IO ()
prop_migrateUpcomingEpisodes_dateGate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    oldTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    newTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epBeforeGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let oldTemplate = middayTemplate oldTemplateGen
          baseDay = pacificDay now
          beforeDay = airDayForTemplate oldTemplate (addDays 1 baseDay)
          afterDay = airDayForTemplate oldTemplate (addDays 1 beforeDay)
          fromDate = afterDay
          beforeTime = beforeDay
          afterTime = afterDay

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, oldTemplateId) <- insertTestShowWithSchedule showInsert oldTemplate
        newTemplateId <-
          TRX.statement () . ShowSchedule.insertScheduleTemplate $
            (middayTemplate newTemplateGen) {ShowSchedule.stiShowId = showId}

        beforeId <-
          unwrapInsert . UUT.insertEpisode $
            epBeforeGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just oldTemplateId, UUT.eiAirDate = Just beforeTime, UUT.eiCreatedBy = userId}
        afterId <-
          unwrapInsert . UUT.insertEpisode $
            epAfterGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just oldTemplateId, UUT.eiAirDate = Just afterTime, UUT.eiCreatedBy = userId}

        migrated <- TRX.statement () (UUT.migrateUpcomingEpisodes oldTemplateId newTemplateId fromDate)

        afterBefore <- TRX.statement () (UUT.getEpisodeById beforeId)
        afterAfter <- TRX.statement () (UUT.getEpisodeById afterId)

        TRX.condemn
        pure (afterId, oldTemplateId, newTemplateId, migrated, afterBefore, afterAfter)

      assert $ do
        (afterId, oldTemplateId, newTemplateId, migrated, mBefore, mAfter) <- assertRight result
        -- Only the on/after episode moved.
        migrated === [afterId]
        -- The interim episode keeps the old template and its air time.
        beforeEp <- assertJust mBefore
        UUT.scheduleTemplateId beforeEp === Just oldTemplateId
        UUT.airDate beforeEp === Just beforeTime
        -- The on/after episode is on the new template, at the same air time.
        afterEp <- assertJust mAfter
        UUT.scheduleTemplateId afterEp === Just newTemplateId
        UUT.airDate afterEp === Just afterTime

-- | The first date after @from@ whose day of the month is in the first week.
--
-- Adding 14 days to it lands in the third week, on the same weekday, inside the same
-- month. So the pair gives one recurrence two dates that a week set can separate.
firstWeekDayAfter :: Day -> Day
firstWeekDayAfter from =
  let inFirstWeek day = let (_, _, dom) = toGregorian day in dom <= 7
   in case filter inFirstWeek (take 400 (iterate (addDays 1) (addDays 1 from))) of
        (day : _) -> day
        [] -> from

-- | A weeks-only change keeps the episodes the new week set still covers.
--
-- The show airs every week and then narrows to the first week only. One upcoming
-- episode sits in the first week of its month and one in the third. The replacement
-- template airs the first and not the third, so the first moves and the third stays
-- behind for 'clearTemplateForUpcomingEpisodes' to detach.
--
-- The two statements run in that order and split one set between them, which is the
-- pair the show edit handler applies. Asserting both here is the point: a moved
-- episode must keep its date, and the episode left behind must lose its slot rather
-- than sit on a template that does not air it.
prop_migrateUpcomingEpisodesAiringOn :: TestDBConfig -> PropertyT IO ()
prop_migrateUpcomingEpisodesAiringOn cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    oldTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    firstWeekEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    thirdWeekEpGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = pacificDay now
          firstWeekDay = firstWeekDayAfter today
          thirdWeekDay = addDays 14 firstWeekDay
          everyWeek = (middayTemplate oldTemplateGen) {ShowSchedule.stiDayOfWeek = dayOfWeek firstWeekDay, ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5]}
          -- Same weekday and same primary window. Only the weeks narrow.
          firstWeekOnly = everyWeek {ShowSchedule.stiWeeksOfMonth = [1]}

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, oldTemplateId) <- insertTestShowWithSchedule showInsert everyWeek

        newTemplateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate firstWeekOnly {ShowSchedule.stiShowId = showId})
        _ <- TRX.statement () (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert newTemplateId today Nothing))

        firstWeekId <-
          unwrapInsert . UUT.insertEpisode $
            firstWeekEpGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just oldTemplateId, UUT.eiAirDate = Just firstWeekDay, UUT.eiCreatedBy = userId}
        thirdWeekId <-
          unwrapInsert . UUT.insertEpisode $
            thirdWeekEpGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just oldTemplateId, UUT.eiAirDate = Just thirdWeekDay, UUT.eiCreatedBy = userId}

        migrated <- TRX.statement () (UUT.migrateUpcomingEpisodesAiringOn oldTemplateId newTemplateId today)
        detached <- TRX.statement () (UUT.clearTemplateForUpcomingEpisodes oldTemplateId today)

        afterFirst <- TRX.statement () (UUT.getEpisodeById firstWeekId)
        afterThird <- TRX.statement () (UUT.getEpisodeById thirdWeekId)

        TRX.condemn
        pure (firstWeekId, thirdWeekId, newTemplateId, migrated, detached, afterFirst, afterThird)

      assert $ do
        (firstWeekId, thirdWeekId, newTemplateId, migrated, detached, mFirst, mThird) <- assertRight result
        -- The two statements split the set. Neither episode reaches both.
        migrated === [firstWeekId]
        detached === [thirdWeekId]
        -- The covered episode moves and keeps its date, so it airs at the same instant.
        firstEp <- assertJust mFirst
        UUT.scheduleTemplateId firstEp === Just newTemplateId
        UUT.airDate firstEp === Just firstWeekDay
        -- The dropped episode holds no slot at all.
        thirdEp <- assertJust mThird
        UUT.scheduleTemplateId thirdEp === Nothing
        UUT.airDate thirdEp === Nothing

-- | A template with no validity window airs on no date, so it takes no episode.
--
-- The window is half of the predicate, and the recurrence alone would pass here. A
-- template with no window is the state 'insertScheduleSlot' refuses to leave behind,
-- and an episode moved onto one could never reach the transmitter.
prop_migrateUpcomingEpisodesAiringOn_needsAWindow :: TestDBConfig -> PropertyT IO ()
prop_migrateUpcomingEpisodesAiringOn_needsAWindow cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    oldTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = pacificDay now
          airDay = firstWeekDayAfter today
          everyWeek = (middayTemplate oldTemplateGen) {ShowSchedule.stiDayOfWeek = dayOfWeek airDay, ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5]}

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, oldTemplateId) <- insertTestShowWithSchedule showInsert everyWeek

        -- Same recurrence as the old template, and no validity row.
        newTemplateId <- TRX.statement () (ShowSchedule.insertScheduleTemplate everyWeek {ShowSchedule.stiShowId = showId})

        episodeId <-
          unwrapInsert . UUT.insertEpisode $
            episodeGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just oldTemplateId, UUT.eiAirDate = Just airDay, UUT.eiCreatedBy = userId}

        migrated <- TRX.statement () (UUT.migrateUpcomingEpisodesAiringOn oldTemplateId newTemplateId today)
        after <- TRX.statement () (UUT.getEpisodeById episodeId)

        TRX.condemn
        pure (oldTemplateId, migrated, after)

      assert $ do
        (oldTemplateId, migrated, mEpisode) <- assertRight result
        migrated === []
        episode <- assertJust mEpisode
        UUT.scheduleTemplateId episode === Just oldTemplateId
        UUT.airDate episode === Just airDay

-- | closeSchedulesAndDetachEpisodes: only detaches episodes on or after the close date.
--
-- A show that closes its window on a future date keeps airing until then, so the
-- episodes before that date keep their slots. Both episodes here are upcoming,
-- so the air-time guard admits both and only the date gate separates them.
prop_closeSchedules_dateGate :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_dateGate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplateGen <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epBeforeGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterGen <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let scheduleTemplate = middayTemplate scheduleTemplateGen
          baseDay = pacificDay now
          beforeDay = airDayForTemplate scheduleTemplate (addDays 1 baseDay)
          afterDay = airDayForTemplate scheduleTemplate (addDays 1 beforeDay)
          closeDate = afterDay
          beforeTime = beforeDay
          afterTime = afterDay

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) baseDay) Nothing))

        beforeId <-
          unwrapInsert . UUT.insertEpisode $
            epBeforeGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just beforeTime, UUT.eiCreatedBy = userId}
        afterId <-
          unwrapInsert . UUT.insertEpisode $
            epAfterGen {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just afterTime, UUT.eiCreatedBy = userId}

        detached <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId closeDate)

        afterBefore <- TRX.statement () (UUT.getEpisodeById beforeId)
        afterAfter <- TRX.statement () (UUT.getEpisodeById afterId)

        TRX.condemn
        pure (afterId, templateId, detached, afterBefore, afterAfter)

      assert $ do
        (afterId, templateId, detached, mBefore, mAfter) <- assertRight result
        -- Only the on/after episode is detached and reported.
        map UUT.uerId detached === [afterId]
        -- The interim episode keeps its slot, so it still airs before the close.
        beforeEp <- assertJust mBefore
        UUT.scheduleTemplateId beforeEp === Just templateId
        UUT.airDate beforeEp === Just beforeTime
        -- The on/after episode is detached.
        afterEp <- assertJust mAfter
        UUT.scheduleTemplateId afterEp === Nothing
        UUT.airDate afterEp === Nothing

-- | getEpisodesForShow: unscheduled episodes (NULL air_date) sort after scheduled ones.
prop_unscheduledEpisodesSortLast :: TestDBConfig -> PropertyT IO ()
prop_unscheduledEpisodesSortLast cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert a scheduled episode
        let scheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay), UUT.eiCreatedBy = userId}
        scheduledId <- unwrapInsert (UUT.insertEpisode scheduledInsert)

        -- Insert an unscheduled episode
        let unscheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Nothing, UUT.eiAirDate = Nothing, UUT.eiCreatedBy = userId}
        unscheduledId <- unwrapInsert (UUT.insertEpisode unscheduledInsert)

        episodes <- TRX.statement () (UUT.getEpisodesForShow showId UUT.ExcludeArchived (Limit 10) (Offset 0))

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

-- | getPublishedEpisodesForShow: excludes episodes with NULL air_date.
prop_publishedExcludesUnscheduled :: TestDBConfig -> PropertyT IO ()
prop_publishedExcludesUnscheduled cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert a scheduled episode in the past (should appear in published)
        let pastTime = lastAirDayBefore scheduleTemplate (utctDay now)
        let scheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just pastTime, UUT.eiCreatedBy = userId}
        scheduledId <- unwrapInsert (UUT.insertEpisode scheduledInsert)

        -- Insert an unscheduled episode (should NOT appear in published)
        let unscheduledInsert = epTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Nothing, UUT.eiAirDate = Nothing, UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let futureTime = airDayForTemplate scheduleTemplate (addDays 1 (utctDay now))
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      -- The episode airs at 00:00 today, so it has already gone out but still
      -- sits on the change date. The date gate admits it, and the air-time guard
      -- is what excludes it. An episode on an earlier date would be gated out on
      -- the date alone and leave the guard untested.
      let today = pacificDay now
          airedToday =
            scheduleTemplate
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiStartTime = TimeOfDay 0 0 0,
                ShowSchedule.stiEndTime = TimeOfDay 1 0 0
              }
          pastTime = today
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert airedToday

        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just pastTime, UUT.eiCreatedBy = userId}
        _episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        refs <- TRX.statement () (UUT.getUpcomingEpisodesForTemplates [templateId] today)
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    epBeforeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    epAfterTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let baseDay = utctDay now
          beforeDay = airDayForTemplate scheduleTemplate (addDays 1 baseDay)
          afterDay = airDayForTemplate scheduleTemplate (addDays 1 beforeDay)
          fromDate = afterDay
          beforeTime = beforeDay
          afterTime = afterDay
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let epBefore = epBeforeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just beforeTime, UUT.eiCreatedBy = userId}
        let epAfter = epAfterTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just afterTime, UUT.eiCreatedBy = userId}

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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let futureTime = airDayForTemplate scheduleTemplate (addDays 1 (utctDay now))
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
    scheduleTemplate1 <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    scheduleTemplate2 <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId1) <- insertTestShowWithSchedule showInsert scheduleTemplate1

        -- A second template on the same show, which the episode is NOT attached to
        let template2WithShowId = scheduleTemplate2 {ShowSchedule.stiShowId = showId}
        templateId2 <- TRX.statement () (ShowSchedule.insertScheduleTemplate template2WithShowId)

        let futureTime = airDayForTemplate scheduleTemplate1 (addDays 1 (utctDay now))
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId1, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        -- Insert an upcoming episode; the empty-list query must still return nothing
        let futureTime = airDayForTemplate scheduleTemplate (addDays 1 (utctDay now))
        let episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      let today = utctDay now
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
        -- Open-ended window that started 30 days ago.
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing))

        -- The next date this template airs on, so the episode is a real upcoming
        -- airing. Its start time carries whole minutes, so the timestamp round-trips
        -- through PostgreSQL, which a value from getCurrentTime would not.
        let futureTime = airDayForTemplate scheduleTemplate (addDays 1 today)
            episodeInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just futureTime, UUT.eiCreatedBy = userId}
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
        UUT.airDate afterClose === Nothing

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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)

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
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      now <- liftIO getCurrentTime
      -- The episode airs at 00:00 today, so it has already gone out but still
      -- sits on the close date. The date gate therefore admits it and the
      -- air-time guard is what keeps it attached. An episode on an earlier date
      -- would be excluded by the date gate alone and leave the guard untested.
      let today = pacificDay now
          airedToday =
            scheduleTemplate
              { ShowSchedule.stiDayOfWeek = dayOfWeek today,
                ShowSchedule.stiWeeksOfMonth = [1, 2, 3, 4, 5],
                ShowSchedule.stiStartTime = TimeOfDay 0 0 0,
                ShowSchedule.stiEndTime = TimeOfDay 1 0 0
              }
          pastTime = today
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert airedToday
        _ <- unwrapInsert (ShowSchedule.insertValidity (ShowSchedule.ValidityInsert templateId (addDays (-30) today) Nothing))

        let pastInsert = episodeTemplate {UUT.eiId = showId, UUT.eiScheduleTemplateId = Just templateId, UUT.eiAirDate = Just pastTime, UUT.eiCreatedBy = userId}
        pastId <- unwrapInsert (UUT.insertEpisode pastInsert)

        detached <- TRX.statement () (UUT.closeSchedulesAndDetachEpisodes showId today)
        afterClose <- TRX.statement () (UUT.getEpisodeById pastId)

        TRX.condemn
        pure (templateId, pastTime, detached, afterClose)

      assert $ do
        (templateId, pastTime', detached, mAfterClose) <- assertRight result
        detached === []
        afterClose <- assertJust mAfterClose
        UUT.scheduleTemplateId afterClose === Just templateId
        UUT.airDate afterClose === Just pastTime'

-- | closeSchedulesAndDetachEpisodes: a window that already closed does not move.
--
-- Without the guards, an old window would jump forward to the close date. That
-- reopens a period the show did not hold, and it can manufacture an overlap with
-- whichever show took the slot afterwards.
prop_closeSchedules_leavesClosedWindow :: TestDBConfig -> PropertyT IO ()
prop_closeSchedules_leavesClosedWindow cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)

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

-- | getEpisodeByShowAndNumber must not return an archived episode.
--
-- Archive is the station's moderation tool. This query backs the public episode
-- page, so a row it returns is a row the public can see and play.
prop_archivedEpisodeIsNotFoundByNumber :: TestDBConfig -> PropertyT IO ()
prop_archivedEpisodeIsNotFoundByNumber cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)

        let showSlug = Shows.siSlug showInsert
        mBefore <- TRX.statement () (UUT.getEpisodeById episodeId)
        epNumber <- case mBefore of
          Nothing -> pure 0
          Just ep -> pure (UUT.episodeNumber ep)

        found <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber UUT.ExcludeArchived)
        _ <- TRX.statement () (UUT.deleteEpisode episodeId)
        afterArchive <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber UUT.ExcludeArchived)

        TRX.condemn
        pure (found, afterArchive)

      assert $ do
        (found, afterArchive) <- assertRight result
        live <- assertJust found
        UUT.deletedAt live === Nothing
        assertNothing afterArchive

-- | IncludeArchived must return what ExcludeArchived hides.
--
-- The dashboard reads this way for staff and admins, so they can moderate what
-- they archived. Both queries take the filter, so both are checked here.
prop_includeArchivedSeesArchivedEpisode :: TestDBConfig -> PropertyT IO ()
prop_includeArchivedSeesArchivedEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        mBefore <- TRX.statement () (UUT.getEpisodeById episodeId)
        epNumber <- case mBefore of
          Nothing -> pure 0
          Just ep -> pure (UUT.episodeNumber ep)

        _ <- TRX.statement () (UUT.deleteEpisode episodeId)

        let showSlug = Shows.siSlug showInsert
        byNumber <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber UUT.IncludeArchived)
        listed <- TRX.statement () (UUT.getEpisodesForShow showId UUT.IncludeArchived (Limit 10) (Offset 0))
        hidden <- TRX.statement () (UUT.getEpisodesForShow showId UUT.ExcludeArchived (Limit 10) (Offset 0))

        TRX.condemn
        pure (episodeId, byNumber, listed, hidden)

      assert $ do
        (episodeId, byNumber, listed, hidden) <- assertRight result
        found <- assertJust byNumber
        UUT.id found === episodeId
        map UUT.id listed === [episodeId]
        map UUT.id hidden === []

-- | restoreEpisode must clear deleted_at, so the episode is public again.
prop_restoreEpisodeClearsDeletedAt :: TestDBConfig -> PropertyT IO ()
prop_restoreEpisodeClearsDeletedAt cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        mBefore <- TRX.statement () (UUT.getEpisodeById episodeId)
        epNumber <- case mBefore of
          Nothing -> pure 0
          Just ep -> pure (UUT.episodeNumber ep)

        _ <- TRX.statement () (UUT.deleteEpisode episodeId)
        restored <- TRX.statement () (UUT.restoreEpisode episodeId)

        let showSlug = Shows.siSlug showInsert
        publicAgain <- TRX.statement () (UUT.getEpisodeByShowAndNumber showSlug epNumber UUT.ExcludeArchived)

        TRX.condemn
        pure (episodeId, restored, publicAgain)

      assert $ do
        (episodeId, restored, publicAgain) <- assertRight result
        restoredRow <- assertJust restored
        UUT.id restoredRow === episodeId
        -- restoreEpisode returns the live row, so deleted_at is already clear.
        UUT.deletedAt restoredRow === Nothing
        live <- assertJust publicAgain
        UUT.deletedAt live === Nothing

-- | restoreEpisode must report that a live episode had nothing to restore.
--
-- The handler turns the Nothing into a named message rather than claiming that
-- it unarchived something.
prop_restoreEpisodeIgnoresLiveEpisode :: TestDBConfig -> PropertyT IO ()
prop_restoreEpisodeIgnoresLiveEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId
                }
        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        restored <- TRX.statement () (UUT.restoreEpisode episodeId)

        TRX.condemn
        pure restored

      assert $ do
        restored <- assertRight result
        restored === Nothing

-- | getLiveEpisodeAtAirTime must find the episode that took an archived slot.
--
-- unique_episode_air_date covers the live rows only, so a second episode can
-- claim the air time while the first sits archived. The unarchive handler runs
-- this to refuse with a message instead of failing on the index.
prop_liveEpisodeAtAirTimeFindsTheHolder :: TestDBConfig -> PropertyT IO ()
prop_liveEpisodeAtAirTimeFindsTheHolder cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let airTime = airDayForTemplate scheduleTemplate fixtureBaseDay
            episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just airTime,
                  UUT.eiCreatedBy = userId
                }

        firstId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        -- Nothing holds the slot while the first episode is live.
        freeWhileLive <- TRX.statement () (UUT.getLiveEpisodeAtAirDate showId airTime firstId)

        _ <- TRX.statement () (UUT.deleteEpisode firstId)
        -- The partial index frees the slot, so a second episode can take it.
        secondId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        holder <- TRX.statement () (UUT.getLiveEpisodeAtAirDate showId airTime firstId)

        TRX.condemn
        pure (secondId, freeWhileLive, holder)

      assert $ do
        (secondId, freeWhileLive, holder) <- assertRight result
        fmap UUT.id freeWhileLive === Nothing
        taken <- assertJust holder
        UUT.id taken === secondId

--------------------------------------------------------------------------------
-- Play logging

-- | getEpisodeByAudioPath must find an episode that someone soft-deleted.
--
-- The one caller logs a play to @playback_history@ after the audio went out over
-- the air. A filter on @deleted_at@ there loses the association when a host
-- deletes an episode while it plays, and the play then counts against nothing.
prop_audioPathFindsDeletedEpisode :: TestDBConfig -> PropertyT IO ()
prop_audioPathFindsDeletedEpisode cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    scheduleTemplate <- forAllT $ genRecurringScheduleInsert (Shows.Id 1)
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate

        let audioPath = "audio/episodes/2026/08/08/played-while-deleted.mp3"
            episodeInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiAirDate = Just (airDayForTemplate scheduleTemplate fixtureBaseDay),
                  UUT.eiCreatedBy = userId,
                  UUT.eiAudioFilePath = Just audioPath
                }

        episodeId <- unwrapInsert (UUT.insertEpisode episodeInsert)
        _ <- TRX.statement () (UUT.deleteEpisode episodeId)
        found <- TRX.statement () (UUT.getEpisodeByAudioPath audioPath)

        TRX.condemn
        pure (episodeId, found)

      assert $ do
        (episodeId, found) <- assertRight result
        episode <- assertJust found
        UUT.id episode === episodeId

--------------------------------------------------------------------------------
-- Slot windows

-- | The episode that a test puts in one slot.
data EpisodeShape
  = -- | No episode row.
    NoEpisode
  | -- | An episode row with no audio.
    EpisodeWithoutAudio
  | -- | An episode with audio that is longer than its slot by the given
    -- number of seconds.
    EpisodeFillingSlot Int64
  deriving stock (Show)

genEpisodeShape :: (MonadGen m) => m EpisodeShape
genEpisodeShape =
  Gen.choice
    [ pure NoEpisode,
      pure EpisodeWithoutAudio,
      EpisodeFillingSlot <$> Gen.integral (Range.linear 0 600)
    ]

-- | One test instant, as an offset from the opening of a window.
data InstantChoice = InstantChoice
  { -- | True for the second air date, False for the first.
    icSecondAirDay :: Bool,
    -- | True for the replay window. A template with no replay uses the primary.
    icReplay :: Bool,
    -- | The offset from the window opening. It can be before or after the window.
    icOffsetSeconds :: Int64
  }
  deriving stock (Show)

genInstantChoice :: (MonadGen m) => Int64 -> m InstantChoice
genInstantChoice slotSeconds = do
  secondAirDay <- Gen.bool
  replay <- Gen.bool
  -- A constant range. A linear range stays near its origin for the few small
  -- sizes that 10 runs reach, and then no instant falls inside the window.
  offsetSeconds <- Gen.integral (Range.constant (-3600) (slotSeconds + 3600))
  pure (InstantChoice secondAirDay replay offsetSeconds)

-- | A template for the agreement property. Half of them are overnight.
--
-- 'genRecurringScheduleInsert' gives a start before the end. An overnight
-- template swaps the two, so the slot wraps midnight. It has no replay, because
-- the generated replay time fits only the original end.
genSlotTemplate :: (MonadGen m) => m ShowSchedule.ScheduleTemplateInsert
genSlotTemplate = do
  template <- genRecurringScheduleInsert (Shows.Id 1)
  overnight <- Gen.bool
  pure $
    if overnight
      then
        template
          { ShowSchedule.stiStartTime = ShowSchedule.stiEndTime template,
            ShowSchedule.stiEndTime = ShowSchedule.stiStartTime template,
            ShowSchedule.stiReplayStartTime = Nothing
          }
      else template

-- | The length of a template slot in seconds, from its clock times. A slot
-- whose end is at or before its start wraps midnight.
slotSecondsOf :: ShowSchedule.ScheduleTemplateInsert -> Int64
slotSecondsOf template =
  if endSeconds > startSeconds
    then endSeconds - startSeconds
    else 86400 - (startSeconds - endSeconds)
  where
    secondsOfDay time = fromIntegral (todHour time * 3600 + todMin time * 60)
    startSeconds = secondsOfDay (ShowSchedule.stiStartTime template)
    endSeconds = secondsOfDay (ShowSchedule.stiEndTime template)

-- | The instant that an 'InstantChoice' names.
instantFor :: ShowSchedule.ScheduleTemplateInsert -> Day -> Day -> InstantChoice -> UTCTime
instantFor template firstAirDay secondAirDay choice =
  addUTCTime (fromIntegral (icOffsetSeconds choice)) windowOpen
  where
    airDay = if icSecondAirDay choice then secondAirDay else firstAirDay
    openTime = case (icReplay choice, ShowSchedule.stiReplayStartTime template) of
      (True, Just replayStart) -> replayStart
      _ -> ShowSchedule.stiStartTime template
    windowOpen = pacificToUtc (LocalTime airDay openTime)

-- | The episode insert for one slot, or Nothing for 'NoEpisode'.
--
-- An episode with audio is one hour longer than its slot, plus the extra
-- seconds. On the fall-back date a slot can hold one more elapsed hour than its
-- clock times say, and the audio must still fill it.
shapedEpisode :: UUT.Insert -> Int64 -> Text -> Day -> EpisodeShape -> Maybe UUT.Insert
shapedEpisode baseInsert slotSeconds audioPath airDay shape =
  case shape of
    NoEpisode -> Nothing
    EpisodeWithoutAudio ->
      Just datedInsert {UUT.eiAudioFilePath = Nothing}
    EpisodeFillingSlot extraSeconds ->
      Just
        datedInsert
          { UUT.eiAudioFilePath = Just audioPath,
            UUT.eiDurationSeconds = Just (slotSeconds + 3600 + extraSeconds)
          }
  where
    datedInsert = baseInsert {UUT.eiAirDate = Just airDay}

-- | getSlotWindowsBetween agrees with getAiringEpisodes.
--
-- At each test instant, the window that holds the instant names the episode
-- that 'UUT.getAiringEpisodes' puts first. When no episode airs, the window
-- names none, or no window holds the instant.
--
-- Every episode with audio fills its slot. The two queries differ on purpose
-- for a short episode, or one of unknown length. 'UUT.getAiringEpisodes' stops a
-- short episode early and plays an unknown length to the end of the slot. The
-- slot window gives no episode for an unknown length. The other tests cover
-- that case.
prop_slotWindowsAgreeWithAiring :: TestDBConfig -> PropertyT IO ()
prop_slotWindowsAgreeWithAiring cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    template <- forAllT genSlotTemplate
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    anchorOffset <- forAllT $ Gen.integral (Range.constant 0 364)
    firstShape <- forAllT genEpisodeShape
    secondShape <- forAllT genEpisodeShape
    let slotSeconds = slotSecondsOf template
    instantChoices <- forAllT $ Gen.list (Range.singleton 20) (genInstantChoice slotSeconds)

    act $ do
      let firstAirDay = airDayForTemplate template (addDays anchorOffset fixtureBaseDay)
          secondAirDay = airDayForTemplate template (addDays 1 firstAirDay)
          instants = map (instantFor template firstAirDay secondAirDay) instantChoices

      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert {Shows.siStatus = Shows.Active} template
        _ <-
          unwrapInsert $
            ShowSchedule.insertValidity $
              ShowSchedule.ValidityInsert templateId (addDays (-30) firstAirDay) Nothing

        let baseInsert =
              episodeTemplate
                { UUT.eiId = showId,
                  UUT.eiScheduleTemplateId = Just templateId,
                  UUT.eiCreatedBy = userId
                }
            firstEpisode = shapedEpisode baseInsert slotSeconds "audio/episodes/first.mp3" firstAirDay firstShape
            secondEpisode = shapedEpisode baseInsert slotSeconds "audio/episodes/second.mp3" secondAirDay secondShape
        mapM_ insertTestEpisode firstEpisode
        mapM_ insertTestEpisode secondEpisode

        observations <- forM instants $ \instant -> do
          windows <- TRX.statement () (UUT.getSlotWindowsBetween instant (addUTCTime 1 instant))
          airing <- TRX.statement () (UUT.getAiringEpisodes instant)
          pure (instant, windows, airing)

        TRX.condemn
        pure observations

      assert $ do
        observations <- assertRight result
        let fromWindows = [(instant, windowEpisodeAt instant windows) | (instant, windows, _) <- observations]
            fromAiring = [(instant, firstAiringEpisode airing) | (instant, _, airing) <- observations]
        -- Fail when too few runs land an instant on an airing episode, so a
        -- weak generator cannot pass in silence.
        cover 20 "an episode airs at some instant" (any (isJust . snd) fromAiring)
        fromWindows === fromAiring
  where
    -- The episode of the window that holds the instant, if a window holds it.
    windowEpisodeAt instant windows =
      let holdsInstant window = UUT.swStart window <= instant && instant < UUT.swEnd window
       in UUT.seId <$> (find holdsInstant windows >>= UUT.swEpisode)
    -- The episode of the first airing row, if there is one.
    firstAiringEpisode airing =
      case listToMaybe airing of
        Nothing -> Nothing
        Just airingEpisode ->
          let episode = UUT.aeEpisode airingEpisode
           in Just (UUT.id episode)

-- | Monday, January 6, 2025. Every fixed slot window test uses this date.
slotTestDay :: Day
slotTestDay = fromGregorian 2025 1 6

-- | A Pacific clock time on 'slotTestDay', or on a later date.
slotTestTime :: Integer -> TimeOfDay -> UTCTime
slotTestTime daysLater time = pacificToUtc (LocalTime (addDays daysLater slotTestDay) time)

-- | Insert an active show with a Monday 14:00 to 16:00 slot and the given
-- replay start. The slot is valid from December 1, 2024 with no end.
insertMondaySlot :: Shows.Insert -> Maybe TimeOfDay -> TRX.Transaction (Shows.Id, ShowSchedule.TemplateId)
insertMondaySlot showInsert replayStart = do
  let template =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 1,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 16 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = replayStart
          }
  (showId, templateId) <- insertTestShowWithSchedule showInsert {Shows.siStatus = Shows.Active} template
  _ <-
    unwrapInsert $
      ShowSchedule.insertValidity $
        ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
  pure (showId, templateId)

-- | A scheduled slot with no episode row gives one window with no episode.
prop_slotWindowWithNoEpisode :: TestDBConfig -> PropertyT IO ()
prop_slotWindowWithNoEpisode cfg = do
  arrange (bracketConn cfg) $ do
    showInsert <- forAllT showInsertGen

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        _ <- insertMondaySlot showInsert Nothing
        windows <-
          TRX.statement () $
            UUT.getSlotWindowsBetween (slotTestTime 0 (TimeOfDay 0 0 0)) (slotTestTime 1 (TimeOfDay 0 0 0))
        TRX.condemn
        pure windows

      assert $ do
        windows <- assertRight result
        windows
          === [ UUT.SlotWindow
                  { swStart = slotTestTime 0 (TimeOfDay 14 0 0),
                    swEnd = slotTestTime 0 (TimeOfDay 16 0 0),
                    swShowTitle = Shows.siTitle showInsert,
                    swEpisode = Nothing
                  }
              ]

-- | A template with a replay gives the primary window and the replay window.
-- The replay opens at 22:00 and closes at midnight. Both hold the episode.
prop_slotWindowsPrimaryAndReplay :: TestDBConfig -> PropertyT IO ()
prop_slotWindowsPrimaryAndReplay cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      let audioPath = "audio/episodes/2025/01/06/primary-and-replay.mp3"
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertMondaySlot showInsert (Just (TimeOfDay 22 0 0))
        episodeId <-
          insertTestEpisode
            episodeTemplate
              { UUT.eiId = showId,
                UUT.eiScheduleTemplateId = Just templateId,
                UUT.eiAirDate = Just slotTestDay,
                UUT.eiCreatedBy = userId,
                UUT.eiAudioFilePath = Just audioPath,
                UUT.eiDurationSeconds = Just 7080
              }
        windows <-
          TRX.statement () $
            UUT.getSlotWindowsBetween (slotTestTime 0 (TimeOfDay 0 0 0)) (slotTestTime 1 (TimeOfDay 6 0 0))
        TRX.condemn
        pure (episodeId, windows)

      assert $ do
        (episodeId, windows) <- assertRight result
        let window start end =
              UUT.SlotWindow
                { swStart = start,
                  swEnd = end,
                  swShowTitle = Shows.siTitle showInsert,
                  swEpisode =
                    Just
                      UUT.SlotEpisode
                        { seId = episodeId,
                          seAudioFilePath = audioPath,
                          seDurationSeconds = 7080
                        }
                }
        windows
          === [ window (slotTestTime 0 (TimeOfDay 14 0 0)) (slotTestTime 0 (TimeOfDay 16 0 0)),
                window (slotTestTime 0 (TimeOfDay 22 0 0)) (slotTestTime 1 (TimeOfDay 0 0 0))
              ]

-- | An episode with audio but a NULL, 0, or negative length gives a window with
-- no episode.
prop_slotWindowUnknownLength :: TestDBConfig -> PropertyT IO ()
prop_slotWindowUnknownLength cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)
    badLength <- forAllT $ Gen.element [Nothing, Just 0, Just (-60)]

    act $ do
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertMondaySlot showInsert Nothing
        _ <-
          insertTestEpisode
            episodeTemplate
              { UUT.eiId = showId,
                UUT.eiScheduleTemplateId = Just templateId,
                UUT.eiAirDate = Just slotTestDay,
                UUT.eiCreatedBy = userId,
                UUT.eiAudioFilePath = Just "audio/episodes/2025/01/06/unknown-length.mp3",
                UUT.eiDurationSeconds = badLength
              }
        windows <-
          TRX.statement () $
            UUT.getSlotWindowsBetween (slotTestTime 0 (TimeOfDay 0 0 0)) (slotTestTime 1 (TimeOfDay 0 0 0))
        TRX.condemn
        pure windows

      assert $ do
        windows <- assertRight result
        window <- assertSingleton windows
        UUT.swEpisode window === Nothing

-- | A Sunday 01:00 to 02:00 slot on the fall-back date, November 2, 2025.
--
-- The clock reads 01:00 twice that night, at 08:00 UTC and at 09:00 UTC. The
-- window opens the first time, so it runs from 08:00 to 10:00 UTC, two hours.
-- 'UUT.getAiringEpisodes' airs the episode for the same two hours. The test
-- checks one second on each side of both window edges.
prop_slotWindowOnFallBackDate :: TestDBConfig -> PropertyT IO ()
prop_slotWindowOnFallBackDate cfg = do
  arrange (bracketConn cfg) $ do
    userWithMetadata <- forAllT userWithMetadataInsertGen
    showInsert <- forAllT showInsertGen
    episodeTemplate <- forAllT $ episodeInsertGen (Shows.Id 1) (ShowSchedule.TemplateId 1) (User.Id 1)

    act $ do
      let fallBackDay = fromGregorian 2025 11 2
          utcAt = UTCTime fallBackDay
          windowOpen = utcAt (8 * 3600)
          windowClose = utcAt (10 * 3600)
          probes = [addUTCTime (-1) windowOpen, windowOpen, addUTCTime (-1) windowClose, windowClose]
          template =
            ShowSchedule.ScheduleTemplateInsert
              { stiShowId = Shows.Id 1,
                stiDayOfWeek = Sunday,
                stiWeeksOfMonth = [1, 2, 3, 4, 5],
                stiStartTime = TimeOfDay 1 0 0,
                stiEndTime = TimeOfDay 2 0 0,
                stiTimezone = "America/Los_Angeles",
                stiReplayStartTime = Nothing
              }
      result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userWithMetadata
        (showId, templateId) <- insertTestShowWithSchedule showInsert {Shows.siStatus = Shows.Active} template
        _ <-
          unwrapInsert $
            ShowSchedule.insertValidity $
              ShowSchedule.ValidityInsert templateId (fromGregorian 2025 10 1) Nothing
        -- Two hours of audio, so the episode fills the long slot.
        episodeId <-
          insertTestEpisode
            episodeTemplate
              { UUT.eiId = showId,
                UUT.eiScheduleTemplateId = Just templateId,
                UUT.eiAirDate = Just fallBackDay,
                UUT.eiCreatedBy = userId,
                UUT.eiAudioFilePath = Just "audio/episodes/2025/11/02/fall-back.mp3",
                UUT.eiDurationSeconds = Just 7200
              }
        windows <- TRX.statement () (UUT.getSlotWindowsBetween (utcAt 0) (utcAt (12 * 3600)))
        airing <- forM probes $ \probe -> do
          rows <- TRX.statement () (UUT.getAiringEpisodes probe)
          pure (map (UUT.id . UUT.aeEpisode) rows)
        TRX.condemn
        pure (episodeId, windows, airing)

      assert $ do
        (episodeId, windows, airing) <- assertRight result
        window <- assertSingleton windows
        (UUT.swStart window, UUT.swEnd window) === (windowOpen, windowClose)
        fmap UUT.seId (UUT.swEpisode window) === Just episodeId
        airing === [[], [episodeId], [episodeId], []]

{-# LANGUAGE PackageImports #-}

module API.Dashboard.AirBreaks.Timeline.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Timeline.Get.Handler (action)
import API.Dashboard.AirBreaks.Timeline.Get.Templates.Page (DayKind (..), TimelineView (..), dayNav, template)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text.Lazy qualified as Lazy
import Data.Time (LocalTime (..), TimeOfDay (..), UTCTime, fromGregorian)
import Data.Time.Calendar (DayOfWeek (..))
import Design.Tokens qualified as Tokens
import Domain.DayTimeline (Flag (..), Problem (..), SegmentView (..))
import Domain.Timeline (Content (..), EphemeralCandidate (..), EpisodeCandidate (..), Item (..), Segment (..))
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Lucid qualified
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Expectation, Spec, describe, expectationFailure, it, shouldBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.AirBreaks.Timeline.Get.Handler.action" $ do
      it "forecasts a future day without storing it" futureDayStoresNothing
      it "builds today's plan when viewed" todayBuildsPlan
      it "shows only the scheduled segments" showsOnlyScheduled
      it "shows a rerun in a slot with no episode and keeps the flag" showsRerun
      it "shows no rerun for a show with no earlier episode" showsMissWithoutRerun
      it "gives the fill after a rerun no error tint" . const $ noTintAfterRerun
      it "gives the fill of a slot with no rerun the error tint" . const $ tintWithoutRerun

--------------------------------------------------------------------------------

-- | Noon Pacific on Monday, January 6, 2025.
noon :: UTCTime
noon = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay 12 0 0))

futureDayStoresNothing :: TestDBConfig -> IO ()
futureDayStoresNothing cfg = bracketAppM cfg $ do
  let tomorrow = fromGregorian 2025 1 7
  view <- runExceptT (action noon tomorrow)
  -- True means this call created the day row, so the view did not.
  created <- execQuery (BreakPlans.insertPlanDay tomorrow)
  liftIO $ do
    case view of
      Right v -> tvKind v `shouldBe` FutureDay
      Left err -> expectationFailure ("Expected a view, got " <> show err)
    created `shouldBe` Right True

todayBuildsPlan :: TestDBConfig -> IO ()
todayBuildsPlan cfg = bracketAppM cfg $ do
  let today = fromGregorian 2025 1 6
  _ <- runExceptT (action noon today)
  created <- execQuery (BreakPlans.insertPlanDay today)
  liftIO $ created `shouldBe` Right False

-- | A show on Mondays from 11:00 to 13:00.
seedNoonSlot :: AppM ()
seedNoonSlot = void seedNoonSlotIds

-- | The show of 'seedNoonSlot', with its ids.
seedNoonSlotIds :: AppM (Shows.Id, ShowSchedule.TemplateId)
seedNoonSlotIds = do
  let showInsert = Shows.Insert "Noon Show" (Slug "noon-show") Nothing Nothing Shows.Active
      scheduleTemplate =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 11 0 0,
            stiEndTime = TimeOfDay 13 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (showId, templateId) <- insertTestShowWithSchedule showInsert scheduleTemplate
    _ <-
      unwrapInsert $
        ShowSchedule.insertValidity $
          ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
    pure (showId, templateId)
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right ids -> pure ids

-- | An episode of the noon show that aired on Monday, December 30, 2024.
seedEarlierEpisode :: (Shows.Id, ShowSchedule.TemplateId) -> AppM ()
seedEarlierEpisode (showId, templateId) = do
  userInsert <- liftIO $ mkUserInsert "noon-host" UserMetadata.Host
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    insertTestEpisode
      Episodes.Insert
        { Episodes.eiId = showId,
          Episodes.eiDescription = Nothing,
          Episodes.eiAudioFilePath = Just "audio/episodes/2024/12/30/noon-show_2024-12-30_abc.mp3",
          Episodes.eiAudioFileSize = Just 1024,
          Episodes.eiAudioMimeType = Just "audio/mpeg",
          Episodes.eiDurationSeconds = Just 7000,
          Episodes.eiArtworkUrl = Nothing,
          Episodes.eiScheduleTemplateId = Just templateId,
          Episodes.eiAirDate = Just (fromGregorian 2024 12 30),
          Episodes.eiCreatedBy = userId
        }
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right _ -> pure ()

-- | The page of January 6, 2025, as text.
renderedDay :: AppM Lazy.Text
renderedDay =
  runExceptT (action noon (fromGregorian 2025 1 6)) >>= \case
    Left err -> error ("Expected a view, got " <> show err)
    Right v -> pure (Lucid.renderText (template v))

-- | The noon slot has no episode for the day and one earlier episode. The
-- page shows the rerun, and still flags the slot.
showsRerun :: TestDBConfig -> IO ()
showsRerun cfg = bracketAppM cfg $ do
  seedNoonSlotIds >>= seedEarlierEpisode
  page <- renderedDay
  liftIO $ do
    Lazy.isInfixOf "has no playable episode: a rerun of episode 1 airs" page `shouldBe` True
    -- The flag stays, with a text of its own for a slot that a rerun covers.
    Lazy.isInfixOf "NO NEW EPISODE · RERUN" page `shouldBe` True
    Lazy.isInfixOf "NO PLAYABLE EPISODE" page `shouldBe` False
    Lazy.isInfixOf ">rerun</span>" page `shouldBe` True
    Lazy.isInfixOf "Noon Show - Episode 1" page `shouldBe` True

showsMissWithoutRerun :: TestDBConfig -> IO ()
showsMissWithoutRerun cfg = bracketAppM cfg $ do
  seedNoonSlot
  page <- renderedDay
  liftIO $ do
    Lazy.isInfixOf "has no playable episode" page `shouldBe` True
    Lazy.isInfixOf "NO PLAYABLE EPISODE" page `shouldBe` True
    Lazy.isInfixOf "rerun" page `shouldBe` False

--------------------------------------------------------------------------------
-- The error tint of fill rows

-- | The 11:00 to 13:00 slot of the tint tests, with no episode.
tintSlot :: Episodes.SlotWindow
tintSlot =
  Episodes.SlotWindow
    { Episodes.swStart = hourAt 11,
      Episodes.swEnd = hourAt 13,
      Episodes.swShowId = Shows.Id 1,
      Episodes.swTemplateId = ShowSchedule.TemplateId 1,
      Episodes.swShowTitle = "Noon Show",
      Episodes.swShowSlug = Slug "noon-show",
      Episodes.swAirDate = fromGregorian 2025 1 6,
      Episodes.swEpisode = Nothing
    }

-- | A whole hour on January 6, 2025, in Pacific time.
hourAt :: Int -> UTCTime
hourAt hour = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay hour 0 0))

-- | An earlier episode of half an hour.
tintRerun :: EpisodeCandidate
tintRerun =
  EpisodeCandidate
    { ecId = Episodes.Id 4,
      ecShowTitle = "Noon Show",
      ecShowSlug = Slug "noon-show",
      ecEpisodeNumber = Episodes.EpisodeNumber 4,
      ecAudioKey = "audio/episodes/earlier.mp3",
      ecSeconds = 1800
    }

-- | The page of a future day with one segment for 'tintSlot'. The segment
-- holds the given first item, then ephemeral fill, and it has the given flag.
tintPage :: Content -> Flag -> Lazy.Text
tintPage firstContent flag =
  Lucid.renderText . template $
    TimelineView
      { tvDay = fromGregorian 2025 1 6,
        tvKind = FutureDay,
        tvNow = hourAt 0,
        tvSegments = [SegmentView segment (Just tintSlot) [flag]],
        tvProblems = [Problem (hourAt 11) (Just tintSlot) flag]
      }
  where
    segment = Segment (hourAt 11) (hourAt 13) [item firstContent (hourAt 11), item fill (hourAt 12)]
    fill = EphemeralContent (EphemeralCandidate (EphemeralUploads.Id 1) "Night Sounds" "audio/ephemeral/night.mp3" 3600)
    item content start =
      Item
        { itemContent = content,
          itemStart = start,
          itemPlannedStart = start,
          itemSeconds = 3600,
          itemCueIn = 0,
          itemCueOut = Nothing
        }

-- | The class of the error tint, as it is in the page.
errorTint :: Lazy.Text
errorTint = Lazy.fromStrict Tokens.errorBg

-- | The fill after a rerun is normal fill, so no row has the error tint.
noTintAfterRerun :: Expectation
noTintAfterRerun =
  Lazy.isInfixOf errorTint (tintPage (RerunContent tintRerun) (NoPlayableEpisode tintSlot (Just tintRerun)))
    `shouldBe` False

-- | A slot that airs only fill keeps the error tint on its fill rows.
tintWithoutRerun :: Expectation
tintWithoutRerun = do
  let fill = EphemeralContent (EphemeralCandidate (EphemeralUploads.Id 2) "Field Notes" "audio/ephemeral/field.mp3" 3600)
  Lazy.isInfixOf errorTint (tintPage fill (NoPlayableEpisode tintSlot Nothing)) `shouldBe` True

-- | The unscheduled hours are left out. The slot has no break inside it, so
-- it is one segment.
showsOnlyScheduled :: TestDBConfig -> IO ()
showsOnlyScheduled cfg = bracketAppM cfg $ do
  seedNoonSlot
  view <- runExceptT (action noon (fromGregorian 2025 1 6))
  liftIO $ case view of
    Left err -> expectationFailure ("Expected a view, got " <> show err)
    Right v -> do
      let at hour = pacificToUtc (LocalTime (fromGregorian 2025 1 6) (TimeOfDay hour 0 0))
      map (segStart . svSegment) (tvSegments v) `shouldBe` [at 11]
      -- An item airs at noon, so the page marks it.
      Lazy.isInfixOf "id=\"now\"" (Lucid.renderText (template v)) `shouldBe` True
      -- The NOW link scrolls to it and adds no entry to the browser history.
      let nav = Lucid.renderText (dayNav v)
      Lazy.isInfixOf "scrollIntoView" nav `shouldBe` True
      Lazy.isInfixOf "return false" nav `shouldBe` True

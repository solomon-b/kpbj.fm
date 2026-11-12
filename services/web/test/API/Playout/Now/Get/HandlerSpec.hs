{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for the now endpoint.
--
-- Liquidsoap plays the items of the segment that this endpoint returns. A wrong
-- cue point airs the wrong part of a show, and a wrong start runs the schedule
-- late.
--
-- The schedule has one show on Monday from 14:00 to 15:00, with one episode.
-- The break items are one station ID and one PSA.
module API.Playout.Now.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Now.Get.Handler (actionAt, segmentId, toItemResponse)
import API.Playout.Types (ItemResponse (..), NowResponse (..), SegmentResponse (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Either (isLeft)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time
  ( Day,
    DayOfWeek (..),
    LocalTime (..),
    TimeOfDay (..),
    UTCTime,
    addDays,
    addUTCTime,
    diffUTCTime,
    fromGregorian,
  )
import Domain.Timeline
  ( BreakTrack (..),
    Content (..),
    EphemeralCandidate (..),
    EpisodeCandidate (..),
    Item (..),
    Segment (..),
    segmentContaining,
  )
import Domain.Types.Slug (Slug (..))
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.BreakPlans qualified as BreakPlans
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Timeline (timelineAround)
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestStationId, insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Playout.Now.Get.Handler.actionAt" $ do
      it "returns the current segment resumed at now" resumedAtNow
      it "returns the segment that starts at from" segmentAtFrom
      it "returns the whole next segment after the current segment" nextAfterCurrent
      it "returns the whole next segment after the segment at from" nextAfterFrom
      it "keeps the planned start of an item resumed after a restart" plannedStartKept
      it "gives a break item its id and planned start" breakItemFields
      it "returns an error when from is not a segment start" fromNotSegmentStart
      it "rejects a from later than the look-around window and stores no plan" farFutureFromRejected
      it "rejects a from earlier than now and stores no plan" pastFromRejected
      it "encodes each content kind" . const $ encodesEachContentKind
      it "encodes the next segment beside the fields of the first" . const $ encodesNext
      it "gives the next segment the id of the segment at from" nextIdMatchesFrom
      it "keeps the id of a segment while it airs" idStableWhileAiring
      it "changes the id only when what airs changes" . const $ idTracksAiringFields

--------------------------------------------------------------------------------
-- Fixtures

-- | A test date: Monday, January 6, 2025.
testDay :: Day
testDay = fromGregorian 2025 1 6

pacificAt :: TimeOfDay -> UTCTime
pacificAt = pacificToUtc . LocalTime testDay

-- | Insert one station ID, one PSA, and the episode slot. Returns the PSA id.
seedAll :: AppM BreakItems.Id
seedAll = do
  userInsert <- liftIO $ mkUserInsert "now-host" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    _ <-
      insertTestStationId
        StationIds.Insert
          { siiTitle = "Station ID",
            siiAudioFilePath = "audio/station-ids/2025/01/06/id_2025-01-06_abc.mp3",
            siiMimeType = "audio/mpeg",
            siiFileSize = 512,
            siiDurationSeconds = 10,
            siiCreatorId = userId
          }
    psaId <-
      unwrapInsert $
        BreakItems.insertBreakItem
          BreakItems.Insert
            { biiTitle = "Library Hours",
              biiCategory = BreakItems.Psa,
              biiUnderwriterId = Nothing,
              biiShowId = Nothing,
              biiReviewStatus = BreakItems.Approved,
              biiAudioFilePath = "audio/break-items/2025/01/01/library_2025-01-01_def.mp3",
              biiMimeType = "audio/mpeg",
              biiFileSize = 1024,
              biiDurationSeconds = 30,
              biiStartsOn = fromGregorian 2025 1 1,
              biiEndsOn = Nothing,
              biiCreatorId = userId
            }
    (showId, templateId) <-
      insertTestShowWithSchedule
        (Shows.Insert "Now Show" (Slug "now-show") Nothing Nothing Shows.Active)
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 15 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
    _ <-
      unwrapInsert $
        ShowSchedule.insertValidity $
          ShowSchedule.ValidityInsert templateId (fromGregorian 2024 12 1) Nothing
    _ <-
      insertTestEpisode
        Episodes.Insert
          { Episodes.eiId = showId,
            Episodes.eiDescription = Nothing,
            Episodes.eiAudioFilePath = Just "audio/episodes/2025/01/06/now_2025-01-06_abc.mp3",
            Episodes.eiAudioFileSize = Just 1024,
            Episodes.eiAudioMimeType = Just "audio/mpeg",
            Episodes.eiDurationSeconds = Just 3500,
            Episodes.eiArtworkUrl = Nothing,
            Episodes.eiScheduleTemplateId = Just templateId,
            Episodes.eiAirDate = Just testDay,
            Episodes.eiCreatedBy = userId
          }
    pure psaId
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right psaId -> pure psaId

-- | The timeline segment that holds an instant, from the same query that the
-- handler uses.
segmentHolding :: UTCTime -> AppM Segment
segmentHolding instant =
  timelineAround instant instant >>= \case
    Left err -> error ("Timeline failed: " <> show err)
    Right segments -> case segmentContaining instant segments of
      Nothing -> error "No segment holds the instant"
      Just segment -> pure segment

-- | Run 'actionAt' and fail the test on a 'Left'.
nowResponseOf :: UTCTime -> Maybe UTCTime -> AppM NowResponse
nowResponseOf now mFrom =
  actionAt now mFrom >>= \case
    Left reason -> error ("actionAt failed: " <> show reason)
    Right response -> pure response

-- | The first segment of 'nowResponseOf'.
responseOf :: UTCTime -> Maybe UTCTime -> AppM SegmentResponse
responseOf now mFrom = (.nrSegment) <$> nowResponseOf now mFrom

-- | The next segment of a response must be the whole segment that starts at
-- the end of the first one.
shouldBeNextOf :: UTCTime -> Maybe SegmentResponse -> Segment -> Expectation
shouldBeNextOf now mNext later =
  case mNext of
    Nothing -> expectationFailure "The response has no next segment"
    Just next -> do
      next.srSegmentStart `shouldBe` segStart later
      next.srSegmentEnd `shouldBe` segEnd later
      next.srEndsInSeconds `shouldBe` realToFrac (diffUTCTime (segEnd later) now)
      map (.irPlannedStart) next.srItems `shouldBe` map itemPlannedStart (segItems later)
      map (.irCueIn) next.srItems `shouldBe` map itemCueIn (segItems later)

--------------------------------------------------------------------------------
-- Cases

resumedAtNow :: TestDBConfig -> IO ()
resumedAtNow cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    -- A fraction of a second tests that the first item starts on a whole second.
    let wholeNow = pacificAt (TimeOfDay 14 40 0)
        now = addUTCTime 0.5 wholeNow
    segment <- segmentHolding wholeNow
    response <- responseOf now Nothing
    let elapsed = floor (diffUTCTime wholeNow (segStart segment))
    liftIO $ do
      response.srSegmentStart `shouldBe` segStart segment
      response.srSegmentEnd `shouldBe` segEnd segment
      response.srEndsInSeconds `shouldBe` realToFrac (diffUTCTime (segEnd segment) now)
      case response.srItems of
        [] -> expectationFailure "The response has no items"
        first : _ -> do
          first.irType `shouldBe` "episode"
          first.irTitle `shouldBe` "Now Show"
          first.irCueIn `shouldBe` elapsed
          first.irPlannedStart `shouldBe` segStart segment

segmentAtFrom :: TestDBConfig -> IO ()
segmentAtFrom cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 40 0)
    current <- segmentHolding now
    let from = segEnd current
    response <- responseOf now (Just from)
    liftIO $ do
      response.srSegmentStart `shouldBe` from
      case response.srItems of
        [] -> expectationFailure "The response has no items"
        first : _ -> do
          first.irCueIn `shouldBe` 0
          first.irPlannedStart `shouldBe` from

nextAfterCurrent :: TestDBConfig -> IO ()
nextAfterCurrent cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 40 0)
    current <- segmentHolding now
    later <- segmentHolding (segEnd current)
    response <- nowResponseOf now Nothing
    liftIO $ shouldBeNextOf now response.nrNext later

nextAfterFrom :: TestDBConfig -> IO ()
nextAfterFrom cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    -- The segment at 14:00 is the episode slot. The segment after it starts
    -- at 15:00 and is unscheduled time.
    let now = pacificAt (TimeOfDay 13 59 0)
        from = pacificAt (TimeOfDay 14 0 0)
    later <- segmentHolding (pacificAt (TimeOfDay 15 0 0))
    response <- nowResponseOf now (Just from)
    liftIO $ do
      segStart later `shouldBe` pacificAt (TimeOfDay 15 0 0)
      shouldBeNextOf now response.nrNext later

breakItemFields :: TestDBConfig -> IO ()
breakItemFields cfg =
  bracketAppM cfg $ do
    psaId <- seedAll
    -- The break at 15:00 starts at 14:58:00. The 10 second station ID plays
    -- first, so the PSA starts at 14:58:10.
    let now = pacificAt (TimeOfDay 13 59 0)
        from = pacificAt (TimeOfDay 14 0 0)
        expected = [(Just (BreakItems.unId psaId), pacificAt (TimeOfDay 14 58 10))]
    response <- responseOf now (Just from)
    let actual = [(item.irBreakItemId, item.irPlannedStart) | item <- response.srItems, isJust item.irBreakItemId]
    liftIO $ actual `shouldBe` expected

plannedStartKept :: TestDBConfig -> IO ()
plannedStartKept cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    current <- segmentHolding (pacificAt (TimeOfDay 14 40 0))
    -- Liquidsoap restarts 5 seconds into the first break item.
    case [item | item@Item {itemContent = BreakContent _} <- segItems current, itemSeconds item > 5] of
      [] -> liftIO $ expectationFailure "The segment has no break item"
      breakItem : _ -> do
        let originalStart = itemStart breakItem
            now = addUTCTime 5 originalStart
        response <- responseOf now Nothing
        -- The first item is the break item, because now falls inside it.
        liftIO $ case response.srItems of
          [] -> expectationFailure "The response has no items"
          first : _ -> do
            first.irCueIn `shouldBe` itemCueIn breakItem + 5
            first.irPlannedStart `shouldBe` originalStart

fromNotSegmentStart :: TestDBConfig -> IO ()
fromNotSegmentStart cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 40 0)
    result <- actionAt now (Just (addUTCTime 30 now))
    liftIO $ result `shouldSatisfy` isLeft

farFutureFromRejected :: TestDBConfig -> IO ()
farFutureFromRejected cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    -- 04:00 tomorrow is a segment start, 13 hours and 20 minutes after now.
    let now = pacificAt (TimeOfDay 14 40 0)
        from = pacificToUtc (LocalTime (addDays 1 testDay) (TimeOfDay 4 0 0))
    result <- actionAt now (Just from)
    -- True means this call created the day row, so actionAt did not.
    todayCreated <- execQuery (BreakPlans.insertPlanDay testDay)
    tomorrowCreated <- execQuery (BreakPlans.insertPlanDay (addDays 1 testDay))
    liftIO $ do
      result `shouldSatisfy` isLeft
      todayCreated `shouldBe` Right True
      tomorrowCreated `shouldBe` Right True

pastFromRejected :: TestDBConfig -> IO ()
pastFromRejected cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    -- 14:00 is a segment start, 40 minutes before now.
    let now = pacificAt (TimeOfDay 14 40 0)
        from = pacificAt (TimeOfDay 14 0 0)
    result <- actionAt now (Just from)
    -- True means this call created the day row, so actionAt did not.
    todayCreated <- execQuery (BreakPlans.insertPlanDay testDay)
    yesterdayCreated <- execQuery (BreakPlans.insertPlanDay (addDays (-1) testDay))
    liftIO $ do
      result `shouldSatisfy` isLeft
      todayCreated `shouldBe` Right True
      yesterdayCreated `shouldBe` Right True

-- | The JSON of a response with a next segment, and of one without.
--
-- Liquidsoap reads the first segment from @current@, and the next segment
-- from @next@.
encodesNext :: Expectation
encodesNext = do
  let segment start end =
        SegmentResponse
          { srId = "id",
            srSegmentStart = start,
            srSegmentEnd = end,
            srEndsInSeconds = 60,
            srItems = []
          }
      first = segment (pacificAt (TimeOfDay 14 0 0)) (pacificAt (TimeOfDay 15 0 0))
      second = segment (pacificAt (TimeOfDay 15 0 0)) (pacificAt (TimeOfDay 16 0 0))
      fields start end =
        [ "id" .= ("id" :: Text),
          "segment_start" .= (start :: Text),
          "segment_end" .= (end :: Text),
          "ends_in_seconds" .= (60 :: Double),
          "items" .= ([] :: [Value])
        ]
  toJSON (NowResponse first (Just second))
    `shouldBe` object
      [ "current" .= object (fields "2025-01-06T22:00:00Z" "2025-01-06T23:00:00Z"),
        "next" .= object (fields "2025-01-06T23:00:00Z" "2025-01-07T00:00:00Z")
      ]
  toJSON (NowResponse first Nothing)
    `shouldBe` object
      [ "current" .= object (fields "2025-01-06T22:00:00Z" "2025-01-06T23:00:00Z"),
        "next" .= Null
      ]

-- | The JSON of one item of each content kind.
--
-- Liquidsoap parses these fields by name. The media URL function marks the
-- key, so the test shows which key each URL comes from.
encodesEachContentKind :: Expectation
encodesEachContentKind = do
  let mediaUrl key = "https://media.test/" <> key
      start = pacificAt (TimeOfDay 14 0 0)
      encode placed = toJSON (toItemResponse mediaUrl placed)
      item content seconds cueOut =
        Item
          { itemContent = content,
            itemStart = start,
            itemPlannedStart = start,
            itemSeconds = seconds,
            itemCueIn = 0,
            itemCueOut = cueOut
          }
      episode =
        EpisodeContent
          EpisodeCandidate
            { ecId = Episodes.Id 7,
              ecShowTitle = "Now Show",
              ecAudioKey = "audio/episodes/now.mp3",
              ecSeconds = 3500
            }
      ephemeral =
        EphemeralContent
          EphemeralCandidate
            { ecdId = EphemeralUploads.Id 8,
              ecdTitle = "Night Sounds",
              ecdAudioKey = "audio/ephemeral/night.mp3",
              ecdSeconds = 300
            }
      psa =
        BreakContent
          BreakTrack
            { btSourceType = "psa",
              btTitle = "Library Hours",
              btAudioKey = "audio/break-items/library.mp3",
              btSeconds = 30,
              btBreakItemId = Just 42
            }
  encode (item episode 3480 (Just 3480))
    `shouldBe` expectedJson "episode" "https://media.test/audio/episodes/now.mp3" "Now Show" 3480 (Number 3480) Null
  encode (item ephemeral 300 Nothing)
    `shouldBe` expectedJson "ephemeral" "https://media.test/audio/ephemeral/night.mp3" "Night Sounds" 300 Null Null
  encode (item psa 30 Nothing)
    `shouldBe` expectedJson "psa" "https://media.test/audio/break-items/library.mp3" "Library Hours" 30 Null (Number 42)
  encode (item FillerContent 80 (Just 80))
    `shouldBe` expectedJson "filler" "https://media.test/audio/filler/kpbj-filler.mp3" "KPBJ 95.9 FM" 80 (Number 80) Null
  where
    -- 14:00 Pacific in January is 22:00 UTC.
    expectedJson :: Text -> Text -> Text -> Int -> Value -> Value -> Value
    expectedJson itemType url title seconds cueOut breakItemId =
      object
        [ "type" .= itemType,
          "url" .= url,
          "title" .= title,
          "artist" .= ("KPBJ 95.9 FM" :: Text),
          "duration_seconds" .= seconds,
          "cue_in" .= (0 :: Int),
          "cue_out" .= cueOut,
          "break_item_id" .= breakItemId,
          "planned_start" .= ("2025-01-06T22:00:00Z" :: Text)
        ]

nextIdMatchesFrom :: TestDBConfig -> IO ()
nextIdMatchesFrom cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 40 0)
    current <- segmentHolding now
    response <- nowResponseOf now Nothing
    atFrom <- responseOf now (Just (segEnd current))
    liftIO $ fmap (.srId) response.nrNext `shouldBe` Just atFrom.srId

idStableWhileAiring :: TestDBConfig -> IO ()
idStableWhileAiring cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    -- Both instants fall in the episode, so each response resumes it at a
    -- different cue in.
    early <- nowResponseOf (pacificAt (TimeOfDay 14 40 0)) Nothing
    late <- nowResponseOf (pacificAt (TimeOfDay 14 41 0)) Nothing
    liftIO $ do
      late.nrSegment.srId `shouldBe` early.nrSegment.srId
      fmap (.srId) early.nrNext `shouldNotBe` Just early.nrSegment.srId

-- | The id covers the segment end and the url, cue points, break item id, and
-- planned start of each item. The title does not count.
idTracksAiringFields :: Expectation
idTracksAiringFields = do
  let start = pacificAt (TimeOfDay 14 0 0)
      end = pacificAt (TimeOfDay 15 0 0)
      item =
        ItemResponse
          { irType = "psa",
            irUrl = "https://media.test/audio/break-items/library.mp3",
            irTitle = "Library Hours",
            irArtist = "KPBJ 95.9 FM",
            irDurationSeconds = 30,
            irCueIn = 0,
            irCueOut = Nothing,
            irBreakItemId = Just 42,
            irPlannedStart = start
          }
      idWith changed = segmentId start end [changed]
      base = idWith item
  idWith item {irTitle = "Library Hours (new)"} `shouldBe` base
  idWith item {irUrl = "https://media.test/audio/break-items/other.mp3"} `shouldNotBe` base
  idWith item {irCueIn = 5} `shouldNotBe` base
  idWith item {irCueOut = Just 20} `shouldNotBe` base
  idWith item {irBreakItemId = Just 43} `shouldNotBe` base
  idWith item {irPlannedStart = addUTCTime 10 start} `shouldNotBe` base
  segmentId start (addUTCTime 1800 end) [item] `shouldNotBe` base

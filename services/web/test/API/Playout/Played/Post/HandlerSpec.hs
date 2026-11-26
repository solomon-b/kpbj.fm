{-# LANGUAGE PackageImports #-}

-- | Tests for the played endpoint.
--
-- Delivery counts come from @playback_history.break_item_id@, so the endpoint
-- must store the id Liquidsoap sends, and store nothing for an empty string.
module API.Playout.Played.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Played.Post.Handler (record)
import API.Playout.Types (PlayedRequest (..))
import App.BaseUrl (baseUrl)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Aeson (eitherDecode)
import Data.Int (Int64)
import Data.Text qualified as Text
import Data.Time (DayOfWeek (..), TimeOfDay (..), UTCTime (..), fromGregorian)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
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
    describe "API.Playout.Played.Post.Handler.record" $ do
      it "stores the break item id" storesBreakItemId
      it "stores no break item id for an empty string" storesNothingForEmpty
      it "stores a play for each break source type" storesEachBreakSourceType
      it "records a break item once for one planned start" recordsOncePerPlannedStart
      it "records a resumed episode once for one planned start" recordsEpisodeOncePerPlannedStart
      it "records the same break item at two planned starts" recordsTwoPlannedStarts
      it "records a post with no planned start" recordsWithoutPlannedStart
      it "stores the episode id for a rerun" storesRerunEpisodeId
      it "stores a rerun with no episode id when its url matches no episode" storesRerunWithoutEpisode
      it "decodes the body that radio.liq sends for a break item" . const $ decodesBreakItemBody
      it "decodes a body with no planned start" . const $ decodesBodyWithoutPlannedStart

--------------------------------------------------------------------------------

-- | Insert a staff user and one PSA. Returns the PSA's id.
seedPsa :: AppM BreakItems.Id
seedPsa = do
  userInsert <- liftIO $ mkUserInsert "played" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
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
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right psaId -> pure psaId

-- | A PSA track that started with this break item id annotation.
psaPlayed :: Text.Text -> PlayedRequest
psaPlayed breakItemId =
  PlayedRequest
    { prTitle = "Library Hours",
      prArtist = Nothing,
      prSourceType = "psa",
      prSourceUrl = "http://localhost:4000/media/audio/break-items/library.mp3",
      prStartedAt = UTCTime (fromGregorian 2025 1 6) 3600,
      prBreakItemId = Just breakItemId,
      prPlannedStart = Nothing
    }

-- | Record the request, then read back the newest row's break item id.
recordAndRead :: PlayedRequest -> AppM (Maybe Int64)
recordAndRead request = do
  recorded <- runExceptT (record request)
  case recorded of
    Left err -> error ("Record failed: " <> show err)
    Right () -> pure ()
  execQuery (PlaybackHistory.getRecentPlayback 1) >>= \case
    Right [row] -> pure row.phBreakItemId
    other -> error ("Expected one row, got " <> show other)

storesBreakItemId :: TestDBConfig -> IO ()
storesBreakItemId cfg = bracketAppM cfg $ do
  psaId <- seedPsa
  stored <- recordAndRead (psaPlayed (Text.pack (show (BreakItems.unId psaId))))
  liftIO $ stored `shouldBe` Just (BreakItems.unId psaId)

storesNothingForEmpty :: TestDBConfig -> IO ()
storesNothingForEmpty cfg = bracketAppM cfg $ do
  _ <- seedPsa
  stored <- recordAndRead (psaPlayed "")
  liftIO $ case stored of
    Nothing -> pure ()
    Just n -> expectationFailure ("Expected no break item id, got " <> show n)

-- | The @source_type@ that Liquidsoap sends for a break item of this category.
--
-- The case has no wildcard, so a new category gives a warning here until it
-- gets a source type and a row in @playback_history_source_type_check@.
categorySourceType :: BreakItems.Category -> Text.Text
categorySourceType = \case
  BreakItems.Psa -> "psa"
  BreakItems.Underwriting -> "underwriting"
  BreakItems.Announcement -> "announcement"
  BreakItems.ShowPromo -> "show_promo"

-- | Every source type that can air in a break. The check constraint on
-- @playback_history.source_type@ must accept each one.
breakSourceTypes :: [Text.Text]
breakSourceTypes =
  ["station_id", "ephemeral"] <> map categorySourceType [minBound .. maxBound]

storesEachBreakSourceType :: TestDBConfig -> IO ()
storesEachBreakSourceType cfg =
  bracketAppM cfg $
    mapM_ recordOne (zip [1 ..] breakSourceTypes)
  where
    recordOne :: (Integer, Text.Text) -> AppM ()
    recordOne (n, sourceType) = do
      let request =
            (psaPlayed "")
              { prSourceType = sourceType,
                prStartedAt = UTCTime (fromGregorian 2025 1 6) (fromInteger (3600 + n))
              }
      recorded <- runExceptT (record request)
      case recorded of
        Left err -> error ("Record failed for " <> show sourceType <> ": " <> show err)
        Right () -> pure ()
      execQuery (PlaybackHistory.getRecentPlayback 1) >>= \case
        Right [row] -> liftIO $ row.phSourceType `shouldBe` sourceType
        other -> error ("Expected one row, got " <> show other)

--------------------------------------------------------------------------------
-- Planned start

-- | Record the request and fail the test if the handler fails.
recordOrFail :: PlayedRequest -> AppM ()
recordOrFail request = do
  recorded <- runExceptT (record request)
  case recorded of
    Left err -> error ("Record failed: " <> show err)
    Right () -> pure ()

-- | The planned start of an airing, in seconds after midnight.
plannedAt :: Integer -> Maybe UTCTime
plannedAt seconds = Just (UTCTime (fromGregorian 2025 1 6) (fromInteger seconds))

-- | Every airing in 2025, counted from the delivery query.
airingsIn2025 :: AppM Int
airingsIn2025 = do
  let from = UTCTime (fromGregorian 2025 1 1) 0
      to = UTCTime (fromGregorian 2026 1 1) 0
  execQuery (PlaybackHistory.airingsBetween from to) >>= \case
    Right airings -> pure (length airings)
    Left err -> error ("Count failed: " <> show err)

-- | Every row in the table.
allRows :: AppM Int
allRows =
  execQuery (PlaybackHistory.getRecentPlayback 100) >>= \case
    Right rows -> pure (length rows)
    Left err -> error ("Read failed: " <> show err)

recordsOncePerPlannedStart :: TestDBConfig -> IO ()
recordsOncePerPlannedStart cfg = bracketAppM cfg $ do
  psaId <- seedPsa
  let itemId = Text.pack (show (BreakItems.unId psaId))
      first = (psaPlayed itemId) {prPlannedStart = plannedAt 3600}
      -- Liquidsoap restarted and posted the same airing at a later time.
      resumed = first {prStartedAt = UTCTime (fromGregorian 2025 1 6) 3700}
  recordOrFail first
  recordOrFail resumed
  rows <- allRows
  counted <- airingsIn2025
  liftIO $ rows `shouldBe` 1
  liftIO $ counted `shouldBe` 1

recordsEpisodeOncePerPlannedStart :: TestDBConfig -> IO ()
recordsEpisodeOncePerPlannedStart cfg = bracketAppM cfg $ do
  let first =
        (psaPlayed "")
          { prTitle = "Now Show",
            prSourceType = "episode",
            prSourceUrl = "http://localhost:4000/media/audio/episodes/now.mp3",
            prPlannedStart = plannedAt 3600
          }
      -- Liquidsoap restarted 20 minutes into the episode.
      resumed = first {prStartedAt = UTCTime (fromGregorian 2025 1 6) 4800}
  recordOrFail first
  recordOrFail resumed
  rows <- allRows
  liftIO $ rows `shouldBe` 1

recordsTwoPlannedStarts :: TestDBConfig -> IO ()
recordsTwoPlannedStarts cfg = bracketAppM cfg $ do
  psaId <- seedPsa
  let itemId = Text.pack (show (BreakItems.unId psaId))
  recordOrFail (psaPlayed itemId) {prPlannedStart = plannedAt 3600}
  recordOrFail (psaPlayed itemId) {prPlannedStart = plannedAt 7200}
  rows <- allRows
  liftIO $ rows `shouldBe` 2

recordsWithoutPlannedStart :: TestDBConfig -> IO ()
recordsWithoutPlannedStart cfg = bracketAppM cfg $ do
  psaId <- seedPsa
  let itemId = Text.pack (show (BreakItems.unId psaId))
  recordOrFail (psaPlayed itemId) {prPlannedStart = Nothing}
  recordOrFail (psaPlayed itemId) {prPlannedStart = Nothing}
  rows <- allRows
  liftIO $ rows `shouldBe` 2

--------------------------------------------------------------------------------
-- Reruns

-- | Insert a show with a Monday slot and one episode with audio. Returns the
-- id of the episode.
seedEarlierEpisode :: AppM Episodes.Id
seedEarlierEpisode = do
  userInsert <- liftIO $ mkUserInsert "rerun-host" UserMetadata.Host
  let showInsert = Shows.Insert "Rerun Show" (Slug "rerun-show") Nothing Nothing Shows.Active
      template =
        ShowSchedule.ScheduleTemplateInsert
          { stiShowId = Shows.Id 0,
            stiDayOfWeek = Monday,
            stiWeeksOfMonth = [1, 2, 3, 4, 5],
            stiStartTime = TimeOfDay 14 0 0,
            stiEndTime = TimeOfDay 15 0 0,
            stiTimezone = "America/Los_Angeles",
            stiReplayStartTime = Nothing
          }
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    (showId, templateId) <- insertTestShowWithSchedule showInsert template
    insertTestEpisode
      Episodes.Insert
        { Episodes.eiId = showId,
          Episodes.eiDescription = Nothing,
          Episodes.eiAudioFilePath = Just "audio/episodes/2024/12/30/rerun-show_2024-12-30_abc.mp3",
          Episodes.eiAudioFileSize = Just 1024,
          Episodes.eiAudioMimeType = Just "audio/mpeg",
          Episodes.eiDurationSeconds = Just 3500,
          Episodes.eiArtworkUrl = Nothing,
          Episodes.eiScheduleTemplateId = Just templateId,
          Episodes.eiAirDate = Just (fromGregorian 2024 12 30),
          Episodes.eiCreatedBy = userId
        }
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right episodeId -> pure episodeId

-- | A rerun that started, with the given url.
rerunPlayed :: Text.Text -> PlayedRequest
rerunPlayed url =
  (psaPlayed "")
    { prTitle = "Rerun Show",
      prSourceType = "rerun",
      prSourceUrl = url,
      prPlannedStart = plannedAt 3600
    }

-- | The source type and the episode id of the newest row.
newestRow :: AppM (Text.Text, Maybe Int64)
newestRow =
  execQuery (PlaybackHistory.getRecentPlayback 1) >>= \case
    Right [row] -> pure (row.phSourceType, row.phEpisodeId)
    other -> error ("Expected one row, got " <> show other)

storesRerunEpisodeId :: TestDBConfig -> IO ()
storesRerunEpisodeId cfg = bracketAppM cfg $ do
  episodeId <- seedEarlierEpisode
  -- The handler finds the episode from the media url of the test server.
  appBaseUrl <- baseUrl
  recordOrFail (rerunPlayed (appBaseUrl <> "/media/audio/episodes/2024/12/30/rerun-show_2024-12-30_abc.mp3"))
  stored <- newestRow
  liftIO $ stored `shouldBe` ("rerun", Just (Episodes.unId episodeId))

storesRerunWithoutEpisode :: TestDBConfig -> IO ()
storesRerunWithoutEpisode cfg = bracketAppM cfg $ do
  _ <- seedEarlierEpisode
  -- A media url of the test server, so the handler reaches the episode lookup.
  appBaseUrl <- baseUrl
  recordOrFail (rerunPlayed (appBaseUrl <> "/media/audio/episodes/no-such-file.mp3"))
  stored <- newestRow
  liftIO $ stored `shouldBe` ("rerun", Nothing)

--------------------------------------------------------------------------------

-- | The body of a break item post, as radio.liq builds it with
-- @json.stringify@. Every value is a string, the break item id too.
decodesBreakItemBody :: Expectation
decodesBreakItemBody =
  eitherDecode
    "{\"prTitle\":\"Library Hours\",\"prArtist\":\"KPBJ 95.9 FM\",\
    \\"prSourceType\":\"psa\",\
    \\"prSourceUrl\":\"http://localhost:4000/media/audio/break-items/library.mp3\",\
    \\"prStartedAt\":\"2025-01-06T22:58:11Z\",\"prBreakItemId\":\"42\",\
    \\"prPlannedStart\":\"2025-01-06T22:58:10Z\"}"
    `shouldBe` Right
      PlayedRequest
        { prTitle = "Library Hours",
          prArtist = Just "KPBJ 95.9 FM",
          prSourceType = "psa",
          prSourceUrl = "http://localhost:4000/media/audio/break-items/library.mp3",
          prStartedAt = UTCTime (fromGregorian 2025 1 6) (22 * 3600 + 58 * 60 + 11),
          prBreakItemId = Just "42",
          prPlannedStart = Just (UTCTime (fromGregorian 2025 1 6) (22 * 3600 + 58 * 60 + 10))
        }

-- | The body of an episode post from an older client. The break item id is
-- an empty string, and there is no planned start.
decodesBodyWithoutPlannedStart :: Expectation
decodesBodyWithoutPlannedStart =
  eitherDecode
    "{\"prTitle\":\"Now Show\",\"prArtist\":\"KPBJ 95.9 FM\",\
    \\"prSourceType\":\"episode\",\
    \\"prSourceUrl\":\"http://localhost:4000/media/audio/episodes/now.mp3\",\
    \\"prStartedAt\":\"2025-01-06T22:00:00Z\",\"prBreakItemId\":\"\"}"
    `shouldBe` Right
      PlayedRequest
        { prTitle = "Now Show",
          prArtist = Just "KPBJ 95.9 FM",
          prSourceType = "episode",
          prSourceUrl = "http://localhost:4000/media/audio/episodes/now.mp3",
          prStartedAt = UTCTime (fromGregorian 2025 1 6) (22 * 3600),
          prBreakItemId = Just "",
          prPlannedStart = Nothing
        }

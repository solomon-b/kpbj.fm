{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

-- | Tests for the now-playing endpoint.
--
-- The top player shows the title that this endpoint returns, and links it to
-- the url. A wrong url sends a listener to the page of a different episode.
--
-- The database cases use the schedule of the now endpoint tests: one show on
-- Monday from 14:00 to 15:00, with one episode, one station ID, and one PSA.
module API.Stream.NowPlaying.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Links (rootLink, showEpisodesLinks)
import API.Playout.Now.Get.HandlerSpec (pacificAt, seedAll)
import API.Stream.NowPlaying.Get.Handler (actionAt, nowPlayingOf)
import API.Stream.NowPlaying.Get.Route (NowPlaying (..))
import API.Types (ShowEpisodesRoutes (..))
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (Value (..), object, toJSON, (.=))
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (TimeOfDay (..), UTCTime, addUTCTime)
import Domain.Timeline
  ( BreakTrack (..),
    Content (..),
    EphemeralCandidate (..),
    EpisodeCandidate (..),
    Item (..),
    Segment (..),
    itemAt,
  )
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Timeline (timelineAround)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "API.Stream.NowPlaying.Get.Handler.nowPlayingOf" $ do
    it "links an episode to its page" $ do
      rootLink (showEpisodesLinks.detail (Slug "now-show") 7) `shouldBe` "/shows/now-show/episodes/7"
      nowPlayingOf (Just (itemOf (EpisodeContent episode)))
        `shouldBe` NowPlaying "Now Show - Episode 7" (Just "/shows/now-show/episodes/7")

    it "gives a break track its title and no link" $
      nowPlayingOf (Just (itemOf (BreakContent track))) `shouldBe` NowPlaying "Library Hours" Nothing

    it "gives an ephemeral upload its title and no link" $
      nowPlayingOf (Just (itemOf (EphemeralContent upload))) `shouldBe` NowPlaying "Night Sounds" Nothing

    it "gives filler the station name and no link" $
      nowPlayingOf (Just (itemOf FillerContent)) `shouldBe` NowPlaying "KPBJ 95.9 FM" Nothing

    it "gives the station name when no item is on air" $
      nowPlayingOf Nothing `shouldBe` NowPlaying "KPBJ 95.9 FM" Nothing

    it "does not change the characters of a show title" $
      (nowPlayingOf (Just (itemOf (EpisodeContent episode {ecShowTitle = "Tom & \"Jerry\" <Live>"})))).npTitle
        `shouldBe` "Tom & \"Jerry\" <Live> - Episode 7"

    it "encodes the title and the url" $
      toJSON (NowPlaying "Now Show - Episode 7" (Just "/shows/now-show/episodes/7"))
        `shouldBe` object ["title" .= ("Now Show - Episode 7" :: Text), "url" .= ("/shows/now-show/episodes/7" :: Text)]

    it "encodes a missing url as null" $
      toJSON (NowPlaying "KPBJ 95.9 FM" Nothing)
        `shouldBe` object ["title" .= ("KPBJ 95.9 FM" :: Text), "url" .= Null]

  withTestDB $
    describe "API.Stream.NowPlaying.Get.Handler.actionAt" $ do
      it "returns the episode on air with its page" episodeOnAir
      it "gives the same answer for a fraction of a second" fractionOfSecond
      it "returns a break track with no link" breakTrackOnAir
      it "returns no link when nothing is scheduled" nothingScheduled

--------------------------------------------------------------------------------
-- Fixtures

itemOf :: Content -> Item
itemOf content =
  Item
    { itemContent = content,
      itemStart = start,
      itemPlannedStart = start,
      itemSeconds = 60,
      itemCueIn = 0,
      itemCueOut = Nothing
    }
  where
    start = pacificAt (TimeOfDay 14 0 0)

episode :: EpisodeCandidate
episode =
  EpisodeCandidate
    { ecId = Episodes.Id 7,
      ecShowTitle = "Now Show",
      ecShowSlug = Slug "now-show",
      ecEpisodeNumber = Episodes.EpisodeNumber 7,
      ecAudioKey = "audio/episodes/now.mp3",
      ecSeconds = 3500
    }

track :: BreakTrack
track =
  BreakTrack
    { btSourceType = "psa",
      btTitle = "Library Hours",
      btAudioKey = "audio/break-items/library.mp3",
      btSeconds = 30,
      btBreakItemId = Just 3
    }

upload :: EphemeralCandidate
upload =
  EphemeralCandidate
    { ecdId = EphemeralUploads.Id 8,
      ecdTitle = "Night Sounds",
      ecdAudioKey = "audio/ephemeral/night.mp3",
      ecdSeconds = 300
    }

--------------------------------------------------------------------------------
-- Cases

-- | The episode airs from 14:00. At 14:10 it is on air.
episodeOnAir :: TestDBConfig -> IO ()
episodeOnAir cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 10 0)
    segments <- segmentsAround now
    result <- actionAt now
    liftIO $ case itemContent <$> itemAt now segments of
      Just (EpisodeContent candidate) -> do
        let number = ecEpisodeNumber candidate
        result
          `shouldBe` NowPlaying
            ("Now Show - Episode " <> display number)
            (Just (rootLink (showEpisodesLinks.detail (Slug "now-show") number)))
      other -> expectationFailure ("Expected an episode on air, got " <> show other)

fractionOfSecond :: TestDBConfig -> IO ()
fractionOfSecond cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    let now = pacificAt (TimeOfDay 14 10 0)
    whole <- actionAt now
    fraction <- actionAt (addUTCTime 0.5 now)
    liftIO $ do
      fraction `shouldBe` whole
      whole.npUrl `shouldNotBe` Nothing

breakTrackOnAir :: TestDBConfig -> IO ()
breakTrackOnAir cfg =
  bracketAppM cfg $ do
    _ <- seedAll
    segments <- segmentsAround (pacificAt (TimeOfDay 14 10 0))
    case [(itemStart item, btTitle breakTrack) | item@Item {itemContent = BreakContent breakTrack} <- concatMap segItems segments] of
      [] -> liftIO $ expectationFailure "The timeline has no break track"
      (breakStart, title) : _ -> do
        result <- actionAt breakStart
        liftIO $ result `shouldBe` NowPlaying title Nothing

nothingScheduled :: TestDBConfig -> IO ()
nothingScheduled cfg =
  bracketAppM cfg $ do
    result <- actionAt (pacificAt (TimeOfDay 14 10 0))
    liftIO $ result.npUrl `shouldBe` Nothing

-- | The timeline that the handler reads for an instant.
segmentsAround :: UTCTime -> AppM [Segment]
segmentsAround instant =
  timelineAround instant instant >>= \case
    Left err -> error ("Timeline failed: " <> show err)
    Right segments -> pure segments

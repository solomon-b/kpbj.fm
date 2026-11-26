{-# LANGUAGE QuasiQuotes #-}

-- | Handler for GET /api/stream/now-playing.
module API.Stream.NowPlaying.Get.Handler
  ( handler,

    -- * Exported for testing
    actionAt,
    nowPlayingOf,
  )
where

--------------------------------------------------------------------------------

import API.Links (rootLink, showEpisodesLinks)
import API.Stream.NowPlaying.Get.Route (NowPlaying (..))
import API.Types (ShowEpisodesRoutes (..))
import App.Monad (AppM)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Timeline
  ( BreakTrack (..),
    Content (..),
    EphemeralCandidate (..),
    EpisodeCandidate (..),
    Item (..),
    itemAt,
  )
import Effects.Clock (currentSystemTime)
import Effects.Timeline (timelineAround)
import Log qualified

--------------------------------------------------------------------------------

-- | The title for audio that has no title of its own.
stationName :: Text
stationName = "KPBJ 95.9 FM"

-- | Handler for GET /api/stream/now-playing.
--
-- Returns the timeline item that airs now. It reads the same timeline as
-- @/api/playout/now@, so it states what Liquidsoap plays.
handler :: AppM NowPlaying
handler = currentSystemTime >>= actionAt

-- | 'handler' for a given instant.
--
-- On a timeline failure it logs the reason and gives the station name. The
-- route always answers 200, so the player needs no error path for it.
actionAt ::
  -- | The current time
  UTCTime ->
  AppM NowPlaying
actionAt now = do
  let wholeNow = floorToSecond now
  timelineAround wholeNow wholeNow >>= \case
    Left err -> do
      Log.logAttention "Now playing failed" (Text.pack (show err))
      pure (nowPlayingOf Nothing)
    Right segments -> pure (nowPlayingOf (itemAt wholeNow segments))

-- | The title and the page of an item. Only an episode and a rerun have a
-- page.
nowPlayingOf :: Maybe Item -> NowPlaying
nowPlayingOf mItem =
  case itemContent <$> mItem of
    Just (EpisodeContent episode) ->
      NowPlaying
        { npTitle = [i|#{ecShowTitle episode} - Episode #{ecEpisodeNumber episode}|],
          npUrl = Just (rootLink (showEpisodesLinks.detail (ecShowSlug episode) (ecEpisodeNumber episode)))
        }
    Just (RerunContent episode) ->
      NowPlaying
        { npTitle = [i|#{ecShowTitle episode} - Episode #{ecEpisodeNumber episode} (Rerun)|],
          npUrl = Just (rootLink (showEpisodesLinks.detail (ecShowSlug episode) (ecEpisodeNumber episode)))
        }
    Just (EphemeralContent upload) -> NowPlaying (ecdTitle upload) Nothing
    Just (BreakContent track) -> NowPlaying (btTitle track) Nothing
    Just FillerContent -> NowPlaying stationName Nothing
    Nothing -> NowPlaying stationName Nothing

-- | Drop the fraction of a second. The timeline works in whole seconds.
floorToSecond :: UTCTime -> UTCTime
floorToSecond instant =
  posixSecondsToUTCTime (fromIntegral (floor (utcTimeToPOSIXSeconds instant) :: Integer))

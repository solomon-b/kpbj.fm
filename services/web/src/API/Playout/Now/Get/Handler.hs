-- | Handler for GET /api/playout/now.
module API.Playout.Now.Get.Handler
  ( handler,

    -- * Exported for testing
    actionAt,
    segmentId,
    toItemResponse,
  )
where

--------------------------------------------------------------------------------

import API.Playout.Types (ItemResponse (..), NowResponse (..), SegmentResponse (..), sanitizeAnnotateValue)
import App.BaseUrl (absoluteMediaUrl, baseUrl)
import App.Monad (AppM)
import App.Storage (StorageBackend)
import Control.Monad.Catch (throwM)
import Control.Monad.Reader (asks)
import Crypto.Hash (SHA256, hashlazy)
import Data.Aeson qualified as Aeson
import Data.Has qualified as Has
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Domain.Timeline
  ( BreakTrack (..),
    Content (..),
    EphemeralCandidate (..),
    EpisodeCandidate (..),
    Item (..),
    Segment (..),
    fillerObjectKey,
    resumeAt,
    segmentContaining,
    segmentStartingAt,
  )
import Effects.Clock (currentSystemTime)
import Effects.Timeline (lookAround, timelineAround)
import Log qualified
import Servant.Server (err503)

--------------------------------------------------------------------------------

-- | The artist that the station uses for its own audio.
stationArtist :: Text
stationArtist = "KPBJ 95.9 FM"

-- | Handler for GET /api/playout/now.
--
-- Returns the segment that airs now, or the segment that starts at @from@, and
-- the segment after it. On any failure it logs the reason and answers 503.
-- Liquidsoap retries on any status other than 200, so a failure never makes
-- it play wrong audio.
handler :: Maybe UTCTime -> AppM NowResponse
handler mFrom = do
  now <- currentSystemTime
  result <- actionAt now mFrom
  case result of
    Left reason -> do
      Log.logAttention "Playout now failed" reason
      throwM err503
    Right segment -> pure segment

-- | 'handler' for a given instant.
--
-- Without @from@, the first item resumes at the current whole second. With
-- @from@, the items are the whole segment that starts at that instant. The
-- next segment is always whole.
--
-- A @from@ before now, or more than 'lookAround' after now, gives an error
-- before any query runs. Liquidsoap only asks for the next boundary. This
-- check stops other callers from building break plans for any day.
actionAt ::
  -- | The current time
  UTCTime ->
  -- | The start of the wanted segment
  Maybe UTCTime ->
  AppM (Either Text NowResponse)
actionAt now mFrom =
  case mFrom of
    Just from
      | isOutOfRange from ->
          pure (Left ("The requested from is out of range: " <> Text.pack (show from)))
    _ -> timelineAt now mFrom
  where
    isOutOfRange from = from < now || from > addUTCTime lookAround now

-- | 'actionAt' after the range check.
timelineAt :: UTCTime -> Maybe UTCTime -> AppM (Either Text NowResponse)
timelineAt now mFrom = do
  let wholeNow = floorToSecond now
      centre = fromMaybe wholeNow mFrom
  timelineResult <- timelineAround wholeNow centre
  case timelineResult of
    Left err -> pure (Left ("The timeline query failed: " <> Text.pack (show err)))
    Right segments ->
      case mFrom of
        Nothing ->
          case segmentContaining wholeNow segments of
            Nothing -> pure (Left "No segment contains the current time")
            Just segment -> Right <$> toNowResponse now segments segment (resumeAt wholeNow segment)
        Just from ->
          case segmentStartingAt from segments of
            Nothing -> pure (Left "No segment starts at the requested time")
            Just segment -> Right <$> toNowResponse now segments segment (segItems segment)

-- | Drop the fraction of a second.
--
-- 'resumeAt' floors its instant. The first item must start at the same whole
-- second, so the handler floors first.
floorToSecond :: UTCTime -> UTCTime
floorToSecond instant =
  posixSecondsToUTCTime (fromIntegral (floor (utcTimeToPOSIXSeconds instant) :: Integer))

-- | Build the response for a segment, the items to play, and the segment after
-- it in the timeline.
toNowResponse :: UTCTime -> [Segment] -> Segment -> [Item] -> AppM NowResponse
toNowResponse now segments segment items = do
  current <- toSegmentResponse now segment items
  next <- traverse (\later -> toSegmentResponse now later (segItems later)) (segmentStartingAt (segEnd segment) segments)
  pure NowResponse {nrSegment = current, nrNext = next}

-- | Build the response for a segment and the items to play.
--
-- The id comes from the whole segment, not from the items to play. A segment
-- resumed after a restart keeps the id that it has as @next@ and at @from@.
toSegmentResponse :: UTCTime -> Segment -> [Item] -> AppM SegmentResponse
toSegmentResponse now segment items = do
  storageBackend <- asks (Has.getter @StorageBackend)
  appBaseUrl <- baseUrl
  let mediaUrl key = sanitizeAnnotateValue (absoluteMediaUrl appBaseUrl storageBackend key)
  pure
    SegmentResponse
      { srId = segmentId (segStart segment) (segEnd segment) (map (toItemResponse mediaUrl) (segItems segment)),
        srSegmentStart = segStart segment,
        srSegmentEnd = segEnd segment,
        srEndsInSeconds = realToFrac (diffUTCTime (segEnd segment) now),
        srItems = map (toItemResponse mediaUrl) items
      }

-- | The id of a segment: the SHA-256 of its start, its end, and the fields of
-- each item that decide what airs, as hex.
--
-- The fields are the url, the cue points, the break item id, and the planned
-- start. Two segments with the same id play the same audio at the same times.
-- The title and the artist do not count. The JSON encoding keeps the fields
-- apart, so no two lists of fields give the same input to the hash.
segmentId :: UTCTime -> UTCTime -> [ItemResponse] -> Text
segmentId start end items =
  Text.pack (show (hashlazy @SHA256 (Aeson.encode (start, end, map airingFields items))))
  where
    airingFields item = (item.irUrl, item.irCueIn, item.irCueOut, item.irBreakItemId, item.irPlannedStart)

-- | Build the response for one item.
toItemResponse :: (Text -> Text) -> Item -> ItemResponse
toItemResponse mediaUrl item =
  ItemResponse
    { irType = itemType,
      irUrl = mediaUrl audioKey,
      irTitle = sanitizeAnnotateValue title,
      irArtist = sanitizeAnnotateValue artist,
      irDurationSeconds = itemSeconds item,
      irCueIn = itemCueIn item,
      irCueOut = itemCueOut item,
      irBreakItemId = breakItemId,
      irPlannedStart = itemPlannedStart item
    }
  where
    (itemType, audioKey, title, artist, breakItemId) =
      case itemContent item of
        EpisodeContent episode ->
          ("episode", ecAudioKey episode, ecShowTitle episode, stationArtist, Nothing)
        RerunContent episode ->
          ("rerun", ecAudioKey episode, ecShowTitle episode, stationArtist, Nothing)
        EphemeralContent upload ->
          ("ephemeral", ecdAudioKey upload, ecdTitle upload, stationArtist, Nothing)
        BreakContent track ->
          (btSourceType track, btAudioKey track, btTitle track, stationArtist, btBreakItemId track)
        FillerContent ->
          ("filler", fillerObjectKey, stationArtist, stationArtist, Nothing)

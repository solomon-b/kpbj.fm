module Test.Gen.Time where

--------------------------------------------------------------------------------

import Data.Fixed (Pico, resolution)
import Data.Time
import Data.Time.Clock.POSIX
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

-- | Truncate a UTCTime to microsecond precision to match PostgreSQL timestamptz.
truncateToMicroseconds :: UTCTime -> UTCTime
truncateToMicroseconds (UTCTime day dt) =
  let picoPerMicro = resolution (0 :: Pico) `div` 1000000
      picos = diffTimeToPicoseconds dt
      truncated = (picos `div` picoPerMicro) * picoPerMicro
   in UTCTime day (picosecondsToDiffTime truncated)

genUTCTime :: (MonadGen m) => m UTCTime
genUTCTime = do
  posixTime <- Gen.realFrac_ (Range.linearFrac 0 (10 * 365 * 24 * 60 * 60))
  pure $ truncateToMicroseconds $ posixSecondsToUTCTime posixTime

genFutureUTCTime :: (MonadGen m) => m UTCTime
genFutureUTCTime = do
  posixTime <- Gen.realFrac_ (Range.linearFrac (10 * 365 * 24 * 60 * 60) (20 * 365 * 24 * 60 * 60))
  pure $ truncateToMicroseconds $ posixSecondsToUTCTime posixTime

-- | A date within about ten years of the epoch.
--
-- The counterpart of 'genUTCTime' for @episodes.air_date@. A date needs no
-- truncation, because PostgreSQL stores it exactly.
genDay :: (MonadGen m) => m Day
genDay = do
  offset <- Gen.integral (Range.linear 0 (10 * 365))
  pure $ addDays offset (fromGregorian 1970 1 1)

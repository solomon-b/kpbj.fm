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

module Test.Gen.Time where

--------------------------------------------------------------------------------

import Data.Time
import Data.Time.Clock.POSIX
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genUTCTime :: (MonadGen m) => m UTCTime
genUTCTime = do
  posixTime <- Gen.realFrac_ (Range.linearFrac 0 (10 * 365 * 24 * 60 * 60))
  pure $ posixSecondsToUTCTime posixTime

genFutureUTCTime :: (MonadGen m) => m UTCTime
genFutureUTCTime = do
  posixTime <- Gen.realFrac_ (Range.linearFrac (10 * 365 * 24 * 60 * 60) (20 * 365 * 24 * 60 * 60))
  pure $ posixSecondsToUTCTime posixTime

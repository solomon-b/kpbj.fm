module Test.Gen.Tables.Events where

--------------------------------------------------------------------------------

import Control.Monad.IO.Class (MonadIO (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Test.Gen.DomainTypes (genSlug)
import Test.Gen.Text (genText)
import Test.Gen.Time (genFutureUTCTime, genUTCTime)

--------------------------------------------------------------------------------

genEventStatus :: (MonadGen m) => m Events.Status
genEventStatus = Gen.enumBounded

eventInsertGen :: (MonadIO m, MonadGen m) => User.Id -> m Events.Insert
eventInsertGen userId = do
  eiTitle <- genText
  eiSlug <- genSlug
  eiDescription <- genText
  eiStartsAt <- genUTCTime
  eiEndsAt <- genFutureUTCTime
  eiLocationName <- genText
  eiLocationAddress <- genText
  eiStatus <- genEventStatus
  eiAuthorId <- pure userId
  pure Events.Insert {..}

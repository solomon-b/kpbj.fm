module Test.Gen.Tables.EventImages where

--------------------------------------------------------------------------------

import Effects.Database.Tables.EventImages qualified as EventImages
import Effects.Database.Tables.Events qualified as Events
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText, genUrl)

--------------------------------------------------------------------------------

eventImageInsertGen :: (MonadGen m) => Events.Id -> m EventImages.Insert
eventImageInsertGen eventId = do
  iiImagePath <- genUrl
  iiCaption <- genText
  iiAltText <- genText
  iiSortOrder <- Gen.int64 (Range.linear 0 100)
  pure
    EventImages.Insert
      { iiEventId = eventId,
        iiImagePath = iiImagePath,
        iiCaption = iiCaption,
        iiAltText = iiAltText,
        iiSortOrder = iiSortOrder
      }

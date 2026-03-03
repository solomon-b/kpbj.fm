module Test.Gen.Tables.EpisodeTrack where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Hedgehog (MonadGen (..))
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range
import Test.Gen.Text (genText)

--------------------------------------------------------------------------------

episodeTrackInsertGen :: (MonadGen m) => Episodes.Id -> m EpisodeTrack.Insert
episodeTrackInsertGen episodeId = do
  etiTrackNumber <- Gen.integral (Range.linear 1 100 :: Range.Range Int64)
  etiTitle <- genText
  etiArtist <- genText
  let etiEpisodeId = episodeId
  pure EpisodeTrack.Insert {..}

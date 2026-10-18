-- | Fixtures for the break item handler tests.
--
-- The promo pages and the staff sections all need a show, a promo row, or a
-- staged audio upload. The values here look like what the dashboard writes.
module Test.Handler.BreakItemFixtures
  ( insertActiveShow,
    pendingPromoInsert,
    stagedBreakItemAudio,
    writeStagedAudioFile,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (fromGregorian)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Hasql.Transaction qualified as TRX
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)
import Test.Database.Helpers (unwrapInsert)

--------------------------------------------------------------------------------

-- | Insert an active show with this title and slug.
insertActiveShow :: Text -> Text -> TRX.Transaction Shows.Id
insertActiveShow title slug =
  unwrapInsert $
    Shows.insertShow
      Shows.Insert
        { Shows.siTitle = title,
          Shows.siSlug = Slug slug,
          Shows.siDescription = Nothing,
          Shows.siLogoUrl = Nothing,
          Shows.siStatus = Shows.Active
        }

-- | An 8 second promo for a show, waiting for staff review.
pendingPromoInsert :: User.Id -> Shows.Id -> BreakItems.Insert
pendingPromoInsert creatorId showId =
  BreakItems.Insert
    { BreakItems.biiTitle = "Tune In To Night Shift",
      BreakItems.biiCategory = BreakItems.ShowPromo,
      BreakItems.biiUnderwriterId = Nothing,
      BreakItems.biiShowId = Just showId,
      BreakItems.biiReviewStatus = BreakItems.Pending,
      BreakItems.biiAudioFilePath = "audio/break-items/2026/10/01/break-item_2026-10-01_abc123.mp3",
      BreakItems.biiMimeType = "audio/mpeg",
      BreakItems.biiFileSize = 400000,
      BreakItems.biiDurationSeconds = 8,
      BreakItems.biiStartsOn = fromGregorian 2026 10 1,
      BreakItems.biiEndsOn = Nothing,
      BreakItems.biiCreatorId = creatorId
    }

-- | A staged break item upload as the upload handler writes it.
--
-- The token doubles as the file name, so each case stages its own file.
stagedBreakItemAudio :: Text -> User.Model -> Int64 -> StagedUploads.Insert
stagedBreakItemAudio name userModel durationSeconds =
  StagedUploads.Insert
    { StagedUploads.siToken = StagedUploads.Token name,
      StagedUploads.siUserId = User.mId userModel,
      StagedUploads.siOriginalName = "promo.mp3",
      StagedUploads.siStoragePath = stagingPath name,
      StagedUploads.siMimeType = "audio/mpeg",
      StagedUploads.siFileSize = 400000,
      StagedUploads.siDurationSeconds = Just durationSeconds,
      StagedUploads.siUploadType = StagedUploads.BreakItemAudio
    }

-- | Put the staged file on disk. The claim moves it, so it has to exist.
writeStagedAudioFile :: Text -> IO ()
writeStagedAudioFile name = do
  let stagingFilePath = "/tmp/kpbj/" <> Text.unpack (stagingPath name)
  createDirectoryIfMissing True (takeDirectory stagingFilePath)
  writeFile stagingFilePath "fake audio content"

-- | Where the staged file sits, relative to the local storage root.
stagingPath :: Text -> Text
stagingPath name = "temp/staging/" <> name <> ".mp3"

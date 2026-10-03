{-# LANGUAGE PackageImports #-}

module API.Dashboard.Underwriting.Delivery.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriting.Delivery.Get.Handler (action, parseMonth)
import API.Dashboard.Underwriting.Delivery.Get.Templates.Page (DeliveryRow (..), maxAirings)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Time (Day, LocalTime (..), TimeOfDay (..), fromGregorian)
import Domain.Types.Timezone (pacificToUtc)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "API.Dashboard.Underwriting.Delivery.Get.Handler.parseMonth" $ do
    it "parses YYYY-MM to the 1st" $
      parseMonth "2025-02" `shouldBe` Just (fromGregorian 2025 2 1)
    it "rejects a month out of range" $
      parseMonth "2025-13" `shouldBe` Nothing

  withTestDB $
    describe "API.Dashboard.Underwriting.Delivery.Get.Handler.action" $
      it "counts active days and airings per underwriter" countsPerUnderwriter

--------------------------------------------------------------------------------

february :: Day
february = fromGregorian 2025 2 1

-- | Insert a staff user, two underwriters with one announcement each, and
-- three airings of the first underwriter's announcement.
--
-- The hardware announcement runs all of February. The bakery announcement
-- starts on February 15, so it runs 14 of the month's 28 days.
seed :: AppM ()
seed = do
  userInsert <- liftIO $ mkUserInsert "delivery" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    hardware <- unwrapInsert (Underwriters.insertUnderwriter "Hardware")
    bakery <- unwrapInsert (Underwriters.insertUnderwriter "Bakery")
    hardwareSpot <- unwrapInsert (BreakItems.insertBreakItem (announcement "Hardware Spot" hardware userId))
    _ <-
      unwrapInsert $
        BreakItems.insertBreakItem
          (announcement "Bakery Spot" bakery userId) {BreakItems.biiStartsOn = fromGregorian 2025 2 15}
    forM_ [3, 4, 5] $ \dayOfMonth ->
      TRX.statement () $
        PlaybackHistory.insertPlayback
          PlaybackHistory.Insert
            { piTitle = "Hardware Spot",
              piArtist = Nothing,
              piSourceType = "underwriting",
              piSourceUrl = "http://localhost:4000/media/audio/break-items/hardware.mp3",
              piEpisodeId = Nothing,
              piBreakItemId = Just (BreakItems.unId hardwareSpot),
              piStartedAt = pacificToUtc (LocalTime (fromGregorian 2025 2 dayOfMonth) (TimeOfDay 10 28 0))
            }
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right () -> pure ()

announcement :: Text -> Underwriters.Id -> User.Id -> BreakItems.Insert
announcement title underwriterId creatorId =
  BreakItems.Insert
    { biiTitle = title,
      biiCategory = BreakItems.Underwriting,
      biiUnderwriterId = Just underwriterId,
      biiAudioFilePath = "audio/break-items/2025/01/01/spot_2025-01-01_abc.mp3",
      biiMimeType = "audio/mpeg",
      biiFileSize = 1024,
      biiDurationSeconds = 30,
      biiStartsOn = february,
      biiEndsOn = Nothing,
      biiCreatorId = creatorId
    }

countsPerUnderwriter :: TestDBConfig -> IO ()
countsPerUnderwriter cfg = bracketAppM cfg $ do
  seed
  result <- runExceptT (action february)
  liftIO $ case result of
    Left err -> expectationFailure ("Expected rows, got " <> show err)
    Right rows -> do
      let summary r = (r.drUnderwriter.uwName, r.drDaysActive, maxAirings r, length r.drAirings)
      -- Bakery: active February 15 to 28, so 14 days and at most 28 airings.
      -- Hardware: active all 28 days, so at most 56 airings.
      map summary rows `shouldBe` [("Bakery", 14, 28, 0), ("Hardware", 28, 56, 3)]

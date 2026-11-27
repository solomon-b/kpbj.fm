{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.MissingEpisodes.Past.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.MissingEpisodes.Past.Get.Handler (PastViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Foldable (for_)
import Domain.Attendance (AttendanceRange (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.MissingEpisodes.Past.Get.Handler" $ do
      describe "action" $ do
        it "staff user on empty DB gets no slots for each range" test_staffEmptyDbNoSlots

--------------------------------------------------------------------------------

-- | A database with no air record has no finished slots to count. The action
-- answers each range with an empty list and keeps the range.
test_staffEmptyDbNoSlots :: TestDBConfig -> IO ()
test_staffEmptyDbNoSlots cfg = do
  staffInsert <- mkUserInsert "me-past-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels staffInsert
    (_userModel, userMetaModel) <- expectSetupRight dbResult

    for_ [minBound .. maxBound :: AttendanceRange] $ \range -> do
      result <- runExceptT $ action userMetaModel range

      liftIO $ case result of
        Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
        Right vd -> do
          vd.pvdRange `shouldBe` range
          vd.pvdSlots `shouldBe` []

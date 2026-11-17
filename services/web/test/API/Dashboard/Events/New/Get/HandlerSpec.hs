{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.New.Get.Handler (NewEventViewData (..), action)
import App.Handler.Error ()
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
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
    describe "API.Dashboard.Events.New.Get.Handler" $ do
      describe "action" $ do
        it "returns view data with user metadata for staff user" test_returnsViewData

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user gets valid view data containing their user metadata.
test_returnsViewData :: TestDBConfig -> IO ()
test_returnsViewData cfg = do
  userInsert <- mkUserInsert "ev-new-get-staff" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.nevUserMetadata `shouldBe` userMetaModel

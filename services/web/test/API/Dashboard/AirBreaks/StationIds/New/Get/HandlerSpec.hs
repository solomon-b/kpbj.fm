module API.Dashboard.AirBreaks.StationIds.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.StationIds.New.Get.Handler (StationIdFormViewData (..), action)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldSatisfy)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.AirBreaks.StationIds.New.Get.Handler" $ do
      describe "action" $ do
        it "host user gets form view data" test_hostUserGetsFormViewData
        it "upload URL is populated" test_uploadUrlIsPopulated

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

test_hostUserGetsFormViewData :: TestDBConfig -> IO ()
test_hostUserGetsFormViewData cfg = do
  userInsert <- mkUserInsert "si-new-host" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

test_uploadUrlIsPopulated :: TestDBConfig -> IO ()
test_uploadUrlIsPopulated cfg = do
  userInsert <- mkUserInsert "si-new-url" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> sifUploadUrl vd `shouldSatisfy` (/= "")

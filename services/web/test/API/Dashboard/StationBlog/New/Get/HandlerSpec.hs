module API.Dashboard.StationBlog.New.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.StationBlog.New.Get.Handler (StationBlogNewViewData (..), action)
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
    describe "API.Dashboard.StationBlog.New.Get.Handler" $ do
      describe "action" $ do
        it "staff user gets valid view data with metadata" test_returnsViewData

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user calling action receives view data with matching metadata.
test_returnsViewData :: TestDBConfig -> IO ()
test_returnsViewData cfg = do
  userInsert <- mkUserInsert "sb-new-get-vd" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          setupUserModels userInsert
    (_userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd ->
        UserMetadata.mId (snvdUserMetadata vd) `shouldBe` UserMetadata.mId userMetaModel

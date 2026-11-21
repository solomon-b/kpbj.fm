module API.Dashboard.AirBreaks.Promos.Id.Reject.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Handler (action)
import API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Route (RejectForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.BreakItemFixtures (insertActiveShow, pendingPromoInsert)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.AirBreaks.Promos.Id.Reject.Post.Handler.action" $ do
      it "rejects a pending promo and keeps the trimmed note" test_rejectsPending
      it "refuses an empty note and leaves the promo pending" test_refusesEmptyNote
      it "refuses to reject a promo that is already approved" test_refusesApproved

--------------------------------------------------------------------------------

-- | A staff user and a promo for "Night Shift" with this review status.
setup :: TestDBConfig -> Text -> BreakItems.ReviewStatus -> IO BreakItems.Id
setup cfg name status = do
  userInsert <- mkUserInsert name UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, _) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      let promo = (pendingPromoInsert (User.mId userModel) showId) {BreakItems.biiReviewStatus = status}
      unwrapInsert (BreakItems.insertBreakItem promo)
    liftIO $ expectSetupRight dbResult

test_rejectsPending :: TestDBConfig -> IO ()
test_rejectsPending cfg = do
  promoId <- setup cfg "reject-pending" BreakItems.Pending
  bracketAppM cfg $ do
    let form = RejectForm {rfNote = "  Say the day and time the show airs.  "}
    result <- runExceptT (action promoId form)
    liftIO $ case result of
      Right promo -> do
        BreakItems.bimReviewStatus promo `shouldBe` BreakItems.Rejected
        BreakItems.bimReviewNote promo `shouldBe` Just "Say the day and time the show airs."
      Left err -> expectationFailure $ "Expected the reject to succeed, got " <> show err

test_refusesEmptyNote :: TestDBConfig -> IO ()
test_refusesEmptyNote cfg = do
  promoId <- setup cfg "reject-empty-note" BreakItems.Pending
  bracketAppM cfg $ do
    result <- runExceptT (action promoId RejectForm {rfNote = "   \n  "})
    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      other -> expectationFailure $ "Expected a validation error, got " <> show other
    stored <- execQuery (BreakItems.getById promoId)
    liftIO $ case stored of
      Right (Just promo) -> BreakItems.bimReviewStatus promo `shouldBe` BreakItems.Pending
      other -> expectationFailure $ "Expected the promo, got " <> show other

test_refusesApproved :: TestDBConfig -> IO ()
test_refusesApproved cfg = do
  promoId <- setup cfg "reject-approved" BreakItems.Approved
  bracketAppM cfg $ do
    result <- runExceptT (action promoId RejectForm {rfNote = "Too late."})
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      other -> expectationFailure $ "Expected NotFound, got " <> show other

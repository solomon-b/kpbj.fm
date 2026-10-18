module API.Dashboard.AirBreaks.Promos.Id.Approve.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Id.Approve.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
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
    describe "API.Dashboard.AirBreaks.Promos.Id.Approve.Post.Handler.action" $ do
      it "approves a pending promo" test_approvesPending
      it "refuses to approve a promo that is already approved" test_refusesApproved
      it "refuses a host" test_refusesHost

--------------------------------------------------------------------------------

-- | A user with this role and a pending promo for "Night Shift".
setup :: TestDBConfig -> Text -> UserMetadata.UserRole -> IO (UserMetadata.Model, BreakItems.Id)
setup cfg name role = do
  userInsert <- mkUserInsert name role
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      promoId <- unwrapInsert (BreakItems.insertBreakItem (pendingPromoInsert (User.mId userModel) showId))
      pure (userMeta, promoId)
    liftIO $ expectSetupRight dbResult

test_approvesPending :: TestDBConfig -> IO ()
test_approvesPending cfg = do
  (userMeta, promoId) <- setup cfg "approve-pending" UserMetadata.Staff
  bracketAppM cfg $ do
    result <- runExceptT (action userMeta promoId)
    liftIO $ case result of
      Right promo -> BreakItems.bimReviewStatus promo `shouldBe` BreakItems.Approved
      Left err -> expectationFailure $ "Expected the approve to succeed, got " <> show err

test_refusesApproved :: TestDBConfig -> IO ()
test_refusesApproved cfg = do
  (userMeta, promoId) <- setup cfg "approve-twice" UserMetadata.Staff
  bracketAppM cfg $ do
    _ <- runExceptT (action userMeta promoId)
    second <- runExceptT (action userMeta promoId)
    liftIO $ case second of
      Left (NotFound _) -> pure ()
      other -> expectationFailure $ "Expected NotFound, got " <> show other

test_refusesHost :: TestDBConfig -> IO ()
test_refusesHost cfg = do
  (userMeta, promoId) <- setup cfg "approve-host" UserMetadata.Host
  bracketAppM cfg $ do
    result <- runExceptT (action userMeta promoId)
    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      other -> expectationFailure $ "Expected NotAuthorized, got " <> show other

module API.Dashboard.AirBreaks.Promos.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Get.Handler (PromoListViewData (..), action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Map.Strict qualified as Map
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.BreakItemFixtures (insertActiveShow, pendingPromoInsert)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.AirBreaks.Promos.Get.Handler.action" $ do
      it "lists the promos and their shows for staff" test_listsForStaff
      it "lists pending promos on page 1 only" test_pendingOnFirstPageOnly
      it "refuses a host, also for a promo of their own show" test_refusesHost

--------------------------------------------------------------------------------

test_listsForStaff :: TestDBConfig -> IO ()
test_listsForStaff cfg = do
  userInsert <- mkUserInsert "promo-review-staff" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      promoId <- unwrapInsert (BreakItems.insertBreakItem (pendingPromoInsert (User.mId userModel) showId))
      pure (userMeta, showId, promoId)
    (userMeta, showId, promoId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT (action userMeta Nothing)
    liftIO $ case result of
      Right vd -> do
        map BreakItems.bimId (plvPromos vd) `shouldBe` [promoId]
        Map.keys (plvShowsById vd) `shouldBe` [showId]
        plvHasMore vd `shouldBe` False
      Left err -> expectationFailure $ "Expected the list, got " <> show err

-- | Page 1 lists the pending promo before the approved promo, although the
-- approved promo is newer. Page 2 leaves out the pending promo.
test_pendingOnFirstPageOnly :: TestDBConfig -> IO ()
test_pendingOnFirstPageOnly cfg = do
  userInsert <- mkUserInsert "promo-review-pages" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      let promoInsert = pendingPromoInsert (User.mId userModel) showId
      pendingId <- unwrapInsert (BreakItems.insertBreakItem promoInsert)
      approvedId <- unwrapInsert (BreakItems.insertBreakItem promoInsert {BreakItems.biiReviewStatus = BreakItems.Approved})
      pure (userMeta, pendingId, approvedId)
    (userMeta, pendingId, approvedId) <- liftIO $ expectSetupRight dbResult

    firstPage <- runExceptT (action userMeta (Just (PageNumber 1)))
    secondPage <- runExceptT (action userMeta (Just (PageNumber 2)))
    liftIO $ case (firstPage, secondPage) of
      (Right first, Right second) -> do
        map BreakItems.bimId (plvPromos first) `shouldBe` [pendingId, approvedId]
        map BreakItems.bimId (plvPromos second) `shouldBe` []
      (Left err, _) -> expectationFailure $ "Expected page 1, got " <> show err
      (_, Left err) -> expectationFailure $ "Expected page 2, got " <> show err

test_refusesHost :: TestDBConfig -> IO ()
test_refusesHost cfg = do
  userInsert <- mkUserInsert "promo-review-host" UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      addTestShowHost showId (User.mId userModel)
      _ <- unwrapInsert (BreakItems.insertBreakItem (pendingPromoInsert (User.mId userModel) showId))
      pure userMeta
    userMeta <- liftIO $ expectSetupRight dbResult

    result <- runExceptT (action userMeta Nothing)
    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized, got " <> show err
      Right _ -> expectationFailure "Expected NotAuthorized, got the list"

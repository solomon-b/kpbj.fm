module API.Dashboard.Shows.Slug.Promos.Id.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Promos.Id.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
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
    describe "API.Dashboard.Shows.Slug.Promos.Id.Delete.Handler.action" $ do
      it "withdraws a promo of the show" test_withdrawsPromo
      it "refuses a promo of another show" test_refusesPromoOfOtherShow

--------------------------------------------------------------------------------

-- | The rows every case needs. The handler checks access to the show with
-- 'App.Handler.Combinators.authorizeShow' before 'action' runs.
data Fixture = Fixture
  { fxNightShift :: Shows.Model,
    fxMorningGlory :: Shows.Model,
    -- | A pending promo for "Night Shift"
    fxPromoId :: BreakItems.Id
  }

-- | Two shows, and a promo for "Night Shift".
setup :: TestDBConfig -> Text -> IO Fixture
setup cfg prefix = do
  creatorInsert <- mkUserInsert (prefix <> "-creator") UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (creator, _) <- setupUserModels creatorInsert
      nightShiftId <- insertActiveShow "Night Shift" "night-shift"
      morningGloryId <- insertActiveShow "Morning Glory" "morning-glory"
      promoId <-
        unwrapInsert $
          BreakItems.insertBreakItem (pendingPromoInsert (User.mId creator) nightShiftId)
      nightShift <- getShow nightShiftId
      morningGlory <- getShow morningGloryId
      pure
        Fixture
          { fxNightShift = nightShift,
            fxMorningGlory = morningGlory,
            fxPromoId = promoId
          }
    liftIO $ expectSetupRight dbResult
  where
    getShow showId = TRX.statement () (Shows.getShowById showId) >>= maybe (error "show not found") pure

test_withdrawsPromo :: TestDBConfig -> IO ()
test_withdrawsPromo cfg = do
  fixture <- setup cfg "withdraw-own"
  bracketAppM cfg $ do
    result <- runExceptT (action (fxNightShift fixture) (fxPromoId fixture))
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the withdraw to succeed, got " <> show err
    stored <- execQuery (BreakItems.getById (fxPromoId fixture))
    liftIO $ case stored of
      Right mPromo -> fmap BreakItems.bimId mPromo `shouldBe` Nothing
      Left err -> expectationFailure $ "Database error: " <> show err

-- | A host of "Morning Glory" names their own show in the URL, with the promo
-- of "Night Shift". The promo does not belong to the show in the URL, so the
-- action refuses it, and the promo stays live.
test_refusesPromoOfOtherShow :: TestDBConfig -> IO ()
test_refusesPromoOfOtherShow cfg = do
  fixture <- setup cfg "withdraw-other-show"
  bracketAppM cfg $ do
    result <- runExceptT (action (fxMorningGlory fixture) (fxPromoId fixture))
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      other -> expectationFailure $ "Expected NotFound, got " <> show other
    stored <- execQuery (BreakItems.getById (fxPromoId fixture))
    liftIO $ case stored of
      Right (Just _) -> pure ()
      other -> expectationFailure $ "Expected the promo to stay live, got " <> show other

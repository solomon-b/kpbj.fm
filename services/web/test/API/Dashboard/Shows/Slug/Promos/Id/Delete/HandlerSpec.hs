module API.Dashboard.Shows.Slug.Promos.Id.Delete.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Promos.Id.Delete.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
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
    describe "API.Dashboard.Shows.Slug.Promos.Id.Delete.Handler.action" $ do
      it "lets the host of the show withdraw its promo" test_hostWithdraws
      it "refuses a host of another show who names the promo's show" test_otherHostWithPromoShow
      it "refuses a host of another show who names their own show" test_otherHostWithOwnShow

--------------------------------------------------------------------------------

-- | The rows every case needs.
data Fixture = Fixture
  { fxNightShiftHost :: (User.Model, UserMetadata.Model),
    fxMorningGloryHost :: (User.Model, UserMetadata.Model),
    -- | A pending promo for "Night Shift"
    fxPromoId :: BreakItems.Id
  }

-- | Two shows, one host each, and a promo for "Night Shift".
setup :: TestDBConfig -> Text -> IO Fixture
setup cfg prefix = do
  nightShiftInsert <- mkUserInsert (prefix <> "-night-shift") UserMetadata.Host
  morningGloryInsert <- mkUserInsert (prefix <> "-morning-glory") UserMetadata.Host
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      nightShiftHost <- setupUserModels nightShiftInsert
      morningGloryHost <- setupUserModels morningGloryInsert
      nightShiftId <- insertActiveShow "Night Shift" "night-shift"
      morningGloryId <- insertActiveShow "Morning Glory" "morning-glory"
      addTestShowHost nightShiftId (User.mId (fst nightShiftHost))
      addTestShowHost morningGloryId (User.mId (fst morningGloryHost))
      promoId <-
        unwrapInsert $
          BreakItems.insertBreakItem (pendingPromoInsert (User.mId (fst nightShiftHost)) nightShiftId)
      pure
        Fixture
          { fxNightShiftHost = nightShiftHost,
            fxMorningGloryHost = morningGloryHost,
            fxPromoId = promoId
          }
    liftIO $ expectSetupRight dbResult

-- | Withdraw as this user through this show slug, and expect a refusal. The
-- promo must stay live.
expectRefused :: TestDBConfig -> Fixture -> (User.Model, UserMetadata.Model) -> Slug -> IO ()
expectRefused cfg fixture (userModel, userMeta) showSlug =
  bracketAppM cfg $ do
    result <- runExceptT (action userModel userMeta showSlug (fxPromoId fixture))
    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      other -> expectationFailure $ "Expected NotAuthorized, got " <> show other
    stored <- execQuery (BreakItems.getById (fxPromoId fixture))
    liftIO $ case stored of
      Right (Just _) -> pure ()
      other -> expectationFailure $ "Expected the promo to stay live, got " <> show other

test_hostWithdraws :: TestDBConfig -> IO ()
test_hostWithdraws cfg = do
  fixture <- setup cfg "withdraw-own"
  let (userModel, userMeta) = fxNightShiftHost fixture
  bracketAppM cfg $ do
    result <- runExceptT (action userModel userMeta (Slug "night-shift") (fxPromoId fixture))
    liftIO $ case result of
      Right () -> pure ()
      Left err -> expectationFailure $ "Expected the withdraw to succeed, got " <> show err
    stored <- execQuery (BreakItems.getById (fxPromoId fixture))
    liftIO $ case stored of
      Right mPromo -> fmap BreakItems.bimId mPromo `shouldBe` Nothing
      Left err -> expectationFailure $ "Database error: " <> show err

test_otherHostWithPromoShow :: TestDBConfig -> IO ()
test_otherHostWithPromoShow cfg = do
  fixture <- setup cfg "withdraw-promo-show"
  expectRefused cfg fixture (fxMorningGloryHost fixture) (Slug "night-shift")

-- | The access check reads the show from the promo row, so naming a show the
-- user hosts in the URL does not help.
test_otherHostWithOwnShow :: TestDBConfig -> IO ()
test_otherHostWithOwnShow cfg = do
  fixture <- setup cfg "withdraw-own-show"
  expectRefused cfg fixture (fxMorningGloryHost fixture) (Slug "morning-glory")

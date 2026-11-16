module API.Dashboard.Shows.Slug.Promos.New.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm (..))
import API.Dashboard.Shows.Slug.Promos.New.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.StagedUploads qualified as StagedUploads
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.BreakItemFixtures (insertActiveShow, stagedBreakItemAudio, writeStagedAudioFile)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Promos.New.Post.Handler.action" $ do
      it "saves a pending promo for the host's show" test_hostSubmitsPendingPromo
      it "saves a promo from staff as pending too" test_staffSubmitIsPending
      it "refuses a host who does not host the show" test_refusesOtherHost
      it "refuses a promo longer than 30 seconds" test_refusesLongPromo

--------------------------------------------------------------------------------

-- | A promo form for the given staged upload token.
promoForm :: Text -> BreakItemForm
promoForm token =
  BreakItemForm
    { bifTitle = "Night Shift Every Friday",
      bifAudioToken = token,
      bifStartsOn = "2026-10-20",
      bifEndsOn = "2026-11-30",
      bifUnderwriterId = ""
    }

-- | Insert a user with this role who hosts "Night Shift", and stage an
-- upload of this length under this token.
setupHostAndUpload ::
  TestDBConfig ->
  Text ->
  UserMetadata.UserRole ->
  -- | Upload length in seconds
  Int64 ->
  IO (User.Model, UserMetadata.Model, Shows.Id)
setupHostAndUpload cfg token role seconds = do
  userInsert <- mkUserInsert token role
  writeStagedAudioFile token
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      showId <- insertActiveShow "Night Shift" "night-shift"
      addTestShowHost showId (User.mId userModel)
      _uploadId <- TRX.statement () (StagedUploads.insert (stagedBreakItemAudio token userModel seconds))
      pure (userModel, userMeta, showId)
    liftIO $ expectSetupRight dbResult

-- | Submit the form and check the saved row.
expectPendingPromo :: TestDBConfig -> Text -> UserMetadata.UserRole -> IO ()
expectPendingPromo cfg token role = do
  (userModel, userMeta, showId) <- setupHostAndUpload cfg token role 8
  bracketAppM cfg $ do
    result <- runExceptT (action userModel userMeta (Slug "night-shift") (promoForm token))
    case result of
      Left err -> liftIO $ expectationFailure $ "Expected the promo to save, got " <> show err
      Right promoId -> do
        stored <- execQuery (BreakItems.getById promoId)
        liftIO $ case stored of
          Right (Just promo) -> do
            BreakItems.bimCategory promo `shouldBe` BreakItems.ShowPromo
            BreakItems.bimShowId promo `shouldBe` Just showId
            BreakItems.bimReviewStatus promo `shouldBe` BreakItems.Pending
            BreakItems.bimUnderwriterId promo `shouldBe` Nothing
            BreakItems.bimCreatorId promo `shouldBe` User.mId userModel
            BreakItems.bimDurationSeconds promo `shouldBe` 8
          other -> expectationFailure $ "Expected the saved promo, got " <> show other

test_hostSubmitsPendingPromo :: TestDBConfig -> IO ()
test_hostSubmitsPendingPromo cfg = expectPendingPromo cfg "promo-host-submit" UserMetadata.Host

test_staffSubmitIsPending :: TestDBConfig -> IO ()
test_staffSubmitIsPending cfg = expectPendingPromo cfg "promo-staff-submit" UserMetadata.Staff

-- | A host of "Morning Glory" submits for "Night Shift", which they do not
-- host. The handler refuses before it claims the upload or saves a row.
test_refusesOtherHost :: TestDBConfig -> IO ()
test_refusesOtherHost cfg = do
  userInsert <- mkUserInsert "promo-other-host" UserMetadata.Host
  writeStagedAudioFile "promo-other-host"
  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMeta) <- setupUserModels userInsert
      ownShowId <- insertActiveShow "Morning Glory" "morning-glory"
      addTestShowHost ownShowId (User.mId userModel)
      otherShowId <- insertActiveShow "Night Shift" "night-shift"
      _uploadId <- TRX.statement () (StagedUploads.insert (stagedBreakItemAudio "promo-other-host" userModel 8))
      pure (userModel, userMeta, otherShowId)
    (userModel, userMeta, otherShowId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT (action userModel userMeta (Slug "night-shift") (promoForm "promo-other-host"))
    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      other -> expectationFailure $ "Expected NotAuthorized, got " <> show other

    promos <- execQuery (BreakItems.getPromosForShow otherShowId)
    liftIO $ case promos of
      Right [] -> pure ()
      other -> expectationFailure $ "Expected no promos, got " <> show other

-- | The host stages a 31 second upload. The handler refuses it before it
-- claims the upload or saves a row.
test_refusesLongPromo :: TestDBConfig -> IO ()
test_refusesLongPromo cfg = do
  (userModel, userMeta, showId) <- setupHostAndUpload cfg "promo-too-long" UserMetadata.Host 31
  bracketAppM cfg $ do
    result <- runExceptT (action userModel userMeta (Slug "night-shift") (promoForm "promo-too-long"))
    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      other -> expectationFailure $ "Expected ValidationError, got " <> show other

    promos <- execQuery (BreakItems.getPromosForShow showId)
    liftIO $ case promos of
      Right [] -> pure ()
      other -> expectationFailure $ "Expected no promos, got " <> show other

module API.Dashboard.Profile.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Profile.Edit.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Servant.Multipart (Input (..), Mem, MultipartData (..))
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Profile.Edit.Post.Handler" $ do
      describe "action" $ do
        it "updates profile successfully with valid form data" test_updatesProfileSuccess
        it "returns Left on missing required form fields" test_missingFieldsFailure

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | A complete, valid multipart form payload for profile updates.
validMultipartData :: MultipartData Mem
validMultipartData =
  MultipartData
    { inputs =
        [ Input "display_name" "Updated Name",
          Input "full_name" "Updated Full Name",
          Input "color_scheme" "DarkMode",
          Input "theme" "gruvbox"
        ],
      files = []
    }

-- | A multipart form payload with no inputs, missing all required fields.
emptyMultipartData :: MultipartData Mem
emptyMultipartData =
  MultipartData
    { inputs = [],
      files = []
    }

--------------------------------------------------------------------------------

-- | Submitting a fully-populated form for an existing user succeeds and
-- returns a redirect response.
test_updatesProfileSuccess :: TestDBConfig -> IO ()
test_updatesProfileSuccess cfg = do
  userInsert <- mkUserInsert "profile-edit-post-success" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel validMultipartData
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

-- | Submitting a form with no inputs fails validation because required fields
-- (display_name, full_name, color_scheme, theme) are absent.
test_missingFieldsFailure :: TestDBConfig -> IO ()
test_missingFieldsFailure cfg = do
  userInsert <- mkUserInsert "profile-edit-post-missing" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        userId <- insertTestUser userInsert
        TRX.statement () (User.getUser userId) >>= maybe (error "user not found") pure
    userModel <- liftIO $ expectSetupRight dbResult
    result <- runExceptT $ action userModel emptyMultipartData
    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

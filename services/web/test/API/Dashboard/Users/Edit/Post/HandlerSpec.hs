{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PackageImports #-}

module API.Dashboard.Users.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Data.Text.Display (display)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Servant.Multipart (Input (..), Mem, MultipartData (..))
import Test.Database.Helpers (insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, nonExistentId)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe, shouldNotBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Users.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for missing user" test_notFoundForMissingUser
        it "updates user metadata successfully" test_updatesUserMetadata
        it "returns Right on success" test_returnsRight
        it "fails validation on missing fields" test_failsValidationOnMissingFields
        it "rejects a role change with no permission" test_rejectsRoleWithoutPermission

--------------------------------------------------------------------------------

-- | Build a valid multipart form for editing a user. A Staff user gets the
-- form with no role field.
validEditForm :: Text -> Text -> MultipartData Mem
validEditForm displayName fullName =
  MultipartData
    { inputs =
        [ Input "display_name" displayName,
          Input "full_name" fullName
        ],
      files = []
    }

-- | The form with a role field, as an Admin gets it.
withRole :: Text -> MultipartData Mem -> MultipartData Mem
withRole role form = form {inputs = inputs form <> [Input "role" role]}

--------------------------------------------------------------------------------

-- | Returns NotFound when the target user does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  let form = validEditForm "New Name" "New Full Name"
  bracketAppM cfg $ do
    result <- runExceptT $ action Nothing (User.Id nonExistentId) form
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Updates a user's display name with no role field, and keeps the role.
test_updatesUserMetadata :: TestDBConfig -> IO ()
test_updatesUserMetadata cfg = do
  targetInsert <- mkUserInsert "usr-edit-post-target-update" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    let form = validEditForm "Updated Name" "Updated Full Name"
    result <- runExceptT $ action Nothing targetId form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    meta <- getMetadata targetId
    liftIO $ do
      display meta.mDisplayName `shouldBe` "Updated Name"
      meta.mUserRole `shouldBe` UserMetadata.User

-- | Action returns Right with redirect headers on a successful update.
test_returnsRight :: TestDBConfig -> IO ()
test_returnsRight cfg = do
  targetInsert <- mkUserInsert "usr-edit-post-target-right" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    let form = validEditForm "Staff Name" "Staff Full Name"
    result <- runExceptT $ action Nothing targetId form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

-- | Returns ValidationError when required form fields are missing.
test_failsValidationOnMissingFields :: TestDBConfig -> IO ()
test_failsValidationOnMissingFields cfg = do
  targetInsert <- mkUserInsert "usr-edit-post-target-validation" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    -- Form with only display_name (missing full_name)
    let incompleteForm =
          MultipartData
            { inputs = [Input "display_name" "Only Name"],
              files = []
            }
    result <- runExceptT $ action Nothing targetId incompleteForm

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

-- | A role in the form with no permission is a ValidationError, and nothing
-- is saved. This is a Staff user who sends a role field by hand.
test_rejectsRoleWithoutPermission :: TestDBConfig -> IO ()
test_rejectsRoleWithoutPermission cfg = do
  targetInsert <- mkUserInsert "usr-edit-post-target-escalate" UserMetadata.Staff
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    let form = withRole "Admin" (validEditForm "Escalated Name" "Escalated Full Name")
    result <- runExceptT $ action Nothing targetId form

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

    meta <- getMetadata targetId
    liftIO $ do
      meta.mUserRole `shouldBe` UserMetadata.Staff
      display meta.mDisplayName `shouldNotBe` "Escalated Name"

--------------------------------------------------------------------------------

-- | Read the metadata of a user, and fail the test when it is absent.
getMetadata :: User.Id -> AppM UserMetadata.Model
getMetadata userId = do
  metaResult <-
    runDB $
      TRX.transaction TRX.ReadCommitted TRX.Read $
        TRX.statement () (UserMetadata.getUserMetadata userId)
  liftIO $ do
    mMeta <- expectSetupRight metaResult
    maybe (fail "Expected metadata to exist") pure mMeta

{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Edit.Post.Handler (action)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
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
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

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

--------------------------------------------------------------------------------

-- | Build a valid multipart form for editing a user.
validEditForm :: Text -> Text -> Text -> MultipartData Mem
validEditForm displayName fullName role =
  MultipartData
    { inputs =
        [ Input "display_name" displayName,
          Input "full_name" fullName,
          Input "role" role
        ],
      files = []
    }

--------------------------------------------------------------------------------

-- | Returns NotFound when the target user does not exist.
test_notFoundForMissingUser :: TestDBConfig -> IO ()
test_notFoundForMissingUser cfg = do
  let form = validEditForm "New Name" "New Full Name" "User"
  bracketAppM cfg $ do
    result <- runExceptT $ action (User.Id nonExistentId) form
    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Successfully updates a user's display name and role.
test_updatesUserMetadata :: TestDBConfig -> IO ()
test_updatesUserMetadata cfg = do
  targetInsert <- mkUserInsert "usr-edit-post-target-update" UserMetadata.User
  bracketAppM cfg $ do
    dbResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Write $
          insertTestUser targetInsert
    targetId <- liftIO $ expectSetupRight dbResult

    let form = validEditForm "Updated Name" "Updated Full Name" "Host"
    result <- runExceptT $ action targetId form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Verify the role was actually updated in the DB.
    metaResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (UserMetadata.getUserMetadata targetId)
    liftIO $ do
      metaResult' <- expectSetupRight metaResult
      case metaResult' of
        Nothing -> expectationFailure "Expected metadata to exist after update"
        Just meta -> meta.mUserRole `shouldBe` UserMetadata.Host

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

    let form = validEditForm "Staff Name" "Staff Full Name" "Staff"
    result <- runExceptT $ action targetId form

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

    -- Form with only display_name (missing full_name and role)
    let incompleteForm =
          MultipartData
            { inputs = [Input "display_name" "Only Name"],
              files = []
            }
    result <- runExceptT $ action targetId incompleteForm

    liftIO $ case result of
      Left (ValidationError _) -> pure ()
      Left err -> expectationFailure $ "Expected ValidationError but got: " <> show err
      Right _ -> expectationFailure "Expected Left ValidationError but got Right"

{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Shows.Slug.Edit.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Slug.Edit.Post.Handler (action)
import API.Dashboard.Shows.Slug.Edit.Post.Route (ShowEditForm (..))
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Shows.Slug.Edit.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent slug" test_notFoundForMissingSlug
        it "updates show title when valid form submitted" test_updatesShowTitle
        it "updates show description when valid form submitted" test_updatesShowDescription
        it "preserves existing logo when sefLogoClear is False and no new file" test_preservesLogoUrl

--------------------------------------------------------------------------------

-- | Minimal valid edit form.
editForm :: Text -> Text -> ShowEditForm
editForm title status =
  ShowEditForm
    { sefTitle = title,
      sefDescription = "",
      sefTags = Nothing,
      sefLogoFile = Nothing,
      sefLogoClear = False,
      sefStatus = status,
      sefHosts = [],
      sefSchedulesJson = Nothing
    }

--------------------------------------------------------------------------------

-- | Calling action with a slug that does not exist in the DB yields NotFound.
test_notFoundForMissingSlug :: TestDBConfig -> IO ()
test_notFoundForMissingSlug cfg = do
  userInsert <- mkUserInsert "edit-notfound" UserMetadata.Staff
  let nonexistentSlug = Slug "no-such-show-slug"
  let form = editForm "Any Title" "active"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      TRX.statement () (UserMetadata.getUserMetadata userId)
        >>= maybe (error "metadata not found") pure

    userMetaModel <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel nonexistentSlug form

    liftIO $ case result of
      Left (NotFound _) -> pure ()
      Left err -> expectationFailure $ "Expected NotFound but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | Submitting a valid edit form updates the show title in the database.
test_updatesShowTitle :: TestDBConfig -> IO ()
test_updatesShowTitle cfg = do
  userInsert <- mkUserInsert "edit-title" UserMetadata.Staff

  let originalSlug = Slug "edit-title-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Original Title",
            Shows.siSlug = originalSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }

  let newTitle = "Updated Show Title"
  let form = editForm newTitle "active"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel, showId)

    (userMetaModel, showModel, showId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    -- Fetch by ID (stable) rather than by generated slug (brittle)
    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowById showId)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.title s `shouldBe` newTitle

-- | Submitting a form with a description stores it on the show.
test_updatesShowDescription :: TestDBConfig -> IO ()
test_updatesShowDescription cfg = do
  userInsert <- mkUserInsert "edit-desc" UserMetadata.Staff

  let showSlug = Slug "edit-desc-show"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Desc Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = Nothing,
            Shows.siStatus = Shows.Active
          }
  let form =
        (editForm "Edit Desc Show" "active")
          { sefDescription = "Updated description text."
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel)

    (userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug showSlug)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.description s `shouldBe` Just "Updated description text."

-- | When sefLogoClear is False and no new file is uploaded, the existing logo
-- URL is preserved on the show.
test_preservesLogoUrl :: TestDBConfig -> IO ()
test_preservesLogoUrl cfg = do
  userInsert <- mkUserInsert "edit-logo" UserMetadata.Staff

  let showSlug = Slug "edit-logo-show"
  let originalLogoUrl = Just "https://cdn.example.com/logo.png"
  let showInsert =
        Shows.Insert
          { Shows.siTitle = "Edit Logo Show",
            Shows.siSlug = showSlug,
            Shows.siDescription = Nothing,
            Shows.siLogoUrl = originalLogoUrl,
            Shows.siStatus = Shows.Active
          }
  let form =
        (editForm "Edit Logo Show" "active")
          { sefLogoClear = False,
            sefLogoFile = Nothing
          }

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      userMetaModel <-
        TRX.statement () (UserMetadata.getUserMetadata userId)
          >>= maybe (error "metadata not found") pure
      (showId, _templateId) <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      showModel <-
        TRX.statement () (Shows.getShowById showId)
          >>= maybe (error "show not found") pure
      pure (userMetaModel, showModel)

    (userMetaModel, showModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userMetaModel showModel.slug form

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right _ -> pure ()

    updatedShowResult <-
      runDB $
        TRX.transaction TRX.ReadCommitted TRX.Read $
          TRX.statement () (Shows.getShowBySlug showSlug)

    liftIO $ do
      updatedShowResult' <- expectSetupRight updatedShowResult
      case updatedShowResult' of
        Nothing -> expectationFailure "Expected updated show to exist in DB but got Nothing"
        Just s -> Shows.logoUrl s `shouldBe` originalLogoUrl

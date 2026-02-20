{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Handler (EventListViewData (..), action)
import App.Handler.Error ()
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.Get.Handler" $ do
      describe "action" $ do
        it "returns empty list on fresh DB" test_emptyListOnFreshDB
        it "returns event after insert" test_returnsEventOnInsert
        it "defaults to page 1 when maybePage is Nothing" test_paginationDefaults

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Staff user on an empty DB gets an empty elvEvents list.
test_emptyListOnFreshDB :: TestDBConfig -> IO ()
test_emptyListOnFreshDB cfg = do
  userInsert <- mkUserInsert "ev-get-empty" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.elvEvents `shouldBe` []

-- | After inserting an event, it appears in the elvEvents list.
test_returnsEventOnInsert :: TestDBConfig -> IO ()
test_returnsEventOnInsert cfg = do
  userInsert <- mkUserInsert "ev-get-insert" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Test Concert",
                Events.eiSlug = Slug.mkSlug "test-concert",
                Events.eiDescription = "A great concert.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "The Venue",
                Events.eiLocationAddress = "123 Main St",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = userModel.mId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (userModel, userMetaModel, eventId)
    (userModel, userMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> do
        let eventIds = map Events.emId vd.elvEvents
        elem eventId eventIds `shouldBe` True

-- | When maybePage is Nothing, elvPage is set to 1.
test_paginationDefaults :: TestDBConfig -> IO ()
test_paginationDefaults cfg = do
  userInsert <- mkUserInsert "ev-get-page" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels userInsert
      pure (userModel, userMetaModel)
    (userModel, userMetaModel) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action userModel userMetaModel Nothing

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.elvPage `shouldBe` 1

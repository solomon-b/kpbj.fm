{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Feature.HandlerSpec where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Slug.Feature.Post.Handler (FeatureEventViewData (..), action)
import App.Handler.Combinators (requireStaffNotSuspended)
import App.Handler.Error (HandlerError (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Domain.Types.Slug qualified as Slug
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEvent, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.Events.Slug.Feature.Post.Handler" $ do
      describe "action" $ do
        it "returns NotFound for a nonexistent event ID" test_notFoundForMissingEvent
        it "promotes an unfeatured event to featured" test_promotesEventToFeatured
        it "demotes a featured event from featured" test_demotesEventFromFeatured
        it "returns NotAuthorized for a host-role user" test_hostCannotFeature

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Calling action with a nonexistent event ID yields NotFound "Event".
test_notFoundForMissingEvent :: TestDBConfig -> IO ()
test_notFoundForMissingEvent cfg =
  bracketAppM cfg $ do
    result <- runExceptT $ action (Events.Id 99999)

    liftIO $ case result of
      Left (NotFound "Event") -> pure ()
      Left err -> expectationFailure $ "Expected NotFound \"Event\" but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotFound but got Right"

-- | An unfeatured event is promoted to featured, verified via fevUpdatedEvent.
test_promotesEventToFeatured :: TestDBConfig -> IO ()
test_promotesEventToFeatured cfg = do
  userInsert <- mkUserInsert "ev-feature-promote" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Promote To Featured",
                Events.eiSlug = Slug.mkSlug "promote-to-featured",
                Events.eiDescription = "This event will be promoted.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Promote Venue",
                Events.eiLocationAddress = "1 Promote St",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = userId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      insertTestEvent eventInsert
    eventId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action eventId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.fevUpdatedEvent.emFeaturedOnHomepage `shouldBe` True

-- | A featured event is demoted to not featured, verified via fevUpdatedEvent.
test_demotesEventFromFeatured :: TestDBConfig -> IO ()
test_demotesEventFromFeatured cfg = do
  userInsert <- mkUserInsert "ev-feature-demote" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      userId <- insertTestUser userInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Demote From Featured",
                Events.eiSlug = Slug.mkSlug "demote-from-featured",
                Events.eiDescription = "This event will be demoted.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Demote Venue",
                Events.eiLocationAddress = "2 Demote St",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = userId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = True
              }
      insertTestEvent eventInsert
    eventId <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ action eventId

    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> vd.fevUpdatedEvent.emFeaturedOnHomepage `shouldBe` False

-- | A host-role user should be denied permission to feature events.
test_hostCannotFeature :: TestDBConfig -> IO ()
test_hostCannotFeature cfg = do
  hostInsert <- mkUserInsert "ev-feature-host-denied" UserMetadata.Host
  authorInsert <- mkUserInsert "ev-feature-host-author" UserMetadata.Staff

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_, hostMetaModel) <- setupUserModels hostInsert
      authorId <- insertTestUser authorInsert
      let eventInsert =
            Events.Insert
              { Events.eiTitle = "Host Cannot Feature",
                Events.eiSlug = Slug.mkSlug "host-cannot-feature",
                Events.eiDescription = "Host should not be able to feature this.",
                Events.eiStartsAt = read "2026-06-15 17:00:00 UTC",
                Events.eiEndsAt = read "2026-06-15 19:00:00 UTC",
                Events.eiLocationName = "Denied Venue",
                Events.eiLocationAddress = "3 Denied St",
                Events.eiStatus = Events.Published,
                Events.eiAuthorId = authorId,
                Events.eiPosterImageUrl = Nothing,
                Events.eiFeaturedOnHomepage = False
              }
      eventId <- insertTestEvent eventInsert
      pure (hostMetaModel, eventId)
    (hostMetaModel, eventId) <- liftIO $ expectSetupRight dbResult

    result <- runExceptT $ do
      requireStaffNotSuspended "You do not have permission to feature events." hostMetaModel
      action eventId

    liftIO $ case result of
      Left (NotAuthorized _ _) -> pure ()
      Left err -> expectationFailure $ "Expected NotAuthorized but got: " <> show err
      Right _ -> expectationFailure "Expected Left NotAuthorized but got Right"

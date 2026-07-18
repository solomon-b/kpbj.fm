{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module API.Archive.Get.HandlerSpec where

--------------------------------------------------------------------------------

import API.Archive.Get.Handler (ArchiveListViewData (..), action)
import Control.Monad (forM_)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Time (UTCTime, addUTCTime)
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestEpisode, insertTestShowWithSchedule, insertTestUser)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Archive.Get.Handler" $ do
      describe "action" $ do
        it "returns no episodes on an empty DB" test_emptyDB
        it "returns published episodes across multiple shows" test_acrossShows
        it "includes episodes from inactive shows" test_inactiveShow
        it "paginates: page 1 full + hasMore, page 2 remainder" test_pagination

--------------------------------------------------------------------------------

-- | A past scheduled time so episodes count as "published".
baseScheduledAt :: UTCTime
baseScheduledAt = read "2020-01-01 10:00:00 UTC"

-- | Build a past-scheduled episode insert at an explicit time.
-- @eiId@ is the show id — the field is named misleadingly in @Episodes.Insert@.
-- The @(show_id, scheduled_at)@ pair must be unique, so callers inserting
-- multiple episodes for one show must vary the time.
mkEpisodeAt :: UTCTime -> Shows.Id -> ShowSchedule.TemplateId -> User.Id -> Episodes.Insert
mkEpisodeAt scheduledAt showId templateId userId =
  Episodes.Insert
    { Episodes.eiId = showId,
      Episodes.eiDescription = Just "Archive test episode",
      Episodes.eiAudioFilePath = Nothing,
      Episodes.eiAudioFileSize = Nothing,
      Episodes.eiAudioMimeType = Nothing,
      Episodes.eiDurationSeconds = Nothing,
      Episodes.eiArtworkUrl = Nothing,
      Episodes.eiScheduleTemplateId = Just templateId,
      Episodes.eiScheduledAt = Just scheduledAt,
      Episodes.eiCreatedBy = userId
    }

-- | Episode at the default past time (one-per-show tests).
mkEpisode :: Shows.Id -> ShowSchedule.TemplateId -> User.Id -> Episodes.Insert
mkEpisode = mkEpisodeAt baseScheduledAt

--------------------------------------------------------------------------------

-- | Empty DB -> empty list, hasMore False.
test_emptyDB :: TestDBConfig -> IO ()
test_emptyDB cfg = bracketAppM cfg $ do
  result <- runExceptT $ action (Just 1)
  liftIO $ case result of
    Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
    Right vd -> do
      length vd.alvdEpisodes `shouldBe` 0
      vd.alvdHasMore `shouldBe` False

-- | Two active shows, one past episode each -> 2 results.
test_acrossShows :: TestDBConfig -> IO ()
test_acrossShows cfg = do
  userA <- mkUserInsert "archive-a" UserMetadata.Host
  userB <- mkUserInsert "archive-b" UserMetadata.Host
  let showA = Shows.Insert "Archive Show A" (Slug "archive-show-a") Nothing Nothing Shows.Active
      showB = Shows.Insert "Archive Show B" (Slug "archive-show-b") Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        uidA <- insertTestUser userA
        uidB <- insertTestUser userB
        (sidA, tidA) <- insertTestShowWithSchedule showA defaultScheduleInsert
        (sidB, tidB) <- insertTestShowWithSchedule showB defaultScheduleInsert
        _ <- insertTestEpisode (mkEpisode sidA tidA uidA)
        _ <- insertTestEpisode (mkEpisode sidB tidB uidB)
        pure ()
    _ <- expectSetupRight dbResult

    result <- runExceptT $ action (Just 1)
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length vd.alvdEpisodes `shouldBe` 2

-- | An Inactive (but not deleted) show's episode still appears.
test_inactiveShow :: TestDBConfig -> IO ()
test_inactiveShow cfg = do
  user <- mkUserInsert "archive-inactive" UserMetadata.Host
  let inactiveShow = Shows.Insert "Archive Inactive" (Slug "archive-inactive") Nothing Nothing Shows.Inactive

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        uid <- insertTestUser user
        (sid, tid) <- insertTestShowWithSchedule inactiveShow defaultScheduleInsert
        _ <- insertTestEpisode (mkEpisode sid tid uid)
        pure ()
    _ <- expectSetupRight dbResult

    result <- runExceptT $ action (Just 1)
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right vd -> length vd.alvdEpisodes `shouldBe` 1

-- | 13 episodes -> page 1 returns 12 (hasMore True); page 2 returns 1 (hasMore False).
test_pagination :: TestDBConfig -> IO ()
test_pagination cfg = do
  user <- mkUserInsert "archive-page" UserMetadata.Host
  let paginated = Shows.Insert "Archive Paginated" (Slug "archive-paginated") Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $
      TRX.transaction TRX.ReadCommitted TRX.Write $ do
        uid <- insertTestUser user
        (sid, tid) <- insertTestShowWithSchedule paginated defaultScheduleInsert
        -- Distinct scheduled_at per episode: (show_id, scheduled_at) is unique.
        forM_ [1 .. (13 :: Int)] $ \idx ->
          insertTestEpisode (mkEpisodeAt (addUTCTime (fromIntegral idx * 86400) baseScheduledAt) sid tid uid)
    _ <- expectSetupRight dbResult

    page1 <- runExceptT $ action (Just 1)
    liftIO $ case page1 of
      Left err -> expectationFailure $ "page 1 Left: " <> show err
      Right vd -> do
        length vd.alvdEpisodes `shouldBe` 12
        vd.alvdHasMore `shouldBe` True

    page2 <- runExceptT $ action (Just 2)
    liftIO $ case page2 of
      Left err -> expectationFailure $ "page 2 Left: " <> show err
      Right vd -> do
        length vd.alvdEpisodes `shouldBe` 1
        vd.alvdHasMore `shouldBe` False

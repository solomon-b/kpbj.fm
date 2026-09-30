{-# LANGUAGE PackageImports #-}

-- | Tests for the played endpoint.
--
-- Delivery counts come from @playback_history.break_item_id@, so the endpoint
-- must store the id Liquidsoap sends, and store nothing for an empty string.
module API.Playout.Played.Post.HandlerSpec where

--------------------------------------------------------------------------------

import API.Playout.Played.Post.Handler (record)
import API.Playout.Types (PlayedRequest (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Int (Int64)
import Data.Text qualified as Text
import Data.Time (UTCTime (..), fromGregorian)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (insertTestUser, unwrapInsert)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (mkUserInsert)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)
import "kpbj-web" App.Monad (AppM)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Playout.Played.Post.Handler.record" $ do
      it "stores the break item id" storesBreakItemId
      it "stores no break item id for an empty string" storesNothingForEmpty

--------------------------------------------------------------------------------

-- | Insert a staff user and one PSA. Returns the PSA's id.
seedPsa :: AppM BreakItems.Id
seedPsa = do
  userInsert <- liftIO $ mkUserInsert "played" UserMetadata.Staff
  result <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    userId <- insertTestUser userInsert
    unwrapInsert $
      BreakItems.insertBreakItem
        BreakItems.Insert
          { biiTitle = "Library Hours",
            biiCategory = BreakItems.Psa,
            biiUnderwriterId = Nothing,
            biiSpotsPerMonth = Nothing,
            biiAudioFilePath = "audio/break-items/2025/01/01/library_2025-01-01_def.mp3",
            biiMimeType = "audio/mpeg",
            biiFileSize = 1024,
            biiDurationSeconds = 30,
            biiStartsOn = fromGregorian 2025 1 1,
            biiEndsOn = Nothing,
            biiCreatorId = userId
          }
  case result of
    Left err -> error ("Setup failed: " <> show err)
    Right psaId -> pure psaId

-- | A PSA track that started with this break item id annotation.
psaPlayed :: Text.Text -> PlayedRequest
psaPlayed breakItemId =
  PlayedRequest
    { prTitle = "Library Hours",
      prArtist = Nothing,
      prSourceType = "psa",
      prSourceUrl = "http://localhost:4000/media/audio/break-items/library.mp3",
      prStartedAt = UTCTime (fromGregorian 2025 1 6) 3600,
      prBreakItemId = Just breakItemId
    }

-- | Record the request, then read back the newest row's break item id.
recordAndRead :: PlayedRequest -> AppM (Maybe Int64)
recordAndRead request = do
  recorded <- runExceptT (record request)
  case recorded of
    Left err -> error ("Record failed: " <> show err)
    Right () -> pure ()
  execQuery (PlaybackHistory.getRecentPlayback 1) >>= \case
    Right [row] -> pure row.phBreakItemId
    other -> error ("Expected one row, got " <> show other)

storesBreakItemId :: TestDBConfig -> IO ()
storesBreakItemId cfg = bracketAppM cfg $ do
  psaId <- seedPsa
  stored <- recordAndRead (psaPlayed (Text.pack (show (BreakItems.unId psaId))))
  liftIO $ stored `shouldBe` Just (BreakItems.unId psaId)

storesNothingForEmpty :: TestDBConfig -> IO ()
storesNothingForEmpty cfg = bracketAppM cfg $ do
  _ <- seedPsa
  stored <- recordAndRead (psaPlayed "")
  liftIO $ case stored of
    Nothing -> pure ()
    Just n -> expectationFailure ("Expected no break item id, got " <> show n)

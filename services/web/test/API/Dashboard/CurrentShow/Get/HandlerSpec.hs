module API.Dashboard.CurrentShow.Get.HandlerSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.CurrentShow.Get.Handler (action)
import App.Dashboard.ShowContext (ShowSection (..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Transaction.Sessions qualified as TRX
import Test.Database.Helpers (addTestShowHost, insertTestShowWithSchedule)
import Test.Database.Monad (TestDBConfig, withTestDB)
import Test.Handler.Fixtures (defaultScheduleInsert, expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (bracketAppM)
import Test.Hspec (Spec, describe, expectationFailure, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "API.Dashboard.CurrentShow.Get.Handler" $ do
      describe "action" $ do
        it "gives the episodes URL of the only show" test_episodesUrl
        it "gives the promos URL of the only show" test_promosUrl
        it "gives the settings URL of the only show" test_settingsUrl
        it "gives the URL of the second show when the cookie names it" test_cookieNamesSecondShow
        it "gives nothing to a host with no shows" test_noShowsGivesNothing

--------------------------------------------------------------------------------

test_episodesUrl :: TestDBConfig -> IO ()
test_episodesUrl cfg = do
  result <- resolveForHost cfg "cg-episodes" SectionEpisodes Nothing
  result `shouldBe` Just "dashboard/episodes/cg-episodes-alpha"

test_promosUrl :: TestDBConfig -> IO ()
test_promosUrl cfg = do
  result <- resolveForHost cfg "cg-promos" SectionPromos Nothing
  result `shouldBe` Just "dashboard/shows/cg-promos-alpha/promos"

test_settingsUrl :: TestDBConfig -> IO ()
test_settingsUrl cfg = do
  result <- resolveForHost cfg "cg-settings" SectionSettings Nothing
  result `shouldBe` Just "dashboard/shows/cg-settings-alpha/edit"

test_cookieNamesSecondShow :: TestDBConfig -> IO ()
test_cookieNamesSecondShow cfg = do
  result <- resolveForHost cfg "cg-second" SectionEpisodes (Just "dashboard_show=cg-second-beta")
  result `shouldBe` Just "dashboard/episodes/cg-second-beta"

-- | A host with no shows gets nothing, even when the cookie names a show.
test_noShowsGivesNothing :: TestDBConfig -> IO ()
test_noShowsGivesNothing cfg = do
  hostInsert <- mkUserInsert "cg-noshows" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_, userMetaModel) <- setupUserModels hostInsert
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action (Just (Cookie "dashboard_show=cg-noshows-alpha")) userMetaModel SectionEpisodes
    liftIO $ case result of
      Left err -> expectationFailure $ "Expected Right but got Left: " <> show err
      Right mUri -> fmap show mUri `shouldBe` Nothing

--------------------------------------------------------------------------------

-- | Create a host of two shows, then find the URL of a section.
--
-- The prefix names the shows: @prefix-alpha@ and @prefix-beta@.
-- The titles put alpha before beta.
resolveForHost :: TestDBConfig -> Text -> ShowSection -> Maybe Text -> IO (Maybe String)
resolveForHost cfg prefix section mCookieText = do
  hostInsert <- mkUserInsert (prefix <> "-host") UserMetadata.Host
  let alphaInsert = Shows.Insert ("A " <> prefix) (Slug (prefix <> "-alpha")) Nothing Nothing Shows.Active
      betaInsert = Shows.Insert ("B " <> prefix) (Slug (prefix <> "-beta")) Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels hostInsert
      (alphaId, _) <- insertTestShowWithSchedule alphaInsert defaultScheduleInsert
      (betaId, _) <- insertTestShowWithSchedule betaInsert defaultScheduleInsert
      addTestShowHost alphaId (User.mId userModel)
      addTestShowHost betaId (User.mId userModel)
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- runExceptT $ action (Cookie <$> mCookieText) userMetaModel section
    case result of
      Left err -> liftIO $ expectationFailure ("Expected Right but got Left: " <> show err) >> pure Nothing
      Right mUri -> pure (fmap show mUri)

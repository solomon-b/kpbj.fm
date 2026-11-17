module App.Dashboard.CurrentShowSpec (spec) where

--------------------------------------------------------------------------------

import App.Dashboard.CurrentShow (resolveCurrentShow)
import Control.Monad.IO.Class (liftIO)
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
import Test.Hspec (Spec, describe, it, shouldBe)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "App.Dashboard.CurrentShow" $ do
      describe "resolveCurrentShow" $ do
        it "gives the second show when the cookie names it" test_cookieNamesSecondShow
        it "gives the first show by title when there is no cookie" test_noCookieGivesFirstShow
        it "gives the first show when the cookie names a show the host does not host" test_foreignCookieGivesFirstShow
        it "gives nothing to a user with no shows" test_noShowsGivesNothing
        it "gives an admin a show that the admin does not host" test_adminGetsUnhostedShow
        it "gives staff the show in the cookie when they do not host it" test_staffGetsUnhostedShow
        it "gives staff the first show when the cookie names no show" test_staffUnknownCookieGivesFirstShow
        it "gives an admin the inactive show in the cookie" test_adminGetsInactiveShow

--------------------------------------------------------------------------------

-- | Staff can open every show, so the cookie can name a show they do not host.
test_staffGetsUnhostedShow :: TestDBConfig -> IO ()
test_staffGetsUnhostedShow cfg =
  resolveForRole UserMetadata.Staff cfg "cs-staff" (Just "dashboard_show=cs-staff-other")
    >>= (`shouldBe` Just "cs-staff-other")

-- | A cookie for a slug that no show has gives staff their first show.
test_staffUnknownCookieGivesFirstShow :: TestDBConfig -> IO ()
test_staffUnknownCookieGivesFirstShow cfg =
  resolveForRole UserMetadata.Staff cfg "cs-staffnone" (Just "dashboard_show=cs-staffnone-absent")
    >>= (`shouldBe` Just "cs-staffnone-alpha")

-- | The show list of an admin has active shows only. The cookie can name an
-- inactive show.
test_adminGetsInactiveShow :: TestDBConfig -> IO ()
test_adminGetsInactiveShow cfg = do
  adminInsert <- mkUserInsert "cs-inactive" UserMetadata.Admin
  let showInsert = Shows.Insert "CS Inactive Show" "cs-inactive-show" Nothing Nothing Shows.Inactive

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_, userMetaModel) <- setupUserModels adminInsert
      _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- resolveCurrentShow (Just (Cookie "dashboard_show=cs-inactive-show")) userMetaModel
    liftIO $ fmap (.slug) result `shouldBe` Just (Slug "cs-inactive-show")

-- | A host of two shows, with the cookie for the second show.
test_cookieNamesSecondShow :: TestDBConfig -> IO ()
test_cookieNamesSecondShow cfg =
  resolveForHost cfg "cs-second" (Just "dashboard_show=cs-second-beta")
    >>= (`shouldBe` Just "cs-second-beta")

-- | The same host with no cookie gets the first show by title.
test_noCookieGivesFirstShow :: TestDBConfig -> IO ()
test_noCookieGivesFirstShow cfg =
  resolveForHost cfg "cs-nocookie" Nothing
    >>= (`shouldBe` Just "cs-nocookie-alpha")

-- | A cookie for a show that the host does not host is ignored.
test_foreignCookieGivesFirstShow :: TestDBConfig -> IO ()
test_foreignCookieGivesFirstShow cfg =
  resolveForHost cfg "cs-foreign" (Just "dashboard_show=cs-foreign-other")
    >>= (`shouldBe` Just "cs-foreign-alpha")

-- | A host with no shows gets nothing.
test_noShowsGivesNothing :: TestDBConfig -> IO ()
test_noShowsGivesNothing cfg = do
  hostInsert <- mkUserInsert "cs-noshows" UserMetadata.Host

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_, userMetaModel) <- setupUserModels hostInsert
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- resolveCurrentShow Nothing userMetaModel
    liftIO $ fmap (.slug) result `shouldBe` Nothing

-- | An admin gets any active show that the cookie names.
test_adminGetsUnhostedShow :: TestDBConfig -> IO ()
test_adminGetsUnhostedShow cfg = do
  adminInsert <- mkUserInsert "cs-admin" UserMetadata.Admin
  let showInsert = Shows.Insert "CS Admin Show" "cs-admin-show" Nothing Nothing Shows.Active

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (_, userMetaModel) <- setupUserModels adminInsert
      _ <- insertTestShowWithSchedule showInsert defaultScheduleInsert
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- resolveCurrentShow (Just (Cookie "dashboard_show=cs-admin-show")) userMetaModel
    liftIO $ fmap (.slug) result `shouldBe` Just (Slug "cs-admin-show")

--------------------------------------------------------------------------------

-- | 'resolveForRole' for a user with the host role.
resolveForHost :: TestDBConfig -> Text -> Maybe Text -> IO (Maybe Slug)
resolveForHost = resolveForRole UserMetadata.Host

-- | Create a user who hosts two shows, and one other show, then resolve the
-- current show.
--
-- The prefix names the shows: @prefix-alpha@, @prefix-beta@, and @prefix-other@.
-- The titles put alpha before beta. The user does not host the other show.
resolveForRole :: UserMetadata.UserRole -> TestDBConfig -> Text -> Maybe Text -> IO (Maybe Slug)
resolveForRole role cfg prefix mCookieText = do
  hostInsert <- mkUserInsert (prefix <> "-host") role
  let mkInsert suffix = Shows.Insert ("A " <> prefix <> " " <> suffix) (Slug (prefix <> "-" <> suffix)) Nothing Nothing Shows.Active
      alphaInsert = mkInsert "alpha"
      betaInsert = (mkInsert "beta") {Shows.siTitle = "B " <> prefix <> " beta"}
      otherInsert = mkInsert "other"

  bracketAppM cfg $ do
    dbResult <- runDB $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
      (userModel, userMetaModel) <- setupUserModels hostInsert
      (alphaId, _) <- insertTestShowWithSchedule alphaInsert defaultScheduleInsert
      (betaId, _) <- insertTestShowWithSchedule betaInsert defaultScheduleInsert
      _ <- insertTestShowWithSchedule otherInsert defaultScheduleInsert
      addTestShowHost alphaId (User.mId userModel)
      addTestShowHost betaId (User.mId userModel)
      pure userMetaModel
    userMetaModel <- expectSetupRight dbResult

    result <- resolveCurrentShow (Cookie <$> mCookieText) userMetaModel
    pure $ fmap (.slug) result

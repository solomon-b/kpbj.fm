{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Routing tests for the role gate of "App.Auth.Roles".
--
-- Each test serves a small API with an @'Admin@ gate and a @'Staff@ gate,
-- with the real auth handler and the real rejection handler in the Servant
-- context. It sends one request with a real session from the test database.
-- The request is an HTMX request, a page load, or a @fetch@ call.
module App.Auth.GateSpec (spec) where

--------------------------------------------------------------------------------

import API.Links (apiLinks, dashboardLinks, rootLink, userLinks)
import API.Types (DashboardRoutes (..), Routes (..), UserRoutes (..))
import App.Auth (sessionCookieName)
import App.Auth.Roles (RequireRole, RoleAuth (..), Satisfies (..), roleAuthHandler, roleRejectionHandler)
import App.Config (Environment (..))
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), flashCookie)
import Control.Exception (bracket)
import Control.Monad (void)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, addUTCTime, getCurrentTime)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Interpolate (getOneRow, interp, sql)
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as Hasql
import Hasql.Transaction qualified as TRX
import Hasql.Transaction.Sessions qualified as TRX
import Network.HTTP.Types (Header, statusCode)
import Network.Wai qualified as Wai
import Network.Wai.Test (SResponse (..), defaultRequest, request, runSession, setPath)
import Servant (Context (..), (:<|>) (..), (:>))
import Servant qualified
import Test.Database.Monad (TestDBConfig (..), noOpLoggerEnv, withTestDB)
import Test.Handler.Fixtures (expectSetupRight, mkUserInsert, setupUserModels)
import Test.Handler.Monad (acquireTestPool)
import Test.Hspec (Spec, describe, it, shouldBe, shouldThrow)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  withTestDB $
    describe "App.Auth.Gate" $ do
      it "sends a request with no cookie to the login page" test_noCookie
      it "sends a request with no session cookie to the login page" test_noSessionCookie
      it "sends an expired session to the login page" test_expiredSession
      it "sends a User to the home page" test_user
      it "sends a Host to the dashboard" test_host
      it "sends a Staff user to the dashboard" test_staff
      it "sends a suspended Admin to the dashboard" test_suspendedAdmin
      it "sends a suspended Host to the dashboard" test_suspendedHost
      it "sends a suspended User to the home page" test_suspendedUser
      it "answers a user with no metadata row with a 500" test_noMetadata
      it "lets a database error through as a 500 exception" test_databaseError
      it "lets an Admin through" test_admin
      it "lets a Staff user through a Staff gate" test_staffGateStaff
      it "lets an Admin through a Staff gate" test_staffGateAdmin
      it "sends a Host on a Staff gate to the dashboard" test_staffGateHost
      it "sends a page load with no cookie to the login page" test_browserNoCookie
      it "sends a page load by a Host to the dashboard" test_browserHost
      it "answers a fetch with no cookie with a 401" test_fetchNoCookie
      it "answers a fetch with an expired session with a 401" test_fetchExpiredSession
      it "answers a fetch by a Host with a 403" test_fetchHost
      it "answers a fetch by a suspended Admin with a 403" test_fetchSuspendedAdmin
      it "lets a fetch by an Admin through" test_fetchAdmin

--------------------------------------------------------------------------------
-- Tests

test_noCookie :: TestDBConfig -> IO ()
test_noCookie cfg = withPool cfg $ \pool -> do
  response <- probe pool []
  shouldBeRedirect response loginUrl loginFlash

test_noSessionCookie :: TestDBConfig -> IO ()
test_noSessionCookie cfg = withPool cfg $ \pool -> do
  response <- probe pool [("Cookie", "_flash=x")]
  shouldBeRedirect response loginUrl loginFlash

test_expiredSession :: TestDBConfig -> IO ()
test_expiredSession cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-expired" UserMetadata.Admin NotSuspended (-3600)
  response <- probe pool [cookie]
  shouldBeRedirect response loginUrl loginFlash

test_user :: TestDBConfig -> IO ()
test_user cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-user" UserMetadata.User NotSuspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink apiLinks.rootGet) (deniedFlash "You need Admin permissions to do this.")

test_host :: TestDBConfig -> IO ()
test_host cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-host" UserMetadata.Host NotSuspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "You need Admin permissions to do this.")

test_suspendedAdmin :: TestDBConfig -> IO ()
test_suspendedAdmin cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-suspended-admin" UserMetadata.Admin Suspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "Your account is suspended.")

test_staff :: TestDBConfig -> IO ()
test_staff cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-staff" UserMetadata.Staff NotSuspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "You need Admin permissions to do this.")

-- | The target comes from the stored role, so a suspended Host still goes to
-- the dashboard.
test_suspendedHost :: TestDBConfig -> IO ()
test_suspendedHost cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-suspended-host" UserMetadata.Host Suspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "Your account is suspended.")

test_suspendedUser :: TestDBConfig -> IO ()
test_suspendedUser cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-suspended-user" UserMetadata.User Suspended 3600
  response <- probe pool [cookie]
  shouldBeRedirect response (rootLink apiLinks.rootGet) (deniedFlash "Your account is suspended.")

-- | Every user has a metadata row, so a missing row is a data fault and not
-- an auth failure.
test_noMetadata :: TestDBConfig -> IO ()
test_noMetadata cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-no-metadata" UserMetadata.Admin NoMetadata 3600
  response <- probe pool [cookie]
  shouldBeStatus response 500

-- | @authHandlerFor@ raises a session query error as an exception, not as a
-- 'Servant.ServerError' value. It passes the login redirect, and Warp answers
-- it with a 500.
test_databaseError :: TestDBConfig -> IO ()
test_databaseError cfg =
  withPool cfg {testDBConfigPort = "1"} $ \pool ->
    probe pool [("Cookie", sessionCookieName Development <> "=00000000-0000-0000-0000-000000000000")]
      `shouldThrow` (\e -> Servant.errHTTPCode e == 500)

test_admin :: TestDBConfig -> IO ()
test_admin cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-admin" UserMetadata.Admin NotSuspended 3600
  response <- probe pool [cookie]
  statusCode (simpleStatus response) `shouldBe` 200
  simpleBody response `shouldBe` "Admin"

test_staffGateStaff :: TestDBConfig -> IO ()
test_staffGateStaff cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-staff-gate-staff" UserMetadata.Staff NotSuspended 3600
  response <- staffProbe pool [cookie]
  statusCode (simpleStatus response) `shouldBe` 200
  simpleBody response `shouldBe` "Staff"

-- | A gate admits every role above the one it requires.
test_staffGateAdmin :: TestDBConfig -> IO ()
test_staffGateAdmin cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-staff-gate-admin" UserMetadata.Admin NotSuspended 3600
  response <- staffProbe pool [cookie]
  statusCode (simpleStatus response) `shouldBe` 200
  simpleBody response `shouldBe` "Admin"

test_staffGateHost :: TestDBConfig -> IO ()
test_staffGateHost cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-staff-gate-host" UserMetadata.Host NotSuspended 3600
  response <- staffProbe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "You need Staff permissions to do this.")

test_browserNoCookie :: TestDBConfig -> IO ()
test_browserNoCookie cfg = withPool cfg $ \pool -> do
  response <- browserProbe pool []
  shouldBeRedirect response loginUrl loginFlash

test_browserHost :: TestDBConfig -> IO ()
test_browserHost cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-browser-host" UserMetadata.Host NotSuspended 3600
  response <- browserProbe pool [cookie]
  shouldBeRedirect response (rootLink dashboardLinks.home) (deniedFlash "You need Admin permissions to do this.")

test_fetchNoCookie :: TestDBConfig -> IO ()
test_fetchNoCookie cfg = withPool cfg $ \pool -> do
  response <- fetchProbe pool []
  shouldBeStatus response 401

test_fetchExpiredSession :: TestDBConfig -> IO ()
test_fetchExpiredSession cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-fetch-expired" UserMetadata.Admin NotSuspended (-3600)
  response <- fetchProbe pool [cookie]
  shouldBeStatus response 401

test_fetchHost :: TestDBConfig -> IO ()
test_fetchHost cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-fetch-host" UserMetadata.Host NotSuspended 3600
  response <- fetchProbe pool [cookie]
  shouldBeStatus response 403

test_fetchSuspendedAdmin :: TestDBConfig -> IO ()
test_fetchSuspendedAdmin cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-fetch-suspended-admin" UserMetadata.Admin Suspended 3600
  response <- fetchProbe pool [cookie]
  shouldBeStatus response 403

test_fetchAdmin :: TestDBConfig -> IO ()
test_fetchAdmin cfg = withPool cfg $ \pool -> do
  cookie <- userWithSession pool "gate-fetch-admin" UserMetadata.Admin NotSuspended 3600
  response <- fetchProbe pool [cookie]
  statusCode (simpleStatus response) `shouldBe` 200
  simpleBody response `shouldBe` "Admin"

--------------------------------------------------------------------------------
-- The probe API

-- | One route behind an @'Admin@ gate and one behind a @'Staff@ gate. Each
-- answers with the stored role of the user.
type ProbeAPI =
  RequireRole "cookie-auth" 'UserMetadata.Admin :> "probe" :> Servant.Get '[Servant.PlainText] Text
    :<|> RequireRole "cookie-auth" 'UserMetadata.Staff :> "staff-probe" :> Servant.Get '[Servant.PlainText] Text

probeServer :: Servant.Server ProbeAPI
probeServer =
  (\(Satisfies _ auth) -> storedRole auth)
    :<|> (\(Satisfies _ auth) -> storedRole auth)

storedRole :: RoleAuth r -> Servant.Handler Text
storedRole auth = pure (display auth.raMetadata.mUserRole)

-- | Send @GET /probe@ as an HTMX request with the given headers, through the
-- real @'Admin@ gate.
probe :: HSQL.Pool -> [Header] -> IO SResponse
probe pool headers = probePath pool "/probe" (htmxHeader : headers)

-- | Send @GET /staff-probe@ as an HTMX request with the given headers, through
-- the real @'Staff@ gate.
staffProbe :: HSQL.Pool -> [Header] -> IO SResponse
staffProbe pool headers = probePath pool "/staff-probe" (htmxHeader : headers)

-- | Send @GET /probe@ as a @fetch@ call with the default headers of the
-- browser, through the real @'Admin@ gate.
fetchProbe :: HSQL.Pool -> [Header] -> IO SResponse
fetchProbe pool headers = probePath pool "/probe" (("Accept", "*/*") : headers)

-- | Send @GET /probe@ as a page load in a browser, through the real @'Admin@
-- gate.
browserProbe :: HSQL.Pool -> [Header] -> IO SResponse
browserProbe pool headers = probePath pool "/probe" (("Accept", "text/html,application/xhtml+xml,*/*;q=0.8") : headers)

htmxHeader :: Header
htmxHeader = ("HX-Request", "true")

probePath :: HSQL.Pool -> ByteString -> [Header] -> IO SResponse
probePath pool path headers =
  runSession (request (setPath defaultRequest path) {Wai.requestHeaders = headers}) app
  where
    app =
      Servant.serveWithContext
        (Servant.Proxy @ProbeAPI)
        ( roleAuthHandler noOpLoggerEnv pool Development
            :. roleRejectionHandler noOpLoggerEnv
            :. Servant.EmptyContext
        )
        probeServer

--------------------------------------------------------------------------------
-- Helpers

-- | The state of the user that 'userWithSession' makes.
data Suspension = Suspended | NotSuspended | NoMetadata

withPool :: TestDBConfig -> (HSQL.Pool -> IO a) -> IO a
withPool cfg = bracket (acquireTestPool cfg) HSQL.release

-- | Make a user with a session that expires the given number of seconds from
-- now, and return the cookie header for that session.
userWithSession :: HSQL.Pool -> Text -> UserMetadata.UserRole -> Suspension -> NominalDiffTime -> IO Header
userWithSession pool emailPrefix role suspension expiresIn = do
  userInsert <- mkUserInsert emailPrefix role
  now <- getCurrentTime
  result <- HSQL.use pool $ TRX.transaction TRX.ReadCommitted TRX.Write $ do
    (user, _) <- setupUserModels userInsert
    case suspension of
      Suspended -> void $ TRX.statement () (UserMetadata.suspendUser user.mId "test suspension")
      NotSuspended -> pure ()
      NoMetadata -> TRX.statement () (deleteMetadata user.mId)
    getOneRow
      <$> TRX.statement
        ()
        ( ServerSessions.insertServerSession
            ServerSessions.ServerSessionInsert
              { ssiUserId = user.mId,
                ssiIpAddress = Nothing,
                ssiUserAgent = Nothing,
                ssiExpiresAt = addUTCTime expiresIn now
              }
        )
  session <- expectSetupRight result
  pure ("Cookie", sessionCookieName Development <> "=" <> encodeUtf8 (display session.mSessionId))

deleteMetadata :: User.Id -> Hasql.Statement () ()
deleteMetadata userId = interp False [sql| DELETE FROM user_metadata WHERE user_id = #{userId} |]

shouldBeRedirect :: SResponse -> Text -> FlashMessage -> IO ()
shouldBeRedirect response url flash = do
  statusCode (simpleStatus response) `shouldBe` 200
  lookup "HX-Redirect" (simpleHeaders response) `shouldBe` Just (encodeUtf8 url)
  lookup "Set-Cookie" (simpleHeaders response) `shouldBe` Just (encodeUtf8 (flashCookie (Just flash)))

-- | A plain error status with no redirect and no flash.
shouldBeStatus :: SResponse -> Int -> IO ()
shouldBeStatus response code = do
  statusCode (simpleStatus response) `shouldBe` code
  lookup "HX-Redirect" (simpleHeaders response) `shouldBe` Nothing
  lookup "Set-Cookie" (simpleHeaders response) `shouldBe` Nothing

loginUrl :: Text
loginUrl = rootLink (userLinks.loginGet Nothing Nothing)

loginFlash :: FlashMessage
loginFlash = FlashMessage Error "Login Required" "Please log in to continue."

deniedFlash :: Text -> FlashMessage
deniedFlash = FlashMessage Error "Access Denied"

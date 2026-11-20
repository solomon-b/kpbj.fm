-- The package defaults to Haskell2010, and the code that the splice below
-- generates needs more than that. @servant-auth-roles@ is written against
-- GHC2021, which supplies most of these extensions.
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
-- singletons generates defunctionalization symbols that this module does not
-- use.
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- | Role gates for Servant routes, built on @servant-auth-roles@.
--
-- A route states the role that it needs in its type:
--
-- @
-- type Route =
--   "dashboard" :> "stream-settings" :> "stop"
--     :> RequireRole "cookie-auth" 'Admin
--     :> Servant.Post '[HTML] (Lucid.Html ())
-- @
--
-- The handler receives a 'Satisfies' in place of a @Maybe Cookie@. It holds
-- the authenticated user and a proof that the role is high enough.
--
-- 'UserMetadata.UserRole' is declared as @User | Host | Staff | Admin@, in
-- that order. 'deriveOrdRole' reads the declaration order as the hierarchy,
-- so a gate on @'Staff@ admits Staff and Admin. The generated instances are
-- orphans. Thus kpbj-database does not depend on @servant-auth-roles@.
module App.Auth.Roles
  ( -- * Role gates
    RequireRole,
    Satisfies (..),
    SomeRole (..),

    -- * Auth value
    RoleAuth (..),
    effectiveRole,

    -- * Servant context entries
    roleAuthHandler,
    roleRejectionHandler,

    -- * Failure responses
    toLoginRedirect,
    rejectionError,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, rootLink)
import API.Types (Routes (..))
import App.Auth (CookieAuth, SessionConfig (..), authHandlerFor, defaultSessionConfig)
import App.Config (Environment)
import App.Handler.Error (HandlerError (..), errorRedirectParams, notAuthorizedRedirect)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..), hxRedirectError)
import Control.Monad.Except (catchError, throwError)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Bifunctor (second)
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (getCurrentTime)
import Effects.Database.Tables.ServerSessions qualified as ServerSessions
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Log qualified
import Network.Wai (Request)
import Network.Wai qualified as Wai
import Servant qualified
-- Imported with no import list. The splice below generates code against the
-- singletons Prelude that this module re-exports, and a shorter list starves
-- it.
import Servant.Auth.Roles.TH
import Servant.Server.Experimental.Auth (AuthHandler, AuthServerData, mkAuthHandler, unAuthHandler)

--------------------------------------------------------------------------------

$(deriveOrdRole ''UserMetadata.UserRole)

-- | The authenticated user, indexed by the role that the auth handler found.
--
-- A gate gives this value to the handler inside a 'Satisfies'.
data RoleAuth (r :: UserMetadata.UserRole) = RoleAuth
  { raUser :: User.Model,
    raSession :: ServerSessions.Model,
    raMetadata :: UserMetadata.Model
  }

type instance AuthServerData CookieAuth = SomeRole RoleAuth

-- | The role that a gate checks. A suspended user has the role
-- 'UserMetadata.User', so every gate above 'UserMetadata.User' refuses them.
effectiveRole :: UserMetadata.Model -> UserMetadata.UserRole
effectiveRole metadata
  | UserMetadata.isSuspended metadata = UserMetadata.User
  | otherwise = metadata.mUserRole

--------------------------------------------------------------------------------

-- | Authenticate a request and put the role of the user into the type index.
--
-- The session checks are the ones of 'authHandlerFor'. The session has no
-- idle timeout. It ends at its @expires_at@ time. An auth failure becomes the
-- login redirect (see 'toLoginRedirect').
roleAuthHandler ::
  Log.LoggerEnv ->
  HSQL.Pool ->
  -- | Selects the name of the session cookie
  Environment ->
  AuthHandler Request (SomeRole RoleAuth)
roleAuthHandler logEnv pool env =
  mkAuthHandler $ \request ->
    unAuthHandler inner request `catchError` (throwError . toLoginRedirect)
  where
    inner = authHandlerFor withMetadata pool env defaultSessionConfig {sessionIdleTimeout = Nothing}

    withMetadata user session =
      liftIO (HSQL.use pool (HSQL.statement () (UserMetadata.getUserMetadata user.mId))) >>= \case
        Right (Just metadata) ->
          pure $ someUserRole (effectiveRole metadata) (RoleAuth user session metadata)
        result -> do
          -- A user always has a metadata row, so this is a database fault.
          liftIO $ do
            now <- getCurrentTime
            Log.logMessageIO logEnv now Log.LogAttention "Role auth: no user metadata" $
              Aeson.object
                [ "user_id" .= display user.mId,
                  "error" .= either show (const "no row") result
                ]
          throwError Servant.err401

-- | Answer a user whose role does not satisfy a gate with 'rejectionError',
-- and write one log line.
roleRejectionHandler :: Log.LoggerEnv -> RejectionHandler UserMetadata.UserRole RoleAuth
roleRejectionHandler logEnv =
  RejectionHandler $ \request required (SomeRole _ auth) -> do
    now <- getCurrentTime
    Log.logMessageIO logEnv now Log.LogInfo "Role gate refused request" $
      Aeson.object
        [ "user_id" .= display auth.raUser.mId,
          "stored_role" .= display auth.raMetadata.mUserRole,
          "required_role" .= display required,
          "path" .= Text.Encoding.decodeUtf8Lenient (Wai.rawPathInfo request)
        ]
    pure (rejectionError required auth.raMetadata)

--------------------------------------------------------------------------------

-- | Change an auth failure into the login redirect with the "Login Required"
-- flash.
--
-- The auth handler of @web-server-core@ fails with a 307 when the cookie is
-- absent or not valid, and with a 401 when the session is absent or expired.
-- Any other error does not change.
--
-- A database error in the session query does not come here.
-- @authHandlerFor@ raises it as an exception, so it passes the 'catchError'
-- in 'roleAuthHandler', and Warp answers it with a 500.
toLoginRedirect :: Servant.ServerError -> Servant.ServerError
toLoginRedirect err
  | Servant.errHTTPCode err `elem` [401, 307] =
      uncurry hxRedirectError (second Just (errorRedirectParams apiLinks.rootGet NotAuthenticated))
  | otherwise = err

-- | The response for a user whose role does not satisfy a gate.
--
-- The target comes from the stored role, not from 'effectiveRole'. Thus a
-- suspended host goes to the dashboard home, as the handlers send them today.
rejectionError ::
  -- | The role that the gate requires
  UserMetadata.UserRole ->
  -- | The metadata of the user
  UserMetadata.Model ->
  Servant.ServerError
rejectionError required metadata =
  hxRedirectError
    (rootLink (notAuthorizedRedirect (Just metadata.mUserRole)))
    (Just (FlashMessage Error "Access Denied" message))
  where
    message
      | UserMetadata.isSuspended metadata = "Your account is suspended."
      | otherwise = "You need " <> display required <> " permissions to do this."

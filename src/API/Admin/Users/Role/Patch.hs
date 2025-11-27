{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Users.Role.Patch where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminUserRolePatchLink)
import App.Common (getUserInfo)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxPatch_, hxSwap_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)
import Web.FormUrlEncoded (FromForm)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "PATCH /admin/users/:id/role"
    ( "admin"
        :> "users"
        :> Servant.Capture "id" User.Id
        :> "role"
        :> Servant.Header "Cookie" Cookie
        :> Servant.ReqBody '[Servant.FormUrlEncoded] RoleUpdateForm
        :> Servant.Patch '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

newtype RoleUpdateForm = RoleUpdateForm
  { role :: UserMetadata.UserRole
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (FromForm)

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  User.Id ->
  Maybe Cookie ->
  RoleUpdateForm ->
  m (Lucid.Html ())
handler _tracer targetUserId cookie (RoleUpdateForm newRole) = do
  getUserInfo cookie >>= \case
    Nothing -> pure unauthorizedResponse
    Just (_user, userMetadata) ->
      if not (UserMetadata.isAdmin userMetadata.mUserRole)
        then pure unauthorizedResponse
        else
          execQuerySpan (UserMetadata.updateUserRole targetUserId newRole) >>= \case
            Left _err -> do
              Log.logInfo "Failed to update user role" ()
              pure $ errorResponse targetUserId newRole "Failed to update role"
            Right Nothing -> do
              Log.logInfo "User not found for role update" ()
              pure $ errorResponse targetUserId newRole "User not found"
            Right (Just _) -> do
              Log.logInfo "User role updated successfully" ()
              pure $ successResponse targetUserId newRole

--------------------------------------------------------------------------------
-- Response Templates

successResponse :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
successResponse userId newRole =
  Lucid.div_
    [ Lucid.id_ "role-dropdown-container",
      Lucid.class_ "flex items-center gap-3"
    ]
    $ do
      renderRoleDropdown userId newRole
      Lucid.span_
        [ Lucid.class_ "text-green-600 text-sm font-bold"
        ]
        "Saved"

errorResponse :: User.Id -> UserMetadata.UserRole -> Text -> Lucid.Html ()
errorResponse userId currentRole msg =
  Lucid.div_
    [ Lucid.id_ "role-dropdown-container",
      Lucid.class_ "flex items-center gap-3"
    ]
    $ do
      renderRoleDropdown userId currentRole
      Lucid.span_
        [ Lucid.class_ "text-red-600 text-sm font-bold"
        ]
        $ Lucid.toHtml msg

unauthorizedResponse :: Lucid.Html ()
unauthorizedResponse =
  Lucid.div_
    [ Lucid.id_ "role-dropdown-container"
    ]
    $ Lucid.span_
      [ Lucid.class_ "text-red-600 text-sm font-bold"
      ]
      "Unauthorized"

renderRoleDropdown :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleDropdown userId currentRole =
  Lucid.select_
    [ Lucid.name_ "role",
      Lucid.class_ "p-2 border-2 border-gray-800 bg-white font-bold",
      hxPatch_ [i|/#{rolePatchUrl}|],
      hxTarget_ "#role-dropdown-container",
      hxSwap_ "outerHTML"
    ]
    $ do
      roleOption UserMetadata.User currentRole
      roleOption UserMetadata.Host currentRole
      roleOption UserMetadata.Staff currentRole
      roleOption UserMetadata.Admin currentRole
  where
    rolePatchUrl = Links.linkURI $ adminUserRolePatchLink userId

    roleOption :: UserMetadata.UserRole -> UserMetadata.UserRole -> Lucid.Html ()
    roleOption role selected =
      Lucid.option_
        ( [Lucid.value_ (display role)]
            <> [Lucid.selected_ "selected" | role == selected]
        )
        $ Lucid.toHtml (display role)

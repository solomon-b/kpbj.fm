{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Role.Patch.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Role.Patch.Route (RoleUpdateForm (..))
import API.Links (dashboardUsersLinks)
import API.Types (DashboardUsersRoutes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (HandlerError, handleBannerErrors)
import App.Monad (AppM)
import Control.Monad.Trans.Except (ExceptT)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Result of a role update attempt.
data RoleUpdateResult
  = RoleUpdateSuccess UserMetadata.UserRole
  | RoleUpdateNotFound
  | RoleUpdateFailed

-- | Business logic: update role.
action ::
  User.Id ->
  UserMetadata.UserRole ->
  ExceptT HandlerError AppM RoleUpdateResult
action targetUserId newRole = do
  execQuery (UserMetadata.updateUserRole targetUserId newRole) >>= \case
    Left _err -> do
      Log.logInfo "Failed to update user role" ()
      pure RoleUpdateFailed
    Right Nothing -> do
      Log.logInfo "User not found for role update" ()
      pure RoleUpdateNotFound
    Right (Just _) -> do
      Log.logInfo "User role updated successfully" ()
      pure $ RoleUpdateSuccess newRole

handler ::
  User.Id ->
  Maybe Cookie ->
  RoleUpdateForm ->
  AppM (Lucid.Html ())
handler targetUserId cookie (RoleUpdateForm newRole) =
  handleBannerErrors "Role update" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can change user roles." userMetadata
    result <- action targetUserId newRole
    pure $ case result of
      RoleUpdateSuccess updatedRole -> successResponse targetUserId updatedRole
      RoleUpdateNotFound -> errorResponse targetUserId newRole "User not found"
      RoleUpdateFailed -> errorResponse targetUserId newRole "Failed to update role"

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
        [ class_ $ base [Tokens.errorText, Tokens.textSm, Tokens.fontBold]
        ]
        $ Lucid.toHtml msg

renderRoleDropdown :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleDropdown userId currentRole =
  Lucid.select_
    [ Lucid.name_ "role",
      class_ $ base [Tokens.p2, Tokens.border2, Tokens.borderDefault, Tokens.bgMain, Tokens.fgPrimary, Tokens.fontBold],
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
    rolePatchUrl = Links.linkURI $ dashboardUsersLinks.rolePatch userId

    roleOption :: UserMetadata.UserRole -> UserMetadata.UserRole -> Lucid.Html ()
    roleOption role selected =
      Lucid.option_
        ( [Lucid.value_ (display role)]
            <> [Lucid.selected_ "selected" | role == selected]
        )
        $ Lucid.toHtml (display role)

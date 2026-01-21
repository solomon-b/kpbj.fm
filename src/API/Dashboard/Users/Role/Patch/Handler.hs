{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Users.Role.Patch.Handler where

--------------------------------------------------------------------------------

import API.Dashboard.Users.Role.Patch.Route (RoleUpdateForm (..))
import API.Links (dashboardUsersLinks)
import API.Types (DashboardUsersRoutes (..))
import App.Handler.Combinators (requireAdminNotSuspended, requireAuth)
import App.Handler.Error (handleBannerErrors)
import App.Monad (AppM)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (hxPatch_, hxSwap_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  User.Id ->
  Maybe Cookie ->
  RoleUpdateForm ->
  AppM (Lucid.Html ())
handler _tracer targetUserId cookie (RoleUpdateForm newRole) =
  handleBannerErrors "Role update" $ do
    -- Require admin authentication
    (_user, userMetadata) <- requireAuth cookie
    requireAdminNotSuspended "Only admins can change user roles." userMetadata

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

renderRoleDropdown :: User.Id -> UserMetadata.UserRole -> Lucid.Html ()
renderRoleDropdown userId currentRole =
  Lucid.select_
    [ Lucid.name_ "role",
      Lucid.class_ "p-2 border-2 border-gray-800 bg-white dark:bg-gray-800 font-bold",
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

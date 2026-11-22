module API.Dashboard.Users.Edit.Shared
  ( CanChangeRole,
    checkCanChangeRole,
  )
where

--------------------------------------------------------------------------------

import App.Auth.Roles (RoleAuth, Satisfies, satisfies)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata

--------------------------------------------------------------------------------

-- | Permission to change the role of a user.
--
-- The edit routes admit Staff, but only an Admin can change a role. The
-- constructor is not exported. Only 'checkCanChangeRole' makes this value.
data CanChangeRole = CanChangeRole

-- | Give the permission when the caller of a Staff route is an Admin.
checkCanChangeRole :: Satisfies 'UserMetadata.Staff RoleAuth -> Maybe CanChangeRole
checkCanChangeRole auth = CanChangeRole <$ satisfies @'UserMetadata.Admin auth

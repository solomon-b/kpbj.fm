{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Minimal rel8 schemas for querying host emails.
--
-- These are intentionally slim — only the columns needed for the sync query.
module Database
  ( -- * Query
    fetchHostEmails,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Statement qualified as Hasql
import Rel8

--------------------------------------------------------------------------------
-- Minimal table schemas

-- | Minimal view of the @users@ table.
data Users f = Users
  { userId :: Column f Int64,
    userEmail :: Column f Text,
    userDeletedAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

usersSchema :: TableSchema (Users Name)
usersSchema =
  TableSchema
    { name = "users",
      columns =
        Users
          { userId = "id",
            userEmail = "email",
            userDeletedAt = "deleted_at"
          }
    }

-- | Minimal view of the @user_metadata@ table.
data UserMetadata f = UserMetadata
  { umUserId :: Column f Int64,
    umUserRole :: Column f Text
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

userMetadataSchema :: TableSchema (UserMetadata Name)
userMetadataSchema =
  TableSchema
    { name = "user_metadata",
      columns =
        UserMetadata
          { umUserId = "user_id",
            umUserRole = "user_role"
          }
    }

-- | Minimal view of the @show_hosts@ table.
data ShowHosts f = ShowHosts
  { shUserId :: Column f Int64,
    shLeftAt :: Column f (Maybe UTCTime)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

showHostsSchema :: TableSchema (ShowHosts Name)
showHostsSchema =
  TableSchema
    { name = "show_hosts",
      columns =
        ShowHosts
          { shUserId = "user_id",
            shLeftAt = "left_at"
          }
    }

--------------------------------------------------------------------------------
-- Query

-- | Fetch distinct emails for all users considered "hosts".
--
-- A user is a host if:
--
-- - They have @user_role = 'Host'@ in @user_metadata@, OR
-- - They are actively assigned to a show via @show_hosts@ (where @left_at IS NULL@).
--
-- Only non-deleted users are included.
fetchHostEmails :: Hasql.Statement () [Text]
fetchHostEmails = run $ select hostEmailsQuery

-- | rel8 query: distinct emails of active hosts.
hostEmailsQuery :: Query (Expr Text)
hostEmailsQuery = distinct do
  u <- each usersSchema
  where_ $ isNull (userDeletedAt u)
  hasRole <- exists (hasHostRole u)
  hasShow <- exists (hasShowAssignment u)
  where_ $ hasRole ||. hasShow
  pure (userEmail u)

-- | Subquery: user has role = 'Host' in user_metadata.
hasHostRole :: Users Expr -> Query ()
hasHostRole u = do
  um <- each userMetadataSchema
  where_ $ umUserId um ==. userId u
  where_ $ umUserRole um ==. lit "Host"

-- | Subquery: user is actively assigned to a show.
hasShowAssignment :: Users Expr -> Query ()
hasShowAssignment u = do
  sh <- each showHostsSchema
  where_ $ shUserId sh ==. userId u
  where_ $ isNull (shLeftAt sh)

{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan rel8 instances for external types.
--
-- This module provides 'DBType' and 'DBEq' instances for types from
-- external packages (like web-server-core) that need to be used in
-- rel8 queries.
module OrphanInstances.Rel8 () where

--------------------------------------------------------------------------------

import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Effects.Database.Tables.User qualified as User
import Rel8 (DBEq, DBType (..), parseTypeInformation)

--------------------------------------------------------------------------------

-- | DBType instance for User.Id from web-server-core.
deriving newtype instance DBType User.Id

-- | DBEq instance for User.Id from web-server-core.
deriving newtype instance DBEq User.Id

--------------------------------------------------------------------------------

-- | DBType instance for DisplayName from web-server-core.
--
-- Converts to/from Text for database storage.
instance DBType DisplayName where
  typeInformation =
    parseTypeInformation
      ( \t -> case DisplayName.mkDisplayName t of
          Nothing -> Left $ "Invalid DisplayName: " <> Text.unpack t
          Just dn -> Right dn
      )
      display
      typeInformation

-- | DBEq instance for DisplayName from web-server-core.
instance DBEq DisplayName

--------------------------------------------------------------------------------

-- | DBType instance for FullName from web-server-core.
--
-- Converts to/from Text for database storage.
instance DBType FullName where
  typeInformation =
    parseTypeInformation
      ( \t -> case FullName.mkFullName t of
          Nothing -> Left $ "Invalid FullName: " <> Text.unpack t
          Just fn -> Right fn
      )
      display
      typeInformation

-- | DBEq instance for FullName from web-server-core.
instance DBEq FullName

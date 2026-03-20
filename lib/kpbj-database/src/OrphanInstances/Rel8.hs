{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- | Orphan instances for external types used in database modules.
--
-- This module provides 'DBType' and 'DBEq' instances for types from
-- external packages (like web-server-core) that need to be used in
-- rel8 queries, and 'DecodeValue' / 'EncodeValue' instances for types
-- used with hasql-interpolate raw SQL queries.
module OrphanInstances.Rel8 () where

--------------------------------------------------------------------------------

import Data.Aeson (Value)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Domain.Types.DisplayName (DisplayName)
import Domain.Types.DisplayName qualified as DisplayName
import Domain.Types.FullName (FullName)
import Domain.Types.FullName qualified as FullName
import Effects.Database.Tables.User qualified as User
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))
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

--------------------------------------------------------------------------------

-- | DecodeValue instance for Aeson Value (JSONB columns).
--
-- Allows raw SQL queries via hasql-interpolate to decode JSONB columns
-- directly into Aeson Values.
instance DecodeValue Value where
  decodeValue = Decoders.jsonb

-- | EncodeValue instance for Aeson Value (JSONB columns).
--
-- Allows raw SQL queries via hasql-interpolate to encode Aeson Values
-- as JSONB parameters.
instance EncodeValue Value where
  encodeValue = Encoders.jsonb

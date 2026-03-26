{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

-- | Database table definition and queries for @store_settings@.
--
-- This is a single-row table (id = 1). Uses hasql-interpolate for all queries
-- since Rel8 update/insert patterns don't work cleanly for singleton tables.
module Effects.Database.Tables.StoreSettings
  ( -- * Table Definition
    StoreSetting (..),
    storeSettingSchema,

    -- * Model (Result alias)
    Model,

    -- * Update Type
    UpdateSettings (..),

    -- * Queries
    getSettings,
    updateSettings,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Scientific (Scientific)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import Rel8

--------------------------------------------------------------------------------
-- Table Definition

-- | The @store_settings@ table definition using rel8's higher-kinded data pattern.
--
-- The type parameter @f@ determines the context:
--
-- - @Expr@: SQL expressions for building queries
-- - @Result@: Decoded Haskell values from query results
-- - @Name@: Column names for schema definition
--
-- Note: @ssId@ is a plain @Int64@ (not a newtype) because this is a single-row
-- table with a sentinel id = 1.
data StoreSetting f = StoreSetting
  { ssId :: Column f Int64,
    ssTaxRate :: Column f Scientific,
    ssShipFromName :: Column f Text,
    ssShipFromAddressLine1 :: Column f Text,
    ssShipFromCity :: Column f Text,
    ssShipFromState :: Column f Text,
    ssShipFromZip :: Column f Text,
    ssShipFromCountry :: Column f Text,
    ssOrderNotificationEmail :: Column f Text,
    ssUpdatedAt :: Column f UTCTime
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (StoreSetting f)

deriving stock instance (f ~ Result) => Eq (StoreSetting f)

-- | DecodeRow instance for hasql-interpolate raw SQL compatibility.
instance DecodeRow (StoreSetting Result)


-- | Display instance for StoreSetting Result.
instance Display (StoreSetting Result) where
  displayBuilder settings =
    "StoreSetting { id = "
      <> displayBuilder settings.ssId
      <> " }"

-- | Type alias for backwards compatibility.
--
-- @Model@ is the same as @StoreSetting Result@.
type Model = StoreSetting Result

-- | Table schema connecting the Haskell type to the database table.
storeSettingSchema :: TableSchema (StoreSetting Name)
storeSettingSchema =
  TableSchema
    { name = "store_settings",
      columns =
        StoreSetting
          { ssId = "id",
            ssTaxRate = "tax_rate",
            ssShipFromName = "ship_from_name",
            ssShipFromAddressLine1 = "ship_from_address_line1",
            ssShipFromCity = "ship_from_city",
            ssShipFromState = "ship_from_state",
            ssShipFromZip = "ship_from_zip",
            ssShipFromCountry = "ship_from_country",
            ssOrderNotificationEmail = "order_notification_email",
            ssUpdatedAt = "updated_at"
          }
    }

--------------------------------------------------------------------------------
-- Update Type

-- | Parameters for updating store settings.
data UpdateSettings = UpdateSettings
  { usTaxRate :: Scientific,
    usShipFromName :: Text,
    usShipFromAddressLine1 :: Text,
    usShipFromCity :: Text,
    usShipFromState :: Text,
    usShipFromZip :: Text,
    usShipFromCountry :: Text,
    usOrderNotificationEmail :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get the single settings row.
getSettings :: Hasql.Statement () (Maybe Model)
getSettings =
  interp
    False
    [sql|
      SELECT
        id,
        tax_rate,
        ship_from_name,
        ship_from_address_line1,
        ship_from_city,
        ship_from_state,
        ship_from_zip,
        ship_from_country,
        order_notification_email,
        updated_at
      FROM store_settings
      WHERE id = 1
    |]

-- | Update all mutable settings fields.
--
-- Always operates on the single row (id = 1). The @updated_at@ timestamp is
-- set to @now()@ automatically.
updateSettings :: UpdateSettings -> Hasql.Statement () ()
updateSettings UpdateSettings {..} =
  interp
    True
    [sql|
      UPDATE store_settings SET
        tax_rate = #{usTaxRate},
        ship_from_name = #{usShipFromName},
        ship_from_address_line1 = #{usShipFromAddressLine1},
        ship_from_city = #{usShipFromCity},
        ship_from_state = #{usShipFromState},
        ship_from_zip = #{usShipFromZip},
        ship_from_country = #{usShipFromCountry},
        order_notification_email = #{usOrderNotificationEmail},
        updated_at = now()
      WHERE id = 1
    |]

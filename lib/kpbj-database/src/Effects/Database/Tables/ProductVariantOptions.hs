{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | Database queries for @product_variant_options@.
--
-- This is a join table with a composite primary key (variant_id, option_value_id).
-- All queries use hasql-interpolate since Rel8 doesn't handle composite PKs cleanly.
module Effects.Database.Tables.ProductVariantOptions
  ( -- * Result Type
    VariantOption (..),

    -- * Queries
    insertVariantOption,
    getByVariantId,
    getByProductId,
    deleteByVariantId,
  )
where

--------------------------------------------------------------------------------

import Data.Text.Display (Display (..))
import Effects.Database.Tables.ProductOptionValues qualified as ProductOptionValues
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow, interp, sql)
import Hasql.Statement qualified as Hasql

--------------------------------------------------------------------------------
-- Result Type

-- | A single row from @product_variant_options@.
data VariantOption = VariantOption
  { voVariantId :: ProductVariants.Id,
    voOptionValueId :: ProductOptionValues.Id
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (DecodeRow)


-- | Display instance for logging.
instance Display VariantOption where
  displayBuilder vo =
    "VariantOption { variant_id = "
      <> displayBuilder vo.voVariantId
      <> ", option_value_id = "
      <> displayBuilder vo.voOptionValueId
      <> " }"

--------------------------------------------------------------------------------
-- Queries

-- | Insert a new variant-option association.
insertVariantOption ::
  ProductVariants.Id ->
  ProductOptionValues.Id ->
  Hasql.Statement () ()
insertVariantOption variantId optionValueId =
  interp
    True
    [sql|
      INSERT INTO product_variant_options (variant_id, option_value_id)
      VALUES (#{variantId}, #{optionValueId})
      ON CONFLICT DO NOTHING
    |]

-- | Get all option value IDs for a given variant.
getByVariantId :: ProductVariants.Id -> Hasql.Statement () [ProductOptionValues.Id]
getByVariantId variantId =
  interp
    False
    [sql|
      SELECT option_value_id
      FROM product_variant_options
      WHERE variant_id = #{variantId}
    |]

-- | Get all variant-option rows for all active variants of a product.
--
-- Joins through @product_variants@ to filter by @product_id@ and exclude
-- soft-deleted variants.
getByProductId :: Products.Id -> Hasql.Statement () [VariantOption]
getByProductId productId =
  interp
    False
    [sql|
      SELECT pvo.variant_id, pvo.option_value_id
      FROM product_variant_options pvo
      JOIN product_variants pv ON pv.id = pvo.variant_id
      WHERE pv.product_id = #{productId}
        AND pv.deleted_at IS NULL
    |]


-- | Delete all option associations for a variant.
deleteByVariantId :: ProductVariants.Id -> Hasql.Statement () ()
deleteByVariantId variantId =
  interp
    True
    [sql|
      DELETE FROM product_variant_options
      WHERE variant_id = #{variantId}
    |]

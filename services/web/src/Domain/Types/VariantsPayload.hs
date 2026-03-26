module Domain.Types.VariantsPayload
  ( VariantsPayload (..),
    OptionPayload (..),
    VariantPayload (..),
  )
where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON (..), withObject, (.:), (.:?))
import Data.Int (Int64)
import Data.Text (Text)
import Domain.Types.Cents (Cents)
import Effects.Database.Tables.ProductVariants qualified as ProductVariants
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Top-level payload for options + variants JSON from the edit form.
data VariantsPayload = VariantsPayload
  { vpOptions :: [OptionPayload],
    vpVariants :: [VariantPayload],
    vpDeletedVariantIds :: [ProductVariants.Id]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON VariantsPayload where
  parseJSON = withObject "VariantsPayload" $ \o ->
    VariantsPayload
      <$> o .: "options"
      <*> o .: "variants"
      <*> o .: "deleted_variant_ids"

-- | A single option type with its values.
data OptionPayload = OptionPayload
  { opName :: Text,
    opValues :: [Text]
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON OptionPayload where
  parseJSON = withObject "OptionPayload" $ \o ->
    OptionPayload
      <$> o .: "name"
      <*> o .: "values"

-- | A single variant with optional overrides.
data VariantPayload = VariantPayload
  { vplId :: Maybe ProductVariants.Id,
    vplOptionValues :: [Text],
    vplPriceCents :: Maybe Cents,
    vplInventoryCount :: Int64,
    vplSku :: Maybe Text,
    vplWeightOz :: Maybe Int64
  }
  deriving stock (Generic, Show, Eq)

instance FromJSON VariantPayload where
  parseJSON = withObject "VariantPayload" $ \o ->
    VariantPayload
      <$> o .:? "id"
      <*> o .: "option_values"
      <*> o .:? "price_cents"
      <*> o .: "inventory_count"
      <*> o .:? "sku"
      <*> o .:? "weight_oz"

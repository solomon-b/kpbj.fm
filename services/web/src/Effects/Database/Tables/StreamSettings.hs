{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE StandaloneDeriving #-}

-- | Database table definition and queries for @stream_settings@.
--
-- Singleton table storing the Icecast stream URL and metadata URL used by
-- the web player. Only one row can exist (enforced by CHECK constraint).
module Effects.Database.Tables.StreamSettings
  ( -- * Table Definition
    StreamSetting (..),
    streamSettingSchema,

    -- * Model (Result alias)
    Model,

    -- * Update Type
    Update (..),

    -- * Queries
    getStreamSettings,
    updateStreamSettings,
  )
where

--------------------------------------------------------------------------------

import Data.Int (Int64)
import Data.Maybe (listToMaybe)
import Data.Text (Text)
import Data.Text.Display (Display (..))
import Data.Time (UTCTime)
import Effects.Database.Tables.User qualified as User
import GHC.Generics (Generic)
import Hasql.Interpolate (DecodeRow)
import Hasql.Statement qualified as Hasql
import OrphanInstances.Rel8 ()
import OrphanInstances.UTCTime ()
import Rel8 hiding (Update)
import Rel8 qualified
import Rel8.Expr.Time (now)

--------------------------------------------------------------------------------
-- Table Definition

-- | The @stream_settings@ table definition using rel8's higher-kinded data pattern.
data StreamSetting f = StreamSetting
  { ssId :: Column f Int64,
    ssStreamUrl :: Column f Text,
    ssMetadataUrl :: Column f Text,
    ssUpdatedAt :: Column f UTCTime,
    ssUpdatedBy :: Column f (Maybe User.Id)
  }
  deriving stock (Generic)
  deriving anyclass (Rel8able)

deriving stock instance (f ~ Result) => Show (StreamSetting f)

deriving stock instance (f ~ Result) => Eq (StreamSetting f)

instance DecodeRow (StreamSetting Result)

instance Display (StreamSetting Result) where
  displayBuilder _ = "StreamSetting"

-- | Type alias for backwards compatibility.
type Model = StreamSetting Result

-- | Table schema connecting the Haskell type to the database table.
streamSettingSchema :: TableSchema (StreamSetting Name)
streamSettingSchema =
  TableSchema
    { name = "stream_settings",
      columns =
        StreamSetting
          { ssId = "id",
            ssStreamUrl = "stream_url",
            ssMetadataUrl = "metadata_url",
            ssUpdatedAt = "updated_at",
            ssUpdatedBy = "updated_by"
          }
    }

--------------------------------------------------------------------------------
-- Update Type

-- | Update type for modifying stream settings.
data Update = Update
  { ssuStreamUrl :: Text,
    ssuMetadataUrl :: Text
  }
  deriving stock (Generic, Show, Eq)

--------------------------------------------------------------------------------
-- Queries

-- | Get the stream settings.
--
-- Returns the singleton row from the stream_settings table.
getStreamSettings :: Hasql.Statement () (Maybe Model)
getStreamSettings = fmap listToMaybe $ run $ select do
  row <- each streamSettingSchema
  where_ $ ssId row ==. lit 1
  pure row

-- | Update stream settings and return the updated model.
--
-- The user ID is recorded for audit purposes.
updateStreamSettings :: User.Id -> Update -> Hasql.Statement () (Maybe Model)
updateStreamSettings userId Update {..} =
  fmap listToMaybe $
    run $
      update
        Rel8.Update
          { target = streamSettingSchema,
            from = pure (),
            set = \_ row ->
              row
                { ssStreamUrl = lit ssuStreamUrl,
                  ssMetadataUrl = lit ssuMetadataUrl,
                  ssUpdatedAt = now,
                  ssUpdatedBy = nullify (lit userId)
                },
            updateWhere = \_ row -> ssId row ==. lit 1,
            returning = Returning Prelude.id
          }

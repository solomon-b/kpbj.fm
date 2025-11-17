{-# OPTIONS_GHC -Wno-orphans #-}

module OrphanInstances.TimeOfDay
  ( formatTimeOfDay,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (TimeOfDay)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Hasql.Decoders qualified as Decoders
import Hasql.Encoders qualified as Encoders
import Hasql.Interpolate (DecodeValue (..), EncodeValue (..))

--------------------------------------------------------------------------------

instance EncodeValue TimeOfDay where
  encodeValue = Encoders.time

instance DecodeValue TimeOfDay where
  decodeValue = Decoders.time

--------------------------------------------------------------------------------

-- | Format TimeOfDay as HH:MM text for display
formatTimeOfDay :: TimeOfDay -> Text
formatTimeOfDay = Text.pack . formatTime defaultTimeLocale "%H:%M"

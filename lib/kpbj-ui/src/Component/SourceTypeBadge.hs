-- | The badge for a playout source type, such as @episode@ or @psa@.
--
-- The playback history and the day timeline both show it.
module Component.SourceTypeBadge
  ( sourceTypeBadge,
  )
where

--------------------------------------------------------------------------------

import Component.Badge (Shape (..), Tone (..))
import Component.Badge qualified as Badge
import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------

-- | The badge of a source type. The label is the source type.
sourceTypeBadge :: Text -> Lucid.Html ()
sourceTypeBadge sourceType =
  Badge.badge Tall (tone sourceType) (Lucid.toHtml sourceType)
  where
    tone :: Text -> Tone
    tone = \case
      "episode" -> Success
      "rerun" -> Plain
      "ephemeral" -> Info
      "station_id" -> Warning
      "psa" -> Warning
      "underwriting" -> Warning
      "announcement" -> Warning
      "show_promo" -> Warning
      "filler" -> Muted
      _ -> Inverse

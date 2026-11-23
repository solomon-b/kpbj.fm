-- | The badge for a playout source type, such as @episode@ or @psa@.
--
-- The playback history and the day timeline both show it.
module Component.SourceTypeBadge
  ( sourceTypeBadge,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Badge for source type
sourceTypeBadge :: Text -> Lucid.Html ()
sourceTypeBadge "episode" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.successBg, Tokens.successText]] "episode"
sourceTypeBadge "ephemeral" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.infoBg, Tokens.infoText]] "ephemeral"
sourceTypeBadge "station_id" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.warningBg, Tokens.warningText]] "station_id"
sourceTypeBadge "psa" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.warningBg, Tokens.warningText]] "psa"
sourceTypeBadge "underwriting" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.warningBg, Tokens.warningText]] "underwriting"
sourceTypeBadge "announcement" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.warningBg, Tokens.warningText]] "announcement"
sourceTypeBadge "show_promo" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.warningBg, Tokens.warningText]] "show_promo"
sourceTypeBadge "filler" =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.mutedBg, Tokens.fgMuted]] "filler"
sourceTypeBadge other =
  Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.px3, Tokens.py2, "rounded", Tokens.bgInverse, Tokens.fgInverse]] $ Lucid.toHtml other

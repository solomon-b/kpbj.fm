module API.Dashboard.CurrentShow.Get.Templates (noShowsEmptyState) where

--------------------------------------------------------------------------------

import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Empty state when the user has no shows assigned.
noShowsEmptyState :: Lucid.Html ()
noShowsEmptyState =
  Lucid.section_ [class_ $ base [Theme.bgAlt, "rounded", "overflow-hidden"]] $
    Lucid.div_ [class_ $ base [Theme.fgMuted, "text-center", "p-8"]] $ do
      Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.fontBold, Theme.fgPrimary]] "No Shows Available"
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "You don't have any shows assigned to your account yet."
      Lucid.p_ [class_ $ base [Tokens.textSm, "mt-1"]] "Contact an administrator to get started."

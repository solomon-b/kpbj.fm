-- | Visibility components for KPBJ templates.
--
-- Wrapper components that control visibility based on screen size.
module Design.Lucid.Visibility
  ( desktopOnly,
    mobileOnly,
    tabletUp,
  )
where

--------------------------------------------------------------------------------

import Design.Responsive qualified as Responsive
import Lucid qualified

--------------------------------------------------------------------------------
-- Visibility Components

-- | Show content only on desktop (lg+).
desktopOnly :: Lucid.Html () -> Lucid.Html ()
desktopOnly = Lucid.div_ [Lucid.class_ Responsive.showOnDesktop]

-- | Show content only on mobile (below md).
mobileOnly :: Lucid.Html () -> Lucid.Html ()
mobileOnly = Lucid.div_ [Lucid.class_ Responsive.showOnMobile]

-- | Show content on tablet and up (md+).
tabletUp :: Lucid.Html () -> Lucid.Html ()
tabletUp = Lucid.div_ [Lucid.class_ Responsive.hideOnMobile]

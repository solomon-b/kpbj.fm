-- | Visibility components for KPBJ templates.
--
-- Wrapper components that control visibility based on screen size.
module Component.Visibility
  ( desktopOnly,
    mobileOnly,
    tabletUp,
  )
where

--------------------------------------------------------------------------------

import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------
-- Visibility Components

-- | Show content only on desktop (lg+).
desktopOnly :: Lucid.Html () -> Lucid.Html ()
desktopOnly = Lucid.div_ [Lucid.class_ Tokens.showOnDesktop]

-- | Show content only on mobile (below md).
mobileOnly :: Lucid.Html () -> Lucid.Html ()
mobileOnly = Lucid.div_ [Lucid.class_ Tokens.showOnMobile]

-- | Show content on tablet and up (md+).
tabletUp :: Lucid.Html () -> Lucid.Html ()
tabletUp = Lucid.div_ [Lucid.class_ Tokens.hideOnMobile]

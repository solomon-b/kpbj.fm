-- | Standardized page header component.
--
-- Provides consistent page title styling across the site.
-- Hidden on desktop, visible on mobile/tablet.
module Component.PageHeader
  ( pageHeader,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_, desktop)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a page header (mobile/tablet only, hidden on desktop).
--
-- Example:
--
-- > pageHeader "LOGIN"
pageHeader :: Text -> Lucid.Html ()
pageHeader title =
  Lucid.h1_ [class_ $ do { base [Tokens.fullWidth, Tokens.text2xl, Tokens.fontBold, Tokens.mb8]; desktop ["hidden"] }] $
    Lucid.toHtml title

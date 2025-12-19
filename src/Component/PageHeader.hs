-- | Standardized page header component.
--
-- Provides consistent page title styling across the site.
module Component.PageHeader
  ( pageHeader,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render a page header.
--
-- Example:
--
-- > pageHeader "LOGIN"
pageHeader :: Text -> Lucid.Html ()
pageHeader title =
  Lucid.h1_ [class_ $ base [Tokens.fullWidth, Tokens.text2xl, Tokens.fontBold, Tokens.mb8]] $
    Lucid.toHtml title

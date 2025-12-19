-- | Card components for KPBJ templates.
--
-- Pre-styled card containers with consistent design token usage.
module Design.Lucid.Card
  ( card,
    cardWithHeader,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------
-- Card Components

-- | Basic card with standard styling.
card :: Lucid.Html () -> Lucid.Html ()
card = Lucid.article_ [Lucid.class_ Tokens.cardBase]

-- | Card with a header section.
cardWithHeader :: Text -> Lucid.Html () -> Lucid.Html ()
cardWithHeader title content =
  Lucid.article_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder]] $ do
    Lucid.header_ [Lucid.class_ $ cls [Tokens.bgGray100, Tokens.p4, "border-b-2", Tokens.borderGray800]] $
      Lucid.h3_ [Lucid.class_ Tokens.fontBold] $
        Lucid.toHtml title
    Lucid.div_ [Lucid.class_ Tokens.cardPadding] content

module API.About.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import Component.PageHeader (pageHeader)
import Design (base, class_)
import Design.Tokens (fontMedium, mb6, mb8, textGray800)
import Lucid qualified

--------------------------------------------------------------------------------

-- | About page template
template :: Lucid.Html ()
template = do
  pageHeader "ABOUT KPBJ"
  Lucid.div_ [class_ $ base ["prose", "prose-lg", textGray800]] $ do
    Lucid.p_
      [class_ $ base [mb6, "leading-relaxed"]]
      "KPBJ is an independent, community-driven radio station based in Shadow Hills, California."

    Lucid.p_
      [class_ $ base [mb6, "leading-relaxed"]]
      "KPBJ is a platform to transmit what echoes in the reach of our airwaves. We share diverse voices and artists from the San Fernando Valley, Los Angeles, and beyond through a blend of music, conversation, and storytelling."

    Lucid.p_
      [class_ $ base [mb6, "leading-relaxed"]]
      "Our online stream will be operational in the Fall of 2025, and our FM broadcast on 95.9 FM will follow soon after. Currently, we are run only on volunteer time and the support of individual donors."

    Lucid.p_
      [class_ $ base [mb6, "leading-relaxed font-medium"]]
      "We believe that The Valley is the center of the universe and we want to share it with you."

    Lucid.p_
      [class_ $ base [mb8, "leading-relaxed"]]
      "KPBJ-FM is operated by Sun Valley Arts and Culture, a 501(c)3 nonprofit arts organization."

    Lucid.p_
      [Lucid.class_ fontMedium]
      "Want to get involved? Sign up for our newsletter to learn more."

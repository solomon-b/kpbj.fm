{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Show
  ( renderShowCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, showsLinks)
import API.Types
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

--------------------------------------------------------------------------------

-- | Render a show card for the mobile list view.
--
-- Displays a large image (or gray placeholder) and show title.
-- Tags are displayed on the individual show page.
renderShowCard :: Shows.Model -> Lucid.Html ()
renderShowCard s = do
  let showSlug = s.slug
      showTitle = s.title
  Lucid.a_
    [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
      hxGet_ [i|/#{showGetUrl showSlug}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "block"
    ]
    $ do
      -- Show Image (or gray placeholder)
      Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-[4/3]", "overflow-hidden", Tokens.mb2]] $ do
        case s.logoUrl of
          Just logoUrl -> do
            let logoAlt = showTitle <> " logo"
            Lucid.img_
              [ Lucid.src_ [i|/#{mediaGetUrl}/#{logoUrl}|],
                Lucid.alt_ logoAlt,
                Lucid.class_ "w-full h-full object-cover"
              ]
          Nothing ->
            Lucid.div_ [class_ $ base [Tokens.fullWidth, "h-full", "bg-gray-200", "dark:bg-gray-700"]] mempty

      -- Show Title
      Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2]] $
        Lucid.toHtml showTitle

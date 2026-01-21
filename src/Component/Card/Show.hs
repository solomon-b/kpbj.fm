{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Component.Card.Show
  ( renderShowCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Types
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ showsLinks.detail slug Nothing

--------------------------------------------------------------------------------

-- | Render a show card for the mobile list view.
--
-- Displays a large image (or gray placeholder) and show title.
-- Tags are displayed on the individual show page.
renderShowCard :: StorageBackend -> Shows.Model -> Lucid.Html ()
renderShowCard backend s = do
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
      Lucid.div_ [class_ $ base [Tokens.fullWidth, "aspect-[4/3]", "overflow-hidden", Tokens.mb2, "border", "border-gray-300"]] $ do
        case s.logoUrl of
          Just logoUrl -> do
            let logoAlt = showTitle <> " logo"
            Lucid.img_
              [ Lucid.src_ (buildMediaUrl backend logoUrl),
                Lucid.alt_ logoAlt,
                Lucid.class_ "w-full h-full object-cover"
              ]
          Nothing ->
            Lucid.div_ [class_ $ base [Tokens.fullWidth, "h-full", Tokens.bgGray100]] mempty

      -- Show Title
      Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2]] $
        Lucid.toHtml showTitle

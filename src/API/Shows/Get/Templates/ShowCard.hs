{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.ShowCard
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
-- Displays a large image (or gray placeholder), show title, and genre tags.
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
            Lucid.div_ [class_ $ base [Tokens.fullWidth, "h-full", "bg-gray-200"]] mempty

      Lucid.div_ [class_ $ base [Tokens.fullWidth, "flex", "justify-between"]] $ do
        -- Show Title
        Lucid.h3_ [class_ $ base [Tokens.fontBold, Tokens.mb2]] $
          Lucid.toHtml showTitle

        -- Genre tags
        case s.genre of
          Just genre ->
            Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap2]] $ do
              renderTag genre
          Nothing -> mempty

-- | Render a genre tag pill.
renderTag :: (Lucid.ToHtml a) => a -> Lucid.Html ()
renderTag tag =
  Lucid.span_
    [class_ $ base [Tokens.textSm, "px-3", "py-1"]]
    $ Lucid.toHtml tag

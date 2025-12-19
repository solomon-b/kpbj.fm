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
import Data.Text qualified as Text
import Design.StyleBuilder.Internal (cls)
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

-- | Render a show card for the list view
renderShowCard :: Shows.Model -> Lucid.Html ()
renderShowCard s = do
  let showSlug = s.slug
      showTitle = s.title
      showDescription = s.description
  Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
    -- Show Image
    Lucid.div_ [Lucid.class_ $ cls ["text-center", Tokens.mb4]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
          hxGet_ [i|/#{showGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "block"
        ]
        $ Lucid.div_ [Lucid.class_ $ cls [Tokens.fullWidth, "aspect-square", "bg-gray-300", Tokens.border2, "border-gray-600", "flex", "items-center", "justify-center", Tokens.mb4, Tokens.textLg]]
        $ case s.logoUrl of
          Just logoUrl -> do
            let logoAlt = showTitle <> " logo"
            Lucid.img_ [Lucid.src_ [i|/#{mediaGetUrl}/#{logoUrl}|], Lucid.alt_ logoAlt, Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[SHOW IMG]"

      -- Show Title and Basic Info
      Lucid.h3_ [Lucid.class_ $ cls [Tokens.textXl, Tokens.fontBold, Tokens.mb2]]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
            hxGet_ [i|/#{showGetUrl showSlug}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
        $ Lucid.toHtml showTitle

      Lucid.div_ [Lucid.class_ $ cls [Tokens.textSm, Tokens.textGray600, "mb-3"]] $ do
        -- Schedule info would come from show_schedules table
        "Schedule info here" -- TODO: Join with schedule data

      -- Genre tag
      case s.genre of
        Just genre ->
          Lucid.div_ [Lucid.class_ $ cls [Tokens.textXs, "bg-gray-200", Tokens.textGray800, "px-2", "py-1", "font-mono", "mb-3"]] $
            "#" <> Lucid.toHtml genre
        Nothing -> mempty

    -- Description
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textSm, "leading-relaxed", Tokens.mb4]] $ do
      let truncatedDesc = Text.take 150 showDescription
      Lucid.toHtml $ truncatedDesc <> if Text.length showDescription > 150 then "..." else ""

    -- Actions
    Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.gap2]] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
          hxGet_ [i|/#{showGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls [Tokens.bgGray800, Tokens.textWhite, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "hover:bg-gray-700", "flex-grow", "text-center"]
        ]
        "VIEW"
      Lucid.button_ [Lucid.class_ $ cls ["border", "border-gray-800", Tokens.px3, Tokens.py2, Tokens.textSm, "hover:bg-gray-100"]] "♡"

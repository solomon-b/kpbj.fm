{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Get.Templates.ShowCard
  ( renderShowCard,
  )
where

import {-# SOURCE #-} API (showGetLink)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

-- | Render a show card for the list view
renderShowCard :: Shows.Model -> Lucid.Html ()
renderShowCard s = do
  let showSlug = s.slug
      showTitle = s.title
      showDescription = s.description
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    -- Show Image
    Lucid.div_ [Lucid.class_ "text-center mb-4"] $ do
      Lucid.div_ [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center mb-4 text-lg"] $
        case s.logoUrl of
          Just logoUrl -> Lucid.img_ [Lucid.src_ logoUrl, Lucid.alt_ showTitle, Lucid.class_ "w-full h-full object-cover"]
          Nothing -> "[SHOW IMG]"

      -- Show Title and Basic Info
      Lucid.h3_ [Lucid.class_ "text-xl font-bold mb-2"]
        $ Lucid.a_
          [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
            hxGet_ [i|/#{showGetUrl showSlug}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "hover:underline"
          ]
        $ Lucid.toHtml showTitle

      Lucid.div_ [Lucid.class_ "text-sm text-gray-600 mb-3"] $ do
        -- Schedule info would come from show_schedules table
        "Schedule info here" -- TODO: Join with schedule data

      -- Genre tag
      case s.genre of
        Just genre ->
          Lucid.div_ [Lucid.class_ "text-xs bg-gray-200 text-gray-800 px-2 py-1 font-mono mb-3"] $
            "#" <> Lucid.toHtml genre
        Nothing -> mempty

    -- Description
    Lucid.p_ [Lucid.class_ "text-sm leading-relaxed mb-4"] $ do
      let truncatedDesc = Text.take 150 showDescription
      Lucid.toHtml $ truncatedDesc <> if Text.length showDescription > 150 then "..." else ""

    -- Actions
    Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showGetUrl showSlug}|],
          hxGet_ [i|/#{showGetUrl showSlug}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700 flex-grow text-center"
        ]
        "VIEW"
      Lucid.button_ [Lucid.class_ "border border-gray-800 px-3 py-2 text-sm hover:bg-gray-100"] "♡"
  where
    showGetUrl :: Text.Text -> Links.URI
    showGetUrl slug = Links.linkURI $ showGetLink slug

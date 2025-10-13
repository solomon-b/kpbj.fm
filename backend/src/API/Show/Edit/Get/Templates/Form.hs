{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Show.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, showGetLink)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

showGetUrl :: Text.Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

-- | Show edit template
template :: Shows.Model -> UserMetadata.Model -> Bool -> Lucid.Html ()
template showModel userMeta isStaff = do
  let showSlug = showModel.slug
      showBackUrl = showGetUrl showSlug

  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT SHOW"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBackUrl}|],
            hxGet_ [i|/#{showBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "DASHBOARD"

  -- Edit Show Form
  Lucid.form_ [Lucid.action_ [i|/shows/#{showSlug}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Basic Information
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "BASIC INFORMATION"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        -- Title
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.value_ showModel.title,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Industrial Depths"
            ]

        -- Slug
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] $ do
            "URL Slug *"
            Lucid.span_ [Lucid.class_ "text-sm font-normal text-gray-600 ml-2"] "(used in the show's web address)"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "slug",
              Lucid.required_ "true",
              Lucid.value_ showModel.slug,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. industrial-depths",
              Lucid.pattern_ "[a-z0-9-]+",
              Lucid.title_ "Lowercase letters, numbers, and hyphens only"
            ]
          Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] $
            "URL will be: kpbj.fm/shows/" <> Lucid.span_ [Lucid.class_ "font-mono"] (Lucid.toHtml showModel.slug)

        -- Description
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Description *"
          Lucid.textarea_
            [ Lucid.name_ "description",
              Lucid.required_ "true",
              Lucid.rows_ "6",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
              Lucid.placeholder_ "Describe your show. What kind of music do you play? What's your show's vibe?"
            ]
            (Lucid.toHtml showModel.description)

        -- Genre
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Genre"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "genre",
              Lucid.value_ (fromMaybe "" showModel.genre),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Techno, Ambient, Experimental, Hip-Hop"
            ]
          Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Primary genre or style of music"

    -- Schedule & Settings (Staff+ Only)
    if isStaff
      then Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "SCHEDULE & SETTINGS"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          -- Status
          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show Status *"
            Lucid.select_
              [ Lucid.name_ "status",
                Lucid.required_ "true",
                Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
              ]
              $ do
                Lucid.option_ [Lucid.value_ "active", selectedIf (showModel.status == Shows.Active)] "Active"
                Lucid.option_ [Lucid.value_ "inactive", selectedIf (showModel.status == Shows.Inactive)] "Inactive"
            Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Active shows appear on the shows page"

          -- Frequency
          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Broadcast Frequency *"
            Lucid.select_
              [ Lucid.name_ "frequency",
                Lucid.required_ "true",
                Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
              ]
              $ do
                Lucid.option_ [Lucid.value_ "weekly", selectedIf (showModel.frequency == Shows.Weekly)] "Weekly"
                Lucid.option_ [Lucid.value_ "biweekly", selectedIf (showModel.frequency == Shows.Biweekly)] "Biweekly"
                Lucid.option_ [Lucid.value_ "monthly", selectedIf (showModel.frequency == Shows.Monthly)] "Monthly"
                Lucid.option_ [Lucid.value_ "occasional", selectedIf (showModel.frequency == Shows.Occasional)] "Occasional"
                Lucid.option_ [Lucid.value_ "one-time", selectedIf (showModel.frequency == Shows.OneTime)] "One-time"

          -- Duration
          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Typical Duration (minutes)"
            Lucid.input_
              [ Lucid.type_ "number",
                Lucid.name_ "duration_minutes",
                Lucid.value_ (maybe "" (Text.pack . show) showModel.durationMinutes),
                Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                Lucid.placeholder_ "e.g. 120",
                Lucid.min_ "1",
                Lucid.step_ "1"
              ]
            Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "How long is a typical episode?"
      else do
        -- Hidden fields to preserve existing values for non-staff
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ (case showModel.status of Shows.Active -> "active"; Shows.Inactive -> "inactive")]
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "frequency", Lucid.value_ (case showModel.frequency of Shows.Weekly -> "weekly"; Shows.Biweekly -> "biweekly"; Shows.Monthly -> "monthly"; Shows.Occasional -> "occasional"; Shows.OneTime -> "one-time")]
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "duration_minutes", Lucid.value_ (maybe "" (Text.pack . show) showModel.durationMinutes)]

    -- Artwork & Branding
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "ARTWORK & BRANDING"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        -- Logo URL
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Logo URL"
          Lucid.input_
            [ Lucid.type_ "url",
              Lucid.name_ "logo_url",
              Lucid.value_ (fromMaybe "" showModel.logoUrl),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "https://example.com/logo.png"
            ]
          Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Square logo image (recommended: 300x300px)"

        -- Banner URL
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Banner URL"
          Lucid.input_
            [ Lucid.type_ "url",
              Lucid.name_ "banner_url",
              Lucid.value_ (fromMaybe "" showModel.bannerUrl),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "https://example.com/banner.png"
            ]
          Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Wide banner image (recommended: 1200x300px)"

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
          ]
          "UPDATE SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{showBackUrl}|],
            hxGet_ [i|/#{showBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
          ]
          "CANCEL"

--------------------------------------------------------------------------------

-- Helper to set selected attribute
selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Episode.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeGetLink, hostDashboardGetLink)
import Control.Monad (forM_)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

episodeGetUrl :: Text.Text -> Text.Text -> Links.URI
episodeGetUrl showSlug episodeSlug = Links.linkURI $ episodeGetLink showSlug episodeSlug

--------------------------------------------------------------------------------

-- | Episode edit template
template :: Shows.Model -> Episodes.Model -> [EpisodeTrack.Model] -> UserMetadata.Model -> Bool -> Lucid.Html ()
template showModel episode tracks userMeta isStaff = do
  let showSlug = showModel.slug
      episodeSlug = episode.slug
      episodeBackUrl = episodeGetUrl showSlug episodeSlug

  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Episode #"
          Lucid.toHtml (display episode.episodeNumber)
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{episodeBackUrl}|],
            hxGet_ [i|/#{episodeBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO EPISODE"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "DASHBOARD"

  -- Edit Episode Form
  Lucid.form_ [Lucid.action_ [i|/shows/#{showSlug}/episodes/#{episodeSlug}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Basic Information
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "BASIC INFORMATION"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        -- Title
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.value_ episode.title,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Deep Dive into Techno"
            ]

        -- Slug
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] $ do
            "URL Slug *"
            Lucid.span_ [Lucid.class_ "text-sm font-normal text-gray-600 ml-2"] "(used in the episode's web address)"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "slug",
              Lucid.required_ "true",
              Lucid.value_ episode.slug,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. deep-dive-into-techno",
              Lucid.pattern_ "[a-z0-9-]+",
              Lucid.title_ "Lowercase letters, numbers, and hyphens only"
            ]
          Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] $
            "URL will be: kpbj.fm/shows/" <> Lucid.span_ [Lucid.class_ "font-mono"] (Lucid.toHtml showSlug) <> "/episodes/" <> Lucid.span_ [Lucid.class_ "font-mono"] (Lucid.toHtml episodeSlug)

        -- Description
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Description"
          Lucid.textarea_
            [ Lucid.name_ "description",
              Lucid.rows_ "6",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
              Lucid.placeholder_ "Describe this episode. What tracks did you play? Any special guests or themes?"
            ]
            (Lucid.toHtml $ fromMaybe "" episode.description)

    -- Status & Schedule (Staff+ Only)
    if isStaff
      then Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
        Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "STATUS & SCHEDULE"

        Lucid.div_ [Lucid.class_ "space-y-6"] $ do
          -- Status
          Lucid.div_ $ do
            Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Status *"
            Lucid.select_
              [ Lucid.name_ "status",
                Lucid.required_ "true",
                Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
              ]
              $ do
                Lucid.option_ [Lucid.value_ "draft", selectedIf (episode.status == Episodes.Draft)] "Draft"
                Lucid.option_ [Lucid.value_ "scheduled", selectedIf (episode.status == Episodes.Scheduled)] "Scheduled"
                Lucid.option_ [Lucid.value_ "published", selectedIf (episode.status == Episodes.Published)] "Published"
                Lucid.option_ [Lucid.value_ "archived", selectedIf (episode.status == Episodes.Archived)] "Archived"
            Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mt-1"] "Only published episodes appear on the show page"
      else do
        -- Hidden field to preserve status for non-staff
        Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "status", Lucid.value_ (display episode.status)]

    -- Track Listing
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 border-b border-gray-800 pb-2"] "TRACK LISTING"

      Lucid.div_ [Lucid.id_ "tracks-container", Lucid.class_ "space-y-4"] $ do
        if null tracks
          then do
            Lucid.p_ [Lucid.class_ "text-gray-600 italic"] "No tracks added yet."
          else do
            forM_ (zip [(0 :: Int) ..] tracks) $ \(idx, track) -> do
              renderTrackEditor idx track

      -- Add Track Button
      Lucid.div_ [Lucid.class_ "mt-4"] $ do
        Lucid.button_
          [ Lucid.type_ "button",
            Lucid.id_ "add-track-btn",
            Lucid.class_ "bg-blue-600 text-white px-6 py-2 font-bold hover:bg-blue-700"
          ]
          "+ ADD TRACK"

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
          ]
          "UPDATE EPISODE"
        Lucid.a_
          [ Lucid.href_ [i|/#{episodeBackUrl}|],
            hxGet_ [i|/#{episodeBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
          ]
          "CANCEL"

--------------------------------------------------------------------------------

-- Helper to render a single track editor
renderTrackEditor :: Int -> EpisodeTrack.Model -> Lucid.Html ()
renderTrackEditor idx track = do
  Lucid.div_ [Lucid.class_ "border-2 border-gray-300 p-4 track-item"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center mb-3"] $ do
      Lucid.h3_ [Lucid.class_ "font-bold"] $ "Track #" <> Lucid.toHtml (show (idx + 1))
      Lucid.button_
        [ Lucid.type_ "button",
          Lucid.class_ "text-red-600 font-bold hover:text-red-800 remove-track-btn"
        ]
        "✕ REMOVE"

    Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ [i|tracks[#{idx}][id]|], Lucid.value_ (display track.id)]

    Lucid.div_ [Lucid.class_ "grid grid-cols-2 gap-4"] $ do
      -- Track Number
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Track #"
        Lucid.input_
          [ Lucid.type_ "number",
            Lucid.name_ [i|tracks[#{idx}][track_number]|],
            Lucid.value_ (display track.trackNumber),
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Duration
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Duration"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ [i|tracks[#{idx}][duration]|],
            Lucid.value_ (fromMaybe "" track.duration),
            Lucid.placeholder_ "e.g. 5:42",
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Title
      Lucid.div_ [Lucid.class_ "col-span-2"] $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Title *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ [i|tracks[#{idx}][title]|],
            Lucid.value_ track.title,
            Lucid.required_ "true",
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Artist
      Lucid.div_ [Lucid.class_ "col-span-2"] $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Artist *"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ [i|tracks[#{idx}][artist]|],
            Lucid.value_ track.artist,
            Lucid.required_ "true",
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Album
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Album"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ [i|tracks[#{idx}][album]|],
            Lucid.value_ (fromMaybe "" track.album),
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Label
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Label"
        Lucid.input_
          [ Lucid.type_ "text",
            Lucid.name_ [i|tracks[#{idx}][label]|],
            Lucid.value_ (fromMaybe "" track.label),
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Year
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "block font-bold mb-1 text-sm"] "Year"
        Lucid.input_
          [ Lucid.type_ "number",
            Lucid.name_ [i|tracks[#{idx}][year]|],
            Lucid.value_ (maybe "" display track.year),
            Lucid.class_ "w-full p-2 border border-gray-400 font-mono text-sm"
          ]

      -- Premiere checkbox
      Lucid.div_ $ do
        Lucid.label_ [Lucid.class_ "flex items-center space-x-2"] $ do
          Lucid.input_
            [ Lucid.type_ "checkbox",
              Lucid.name_ [i|tracks[#{idx}][is_exclusive_premiere]|],
              Lucid.value_ "true",
              if track.isExclusivePremiere then Lucid.checked_ else mempty,
              Lucid.class_ "w-4 h-4"
            ]
          Lucid.span_ [Lucid.class_ "text-sm font-bold"] "Exclusive Premiere"

--------------------------------------------------------------------------------

-- Helper to set selected attribute
selectedIf :: Bool -> Lucid.Attributes
selectedIf True = Lucid.selected_ "selected"
selectedIf False = mempty

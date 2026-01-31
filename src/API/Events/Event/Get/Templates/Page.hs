{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Event.Get.Templates.Page
  ( template,
    notFoundTemplate,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks)
import API.Types
import Component.Card.Event (renderEventCardDetail)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI eventsLinks.list

--------------------------------------------------------------------------------

-- | Main event template
template :: StorageBackend -> Events.Model -> UserMetadata.Model -> Lucid.Html () -> Lucid.Html ()
template backend event author renderedDescription = do
  -- Event card with full details
  renderEventCardDetail backend event renderedDescription

  -- Event creator info
  Lucid.section_ [class_ $ base ["mt-6"]] $ do
    Lucid.div_ [class_ $ base [Tokens.p4, "bg-gray-50", "border-l-4", "border-gray-800"]] $ do
      Lucid.div_ [class_ $ base ["flex", "items-start", Tokens.gap4]] $ do
        Lucid.div_ [class_ $ base ["w-12", "h-12", "bg-gray-300", "rounded-full", "flex", "items-center", "justify-center", Tokens.textSm]] $
          Lucid.toHtml $
            Text.take 2 (display author.mDisplayName)
        Lucid.div_ $ do
          Lucid.h3_ [class_ $ base [Tokens.fontBold, "mb-1"]] $
            Lucid.toHtml ("Event created by " <> display author.mDisplayName)
          Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
            Lucid.toHtml $
              "KPBJ " <> Text.pack (show author.mUserRole) <> " • " <> display author.mFullName

  -- Navigation back to events
  Lucid.div_ [class_ $ base ["text-center", "mt-6"]] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
      ]
      "← BACK TO EVENTS"

-- | Template for when event is not found
notFoundTemplate :: Slug -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [class_ $ base [Tokens.bgMain, Tokens.cardBorder, Tokens.p8, "text-center"]] $ do
    Lucid.h1_ [class_ $ base [Tokens.text3xl, Tokens.fontBold, Tokens.mb4]] "Event Not Found"
    Lucid.p_ [class_ $ base [Tokens.fgMuted, Tokens.mb6]] $ do
      "The event with slug \""
      Lucid.code_ [class_ $ base [Tokens.bgAlt, "px-2", "py-1"]] $ Lucid.toHtml (display slug)
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700", "inline-block"]
      ]
      "← BACK TO EVENTS"

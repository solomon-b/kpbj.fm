{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.WeekView
  ( renderWeekContent,
    renderWeekDay,
    renderWeekEvent,
    WeekDay (..),
  )
where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventGetLink, eventsGetLink)
import Data.Foldable (traverse_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, Year)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Domain.Types.PageView (PageView (..))
import Effects.Database.Tables.EventTags qualified as EventTag
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Week day data structure (re-exported from main module)
data WeekDay = WeekDay
  { wdDayName :: Text,
    wdDayNumber :: Int,
    wdDate :: UTCTime,
    wdEvents :: [Events.EventModel]
  }

--------------------------------------------------------------------------------

-- URL helpers
eventGetUrl :: Text -> Links.URI
eventGetUrl slug = Links.linkURI $ eventGetLink slug

eventsGetWeekUrl :: Year -> Int -> Maybe Text -> Links.URI
eventsGetWeekUrl year weekNum maybeTag =
  Links.linkURI $ eventsGetLink maybeTag (Just $ WeekView year weekNum)

formatWeekTitle :: UTCTime -> UTCTime -> Text
formatWeekTitle startDate endDate =
  let startFormatted = Text.toUpper $ Text.pack $ formatTime defaultTimeLocale "%B %d" startDate
      endFormatted = Text.pack $ formatTime defaultTimeLocale "%d, %Y" endDate
   in "WEEK OF " <> startFormatted <> " - " <> endFormatted

--------------------------------------------------------------------------------

-- | Render the week view content (for HTMX updates)
renderWeekContent ::
  Year ->
  Int ->
  Maybe Text ->
  [EventTag.EventTagWithCount] ->
  [WeekDay] ->
  UTCTime ->
  UTCTime ->
  ((Year, Int), (Year, Int)) ->
  Lucid.Html ()
renderWeekContent _year _weekNum _maybeTagFilter _eventTagsWithCounts weekGrid startDate endDate ((prevYear, prevWeek), (nextYear, nextWeek)) = do
  let weekTitle = formatWeekTitle startDate endDate

  -- Week Calendar View
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between mb-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold"] $ Lucid.toHtml weekTitle
      Lucid.div_ [Lucid.class_ "flex gap-2"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          "‹ Previous Week"
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "px-3 py-1 text-gray-600 hover:text-gray-800"
          ]
          "Next Week ›"

    -- Week Grid
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-7 gap-4"] $ do
      traverse_ renderWeekDay weekGrid

-- | Render a single day in the week view
renderWeekDay :: WeekDay -> Lucid.Html ()
renderWeekDay day = do
  Lucid.div_ [Lucid.class_ "border border-gray-200 p-3"] $ do
    Lucid.div_ [Lucid.class_ "font-bold mb-2 text-center"] $
      Lucid.toHtml $
        wdDayName day <> " " <> Text.pack (show $ wdDayNumber day)

    if null (wdEvents day)
      then Lucid.div_ [Lucid.class_ "text-xs text-gray-400 text-center mt-8"] "No events"
      else Lucid.div_ [Lucid.class_ "space-y-2"] $ do
        traverse_ renderWeekEvent (wdEvents day)

-- | Render an event within a week day
renderWeekEvent :: Events.EventModel -> Lucid.Html ()
renderWeekEvent event = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 text-yellow-800 p-2 text-xs"] $ do
    Lucid.div_ [Lucid.class_ "font-bold"] $
      Lucid.toHtml $
        formatTime defaultTimeLocale "%l %p" (Events.emStartsAt event)
    Lucid.div_ [Lucid.class_ "truncate"]
      $ Lucid.a_
        [ Lucid.href_ [i|/#{eventGetUrl (Events.emSlug event)}|],
          hxGet_ [i|/#{eventGetUrl (Events.emSlug event)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
      $ Lucid.toHtml (Events.emTitle event)

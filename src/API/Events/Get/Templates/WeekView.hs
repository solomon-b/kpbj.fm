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

import API.Links (eventsLinks)
import API.Types
import Data.Foldable (traverse_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, Year)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Design (base, class_, tablet)
import Design.Tokens qualified as Tokens
import Domain.Types.PageView (PageView (..))
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.EventTags qualified as EventTags
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
    wdEvents :: [Events.Model]
  }

--------------------------------------------------------------------------------

-- URL helpers
eventGetUrl :: Events.Id -> Slug -> Links.URI
eventGetUrl eventId slug = Links.linkURI $ eventsLinks.detailWithSlug eventId slug

eventsGetWeekUrl :: Year -> Int -> Maybe Text -> Links.URI
eventsGetWeekUrl year weekNum maybeTag =
  Links.linkURI $ eventsLinks.list maybeTag (Just $ WeekView year weekNum)

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
  [EventTags.EventTagWithCount] ->
  [WeekDay] ->
  UTCTime ->
  UTCTime ->
  ((Year, Int), (Year, Int)) ->
  Lucid.Html ()
renderWeekContent _year _weekNum _maybeTagFilter _eventTagsWithCounts weekGrid startDate endDate ((prevYear, prevWeek), (nextYear, nextWeek)) = do
  let weekTitle = formatWeekTitle startDate endDate

  -- Week Calendar View
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mb6]] $ do
      Lucid.h2_ [class_ $ base [Tokens.textXl, Tokens.fontBold]] $ Lucid.toHtml weekTitle
      Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl prevYear prevWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["px-3", "py-1", Tokens.textGray600, "hover:text-gray-800"]
          ]
          "‹ Previous Week"
        Lucid.a_
          [ Lucid.href_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxGet_ [i|/#{eventsGetWeekUrl nextYear nextWeek Nothing}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["px-3", "py-1", Tokens.textGray600, "hover:text-gray-800"]
          ]
          "Next Week ›"

    -- Week Grid
    Lucid.div_ [class_ $ do { base ["grid", "grid-cols-1", Tokens.gap4]; tablet ["grid-cols-7"] }] $ do
      traverse_ renderWeekDay weekGrid

-- | Render a single day in the week view
renderWeekDay :: WeekDay -> Lucid.Html ()
renderWeekDay day = do
  Lucid.div_ [class_ $ base ["border", "border-gray-200", Tokens.p3]] $ do
    Lucid.div_ [class_ $ base [Tokens.fontBold, Tokens.mb2, "text-center"]] $
      Lucid.toHtml $
        wdDayName day <> " " <> Text.pack (show $ wdDayNumber day)

    if null (wdEvents day)
      then Lucid.div_ [class_ $ base [Tokens.textXs, "text-gray-400", "text-center", "mt-8"]] "No events"
      else Lucid.div_ [Lucid.class_ "space-y-2"] $ do
        traverse_ renderWeekEvent (wdEvents day)

-- | Render an event within a week day
renderWeekEvent :: Events.Model -> Lucid.Html ()
renderWeekEvent event = do
  Lucid.div_ [class_ $ base ["bg-yellow-100", "text-yellow-800", Tokens.p2, Tokens.textXs]] $ do
    Lucid.div_ [Lucid.class_ Tokens.fontBold] $
      Lucid.toHtml $
        formatTime defaultTimeLocale "%l %p" (Events.emStartsAt event)
    Lucid.div_ [Lucid.class_ "truncate"]
      $ Lucid.a_
        [ Lucid.href_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
          hxGet_ [i|/#{eventGetUrl (Events.emId event) (Events.emSlug event)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "hover:underline"
        ]
      $ Lucid.toHtml (Events.emTitle event)

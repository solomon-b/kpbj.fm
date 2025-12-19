{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.Controls
  ( renderViewControls,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks)
import API.Types
import Data.Foldable (traverse_)
import Data.Maybe (isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (MonthOfYear, UTCTime, Year, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Design.StyleBuilder.Internal (cls, md)
import Design.Tokens qualified as Tokens
import Domain.Types.PageView (PageView (..), isMonthView, isWeekView)
import Effects.Database.Tables.EventTags qualified as EventTags
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxSwap_, hxTarget_, hxTrigger_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetWeekUrl :: Year -> Int -> Maybe Text -> Links.URI
eventsGetWeekUrl year weekNum maybeTag =
  Links.linkURI $ eventsLinks.list maybeTag (Just $ WeekView year weekNum)

utcTimeToYearWeek :: UTCTime -> (Year, Int)
utcTimeToYearWeek utc =
  let (year, weekNum, _) = toWeekDate (utctDay utc)
   in (year, weekNum)

listEventsUri :: Maybe Text -> Maybe PageView -> Links.URI
listEventsUri maybeTagFilter = Links.linkURI . eventsLinks.list maybeTagFilter

--------------------------------------------------------------------------------

-- | Shared view controls component
renderViewControls :: UTCTime -> PageView -> (Year, MonthOfYear) -> Maybe Text -> [EventTags.EventTagWithCount] -> Lucid.Html ()
renderViewControls currentTime currentView currentMonth maybeTagFilter eventTagsWithCounts = do
  let currentWeek = utcTimeToYearWeek currentTime
  Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-col", md "flex-row", md "items-center", md "justify-between", Tokens.gap4]] $ do
    -- View Toggle
    Lucid.div_ [Lucid.class_ $ cls ["flex", "items-center", Tokens.gap4]] $ do
      Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textSm]] "VIEW:"
      Lucid.div_ [Lucid.class_ $ cls ["flex", Tokens.border2, "border-gray-800"]] $ do
        let listClasses =
              if currentView == ListView
                then cls [Tokens.px4, Tokens.py2, Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold]
                else cls [Tokens.px4, Tokens.py2, Tokens.bgWhite, Tokens.textGray800, Tokens.fontBold, "hover:bg-gray-100"]
            monthClasses =
              if isMonthView currentView
                then cls [Tokens.px4, Tokens.py2, Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold]
                else cls [Tokens.px4, Tokens.py2, Tokens.bgWhite, Tokens.textGray800, Tokens.fontBold, "hover:bg-gray-100"]
            weekClasses =
              if isWeekView currentView
                then cls [Tokens.px4, Tokens.py2, Tokens.bgGray800, Tokens.textWhite, Tokens.fontBold]
                else cls [Tokens.px4, Tokens.py2, Tokens.bgWhite, Tokens.textGray800, Tokens.fontBold, "hover:bg-gray-100"]
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{listEventsUri maybeTagFilter (Just ListView)}|],
            hxGet_ [i|/#{listEventsUri maybeTagFilter (Just ListView)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ listClasses
          ]
          "LIST"
        -- For month view, we'll use current month
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{listEventsUri maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxGet_ [i|/#{listEventsUri maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ monthClasses
          ]
          "MONTH"
        -- For week view, we'll use current week
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxGet_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ weekClasses
          ]
          "WEEK"

    -- Filters
    Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-wrap", "items-center", Tokens.gap4]] $ do
      Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textSm]] "FILTER:"
      -- Event tag filter
      Lucid.select_
        [ Lucid.class_ $ cls [Tokens.border2, "border-gray-600", Tokens.p2, "font-mono", Tokens.textSm],
          Lucid.name_ "tag",
          hxGet_ [i|/#{listEventsUri Nothing (Just currentView)}|],
          hxTarget_ "#events-content-container",
          hxSwap_ "innerHTML",
          hxTrigger_ "change"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> ([Lucid.selected_ "" | isNothing maybeTagFilter])) "All Events"
          traverse_ renderEventTagOption eventTagsWithCounts
  where
    renderEventTagOption :: EventTags.EventTagWithCount -> Lucid.Html ()
    renderEventTagOption tagWithCount =
      Lucid.option_
        ( [Lucid.value_ tagWithCount.etwcTag]
            <> ([Lucid.selected_ "" | Just tagWithCount.etwcTag == maybeTagFilter])
        )
        $ Lucid.toHtml
        $ tagWithCount.etwcTag
          <> " ("
          <> Text.pack (show tagWithCount.etwcCount)
          <> ")"

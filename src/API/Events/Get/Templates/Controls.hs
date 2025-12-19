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
import Design (base, class_, tablet, when)
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
  Lucid.div_ [class_ $ do { base ["flex", "flex-col", Tokens.gap4]; tablet ["flex-row", "items-center", "justify-between"] }] $ do
    -- View Toggle
    Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap4]] $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textSm]] "VIEW:"
      Lucid.div_ [class_ $ base ["flex", Tokens.border2, "border-gray-800"]] $ do
        let viewToggle isActive = class_ $ do
              base [Tokens.px4, Tokens.py2, Tokens.fontBold]
              when isActive $ base [Tokens.bgGray800, Tokens.textWhite]
              when (not isActive) $ base [Tokens.bgWhite, Tokens.textGray800, "hover:bg-gray-100"]
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{listEventsUri maybeTagFilter (Just ListView)}|],
            hxGet_ [i|/#{listEventsUri maybeTagFilter (Just ListView)}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            viewToggle (currentView == ListView)
          ]
          "LIST"
        -- For month view, we'll use current month
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{listEventsUri maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxGet_ [i|/#{listEventsUri maybeTagFilter (Just (uncurry MonthView currentMonth))}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            viewToggle (isMonthView currentView)
          ]
          "MONTH"
        -- For week view, we'll use current week
        Lucid.a_
          [ Lucid.id_ "view-control",
            Lucid.href_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxGet_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek) maybeTagFilter}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            viewToggle (isWeekView currentView)
          ]
          "WEEK"

    -- Filters
    Lucid.div_ [class_ $ base ["flex", "flex-wrap", "items-center", Tokens.gap4]] $ do
      Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textSm]] "FILTER:"
      -- Event tag filter
      Lucid.select_
        [ class_ $ base [Tokens.border2, "border-gray-600", Tokens.p2, "font-mono", Tokens.textSm],
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

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Events.Get.Templates.Controls
  ( renderViewControls,
  )
where

--------------------------------------------------------------------------------

import API.Links (eventsLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Time (MonthOfYear, UTCTime, Year, utctDay)
import Data.Time.Calendar.WeekDate (toWeekDate)
import Design (base, class_, when)
import Design.Tokens qualified as Tokens
import Domain.Types.PageView (PageView (..), isMonthView, isWeekView)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- URL helpers
eventsGetWeekUrl :: Year -> Int -> Links.URI
eventsGetWeekUrl year weekNum =
  Links.linkURI $ eventsLinks.list (Just $ WeekView year weekNum)

utcTimeToYearWeek :: UTCTime -> (Year, Int)
utcTimeToYearWeek utc =
  let (year, weekNum, _) = toWeekDate (utctDay utc)
   in (year, weekNum)

listEventsUri :: Maybe PageView -> Links.URI
listEventsUri = Links.linkURI . eventsLinks.list

--------------------------------------------------------------------------------

-- | Shared view controls component
renderViewControls :: UTCTime -> PageView -> (Year, MonthOfYear) -> Lucid.Html ()
renderViewControls currentTime currentView currentMonth = do
  let currentWeek = utcTimeToYearWeek currentTime
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
          Lucid.href_ [i|/#{listEventsUri (Just ListView)}|],
          hxGet_ [i|/#{listEventsUri (Just ListView)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          viewToggle (currentView == ListView)
        ]
        "LIST"
      -- For month view, we'll use current month
      Lucid.a_
        [ Lucid.id_ "view-control",
          Lucid.href_ [i|/#{listEventsUri (Just (uncurry MonthView currentMonth))}|],
          hxGet_ [i|/#{listEventsUri (Just (uncurry MonthView currentMonth))}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          viewToggle (isMonthView currentView)
        ]
        "MONTH"
      -- For week view, we'll use current week
      Lucid.a_
        [ Lucid.id_ "view-control",
          Lucid.href_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek)}|],
          hxGet_ [i|/#{eventsGetWeekUrl (fst currentWeek) (snd currentWeek)}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          viewToggle (isWeekView currentView)
        ]
        "WEEK"

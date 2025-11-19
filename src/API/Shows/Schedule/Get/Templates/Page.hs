{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (showsGetLink, showsScheduleGetLink)
import API.Shows.Get.Templates.ScheduleView (renderScheduleView)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (DayOfWeek)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> DayOfWeek -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek maybeError = do
  -- Header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 text-center w-full"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "SHOW SCHEDULE"
    Lucid.p_ [Lucid.class_ "text-lg text-gray-600 mb-6"] "Weekly broadcast schedule for KPBJ 95.9FM"

  -- Navigation Tabs
  renderTabs

  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-800 p-6 mb-8 text-center"] $ do
      Lucid.p_ [Lucid.class_ "text-red-800 font-bold"] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [Lucid.class_ "mb-8 w-full"] $ do
        renderScheduleView scheduledShows currentDayOfWeek

-- | Render navigation tabs
renderTabs :: Lucid.Html ()
renderTabs = do
  Lucid.div_ [Lucid.class_ "mb-8 w-full border-b-2 border-gray-800"] $ do
    Lucid.nav_ [Lucid.class_ "flex gap-8"] $ do
      -- Schedule tab (active)
      Lucid.a_
        [ Lucid.href_ [i|/#{showsScheduleGetUrl}|],
          hxGet_ [i|/#{showsScheduleGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "py-3 px-4 font-bold uppercase border-b-2 border-gray-800 bg-white -mb-0.5"
        ]
        "Schedule"

      -- All Shows tab (inactive)
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "py-3 px-4 font-bold uppercase text-gray-600 hover:text-gray-800"
        ]
        "All Shows"
  where
    showsScheduleGetUrl :: Links.URI
    showsScheduleGetUrl = Links.linkURI showsScheduleGetLink

    showsGetUrl :: Links.URI
    showsGetUrl = Links.linkURI $ showsGetLink Nothing Nothing Nothing Nothing

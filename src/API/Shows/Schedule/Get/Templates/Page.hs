{-# LANGUAGE QuasiQuotes #-}

module API.Shows.Schedule.Get.Templates.Page (template) where

--------------------------------------------------------------------------------

import API.Links (showsLinks)
import API.Shows.Get.Templates.ScheduleView (renderScheduleView)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (Day, DayOfWeek, TimeOfDay)
import Design.Tokens qualified as Tokens
import Domain.Types.WeekOffset (WeekOffset)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Lucid.Responsive (cls)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Schedule page template
template :: [ShowSchedule.ScheduledShowWithDetails] -> Maybe DayOfWeek -> Maybe TimeOfDay -> WeekOffset -> Day -> Maybe Text -> Lucid.Html ()
template scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart maybeError = do
  -- Header
  Lucid.section_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p8, Tokens.mb8, "text-center", Tokens.fullWidth]] $ do
    Lucid.h1_ [Lucid.class_ Tokens.heading2xl] "SHOW SCHEDULE"
    Lucid.p_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.textGray600, Tokens.mb6]] "Weekly broadcast schedule for KPBJ 95.9FM"

  -- Navigation Tabs
  renderTabs

  -- Error message if present
  case maybeError of
    Just errorMsg -> Lucid.div_ [Lucid.class_ $ cls ["bg-red-100", Tokens.border2, "border-red-800", Tokens.p6, Tokens.mb8, "text-center"]] $ do
      Lucid.p_ [Lucid.class_ $ cls ["text-red-800", Tokens.fontBold]] $ Lucid.toHtml errorMsg
    Nothing -> do
      -- Schedule View
      Lucid.section_ [Lucid.class_ $ cls [Tokens.mb8, Tokens.fullWidth]] $ do
        renderScheduleView scheduledShows currentDayOfWeek currentTimeOfDay weekOffset weekStart

-- | Render navigation tabs
renderTabs :: Lucid.Html ()
renderTabs = do
  Lucid.div_ [Lucid.class_ $ cls [Tokens.mb8, Tokens.fullWidth, "border-b-2", "border-gray-800"]] $ do
    Lucid.nav_ [Lucid.class_ $ cls ["flex", Tokens.gap8]] $ do
      -- Schedule tab (active)
      Lucid.a_
        [ Lucid.href_ [i|/#{showsScheduleGetUrl}|],
          hxGet_ [i|/#{showsScheduleGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", "border-b-2", "border-gray-800", Tokens.bgWhite, "-mb-0.5"]
        ]
        "Schedule"

      -- All Shows tab (inactive)
      Lucid.a_
        [ Lucid.href_ [i|/#{showsGetUrl}|],
          hxGet_ [i|/#{showsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ $ cls ["py-3", Tokens.px4, Tokens.fontBold, "uppercase", Tokens.textGray600, "hover:text-gray-800"]
        ]
        "All Shows"
  where
    showsScheduleGetUrl :: Links.URI
    showsScheduleGetUrl = Links.linkURI $ showsLinks.schedule Nothing

    showsGetUrl :: Links.URI
    showsGetUrl = Links.linkURI $ showsLinks.list Nothing Nothing Nothing Nothing

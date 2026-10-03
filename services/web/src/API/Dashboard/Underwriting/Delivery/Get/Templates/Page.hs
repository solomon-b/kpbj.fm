{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The underwriting delivery report.
--
-- One row per underwriter: the days it was active in the month, the most it
-- could air in them, and what aired. Each row expands to list its airings.
module API.Dashboard.Underwriting.Delivery.Get.Templates.Page
  ( DeliveryRow (..),
    template,
    maxAirings,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritingLinks)
import API.Types (DashboardUnderwritingRoutes (..))
import Control.Monad (forM_)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, addGregorianMonthsClip, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.BreakPlanner (underwritingDailyCap)
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | One underwriter's delivery for the month.
data DeliveryRow = DeliveryRow
  { drUnderwriter :: Underwriters.Model,
    -- | Days of the month on which one of its announcements may air.
    drDaysActive :: Int,
    -- | Title and start time of each airing, in time order.
    drAirings :: [(Text, UTCTime)]
  }

-- | The most an underwriter can air in its active days.
--
-- Fewer is normal on a day with fewer show breaks than underwriters want.
maxAirings :: DeliveryRow -> Int
maxAirings row = underwritingDailyCap * row.drDaysActive

-- | The report page.
template ::
  -- | First day of the month shown
  Day ->
  [DeliveryRow] ->
  Lucid.Html ()
template firstDay rows =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $ do
    renderMonthNav firstDay
    if null rows
      then
        Lucid.p_
          [class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]]
          "No underwriting was active or aired this month."
      else Lucid.table_ [class_ $ base ["w-full"]] $ do
        Lucid.thead_ $
          Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted]] $
            forM_ ["Underwriter", "Days Active", "Maximum", "Aired"] $ \h ->
              Lucid.th_ [class_ $ base [Tokens.p4, "text-left", Tokens.textSm, Tokens.fontBold]] h
        Lucid.tbody_ $ mapM_ renderRow rows

-- | The month name, with links to the month before and the month after.
renderMonthNav :: Day -> Lucid.Html ()
renderMonthNav firstDay =
  Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.gap4, Tokens.p4]] $ do
    monthLink (addGregorianMonthsClip (-1) firstDay) "← PREVIOUS MONTH"
    Lucid.h2_ [class_ $ base [Tokens.textLg, Tokens.fontBold]] $
      Lucid.toHtml (formatTime defaultTimeLocale "%B %Y" firstDay)
    monthLink (addGregorianMonthsClip 1 firstDay) "NEXT MONTH →"
  where
    monthLink :: Day -> Text -> Lucid.Html ()
    monthLink target label =
      let uri = Links.linkURI (dashboardUnderwritingLinks.delivery (Just (Text.pack (formatTime defaultTimeLocale "%Y-%m" target))))
       in Lucid.a_
            [ Lucid.href_ [i|/#{uri}|],
              class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
            ]
            (Lucid.toHtml label)

renderRow :: DeliveryRow -> Lucid.Html ()
renderRow row = do
  Lucid.tr_ $ do
    cell (Lucid.toHtml row.drUnderwriter.uwName)
    cell (Lucid.toHtml (show row.drDaysActive))
    cell (Lucid.toHtml (show (maxAirings row)))
    cell (Lucid.toHtml (show (length row.drAirings)))
  Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted]] $
    Lucid.td_ [Lucid.colspan_ "4", class_ $ base [Tokens.px4, "pb-4", Tokens.textSm]] $
      Lucid.details_ $ do
        Lucid.summary_ [class_ $ base [Tokens.fgMuted, "cursor-pointer"]] "Airings"
        if null row.drAirings
          then Lucid.p_ [class_ $ base [Tokens.fgMuted]] "None yet."
          else Lucid.ul_ $
            forM_ row.drAirings $ \(title, startedAt) ->
              Lucid.li_ $ Lucid.toHtml (formatAiring startedAt <> "  " <> title)
  where
    cell = Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]]

-- | The Pacific date and time of an airing, for example @Feb 03 10:28 AM@.
formatAiring :: UTCTime -> Text
formatAiring = Text.pack . formatTime defaultTimeLocale "%b %d %I:%M %p" . utcToPacific

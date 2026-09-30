{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

-- | The underwriting delivery report.
--
-- One row per underwriter: spots owed for the month, spots aired, and whether
-- delivery keeps pace with the days gone. Each row expands to list its airings.
module API.Dashboard.Underwriting.Delivery.Get.Templates.Page
  ( DeliveryRow (..),
    template,
    isBehind,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritingLinks)
import API.Types (DashboardUnderwritingRoutes (..))
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, UTCTime, addGregorianMonthsClip, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | One underwriter's delivery for the month.
data DeliveryRow = DeliveryRow
  { drUnderwriter :: Underwriters.Model,
    -- | Spots per month across the underwriter's orders that run in the month.
    drSpotsPerMonth :: Int64,
    -- | Spots owed in the month. An order that runs part of the month owes
    -- that part of its monthly spots, rounded up.
    drOwed :: Int64,
    -- | Spots due by the end of yesterday, at an even pace over the days each
    -- order runs. Today does not count until it ends.
    drDue :: Int64,
    -- | Title and start time of each airing, in time order.
    drAirings :: [(Text, UTCTime)]
  }

-- | Whether delivery is behind pace.
isBehind :: DeliveryRow -> Bool
isBehind row = fromIntegral (length row.drAirings) < row.drDue

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
          "No underwriting owed or aired this month."
      else Lucid.table_ [class_ $ base ["w-full"]] $ do
        Lucid.thead_ $
          Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted]] $
            forM_ ["Underwriter", "Spots / Month", "Owed", "Aired", "Status"] $ \h ->
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
    cell (Lucid.toHtml (show row.drSpotsPerMonth))
    cell (Lucid.toHtml (show row.drOwed))
    cell (Lucid.toHtml (show (length row.drAirings)))
    cell $
      if isBehind row
        then Lucid.span_ [class_ $ base [Tokens.px3, "py-1", "rounded", Tokens.warningBg, Tokens.warningText]] "Behind"
        else Lucid.span_ [class_ $ base [Tokens.px3, "py-1", "rounded", Tokens.successBg, Tokens.successText]] "On pace"
  Lucid.tr_ [class_ $ base ["border-b-2", Tokens.borderMuted]] $
    Lucid.td_ [Lucid.colspan_ "5", class_ $ base [Tokens.px4, "pb-4", Tokens.textSm]] $
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

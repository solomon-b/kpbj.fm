{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.MissingEpisodes.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.HTMX
import OrphanInstances.DayOfWeek (dayOfWeekName)
import OrphanInstances.TimeOfDay (formatTimeOfDay)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Missing episodes list template.
template ::
  [ShowSchedule.ShowMissingEpisode] ->
  Lucid.Html ()
template missingEpisodes = do
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null missingEpisodes
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "missing-episodes-table-body",
              itcHeaders =
                [ ColumnHeader "Show" AlignLeft,
                  ColumnHeader "Host(s)" AlignLeft,
                  ColumnHeader "Scheduled Date" AlignLeft,
                  ColumnHeader "Day" AlignLeft,
                  ColumnHeader "Time Slot" AlignLeft
                ],
              itcNextPageUrl = Nothing,
              itcPaginationConfig = Nothing
            }
          (mapM_ renderRow missingEpisodes)

renderRow :: ShowSchedule.ShowMissingEpisode -> Lucid.Html ()
renderRow sme =
  let showId = sme.smeShowId
      showSlug = sme.smeShowSlug
      showDate = sme.smeShowDate
      showDetailUri = Links.linkURI $ dashboardShowsLinks.detail showId showSlug Nothing
      showDetailUrl = [i|/#{showDetailUri}|] :: Text
      rowId = [i|missing-#{showId}-#{formatDateCompact showDate}|] :: Text
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ [class_ $ base [Tokens.p4, "cursor-pointer"], hxGet_ showDetailUrl, hxTarget_ "#main-content", hxPushUrl_ "true"] $
          Lucid.span_ [Lucid.class_ Tokens.fontBold] $
            Lucid.toHtml sme.smeShowTitle

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml sme.smeHostNames

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (formatDate sme.smeShowDate)

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (dayOfWeekName sme.smeDayOfWeek)

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml $
              formatTimeOfDay sme.smeStartTime <> " - " <> formatTimeOfDay sme.smeEndTime

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "All upcoming shows have episodes uploaded."

-- | Format a Day as "Feb 15, 2026".
formatDate :: Day -> String
formatDate = formatTime defaultTimeLocale "%b %d, %Y"

-- | Format a Day compactly for use in element IDs.
formatDateCompact :: Day -> String
formatDateCompact = formatTime defaultTimeLocale "%Y%m%d"

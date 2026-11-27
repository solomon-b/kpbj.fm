{-# LANGUAGE QuasiQuotes #-}

-- | The missed episodes report.
--
-- The page counts the finished slots of a range. It shows one row per host
-- with the slots that the host missed, and then each missed slot.
module API.Dashboard.MissingEpisodes.Past.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardMissingEpisodesPastLink, dashboardShowsLinks, dashboardUsersLinks)
import API.Types
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.MissingEpisodesNav qualified as MissingEpisodesNav
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), clickableCellAttrs, renderTableWithBodyId, rowAttrs)
import Control.Monad (forM_)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Attendance (AttendanceRange, HostAttendance (..), Tally (..), hostAttendance, missedPercent, missedSlots, rangeLabel, tallySlot)
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.ShowSchedule (PastSlot (..), PastSlotHost (..), TemplateId (..))
import Lucid qualified
import Servant (toUrlPiece)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Missed episodes report template.
template ::
  -- | The range on screen
  AttendanceRange ->
  -- | The finished slots of the range, newest first
  [PastSlot] ->
  Lucid.Html ()
template range slots = do
  MissingEpisodesNav.render MissingEpisodesNav.Past
  renderSummary range (foldMap tallySlot slots)
  if null slots
    then renderEmptyState
    else do
      renderHostTable (hostAttendance slots)
      renderMissedTable (missedSlots slots)

--------------------------------------------------------------------------------
-- Summary

-- | The range links, the total count, and the rule for a missed slot.
renderSummary :: AttendanceRange -> Tally -> Lucid.Html ()
renderSummary range tally =
  Card.section Card.Detail [Tokens.mb4] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", "flex-wrap", Tokens.gap4, Tokens.mb2]] $ do
      Lucid.p_ [class_ $ base [Tokens.textLg, Tokens.fontBold]] $
        Lucid.toHtml (missedOfTotal tally <> " slots missed " <> rangeLabel range)
      Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $
        forM_ [minBound .. maxBound] (rangeLink range)
    Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
      "A slot is missed when no new episode of the show aired in it. A slot that aired a rerun is missed. "
        <> "A slot with two hosts counts for each host. The count starts at the first episode in the air record, "
        <> "and it skips a slot with no air record."

rangeLink ::
  -- | The range on screen
  AttendanceRange ->
  -- | The range of the link
  AttendanceRange ->
  Lucid.Html ()
rangeLink current range =
  Button.render
    (Button.defaults & style & Button.small & Button.dashboardLink [i|/#{uri}|])
    (Lucid.toHtml (Text.toUpper (toUrlPiece range)))
  where
    uri :: Links.URI
    uri = Links.linkURI (dashboardMissingEpisodesPastLink (Just range))

    style :: Button.Spec -> Button.Spec
    style
      | range == current = Button.primary
      | otherwise = Button.secondary

renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Card.card Card.EmptyState [] $
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted]] "No finished slots in this range."

--------------------------------------------------------------------------------
-- Hosts

-- | One row per host, with the most missed slots first.
renderHostTable :: [HostAttendance] -> Lucid.Html ()
renderHostTable hosts =
  Card.section Card.TablePanel [Tokens.mb8] $ do
    panelTitle "BY HOST"
    renderTableWithBodyId
      "missed-episodes-hosts-table-body"
      TableConfig
        { headers =
            [ ColumnHeader "Host" AlignLeft,
              ColumnHeader "Show(s)" AlignLeft,
              ColumnHeader "Missed" AlignLeft,
              ColumnHeader "Rate" AlignLeft
            ],
          wrapperClass = "overflow-x-auto",
          tableClass = "w-full"
        }
      (mapM_ renderHostRow hosts)

renderHostRow :: HostAttendance -> Lucid.Html ()
renderHostRow host =
  let userDetailUri = Links.linkURI $ dashboardUsersLinks.detail host.haUserId
      rowId = "missed-host-" <> display host.haUserId
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ (clickableCellAttrs [i|/#{userDetailUri}|]) $
          Lucid.span_ [class_ $ base [Tokens.fontBold]] $
            Lucid.toHtml (display host.haDisplayName)
        cell $ Lucid.toHtml (Text.intercalate ", " (Set.toList host.haShowTitles))
        cell $ Lucid.toHtml (missedOfTotal host.haTally)
        cell $ Lucid.toHtml (maybe "" (\percent -> display percent <> "%") (missedPercent host.haTally))

--------------------------------------------------------------------------------
-- Missed slots

-- | One row per missed slot, newest first.
renderMissedTable :: [PastSlot] -> Lucid.Html ()
renderMissedTable missed =
  Card.section Card.TablePanel [Tokens.mb8] $ do
    panelTitle "MISSED SLOTS"
    if null missed
      then
        Lucid.p_
          [class_ $ base [Tokens.p4, Tokens.textSm, Tokens.fgMuted]]
          "Every slot in this range aired a new episode."
      else
        renderTableWithBodyId
          "missed-episodes-slots-table-body"
          TableConfig
            { headers =
                [ ColumnHeader "Date" AlignLeft,
                  ColumnHeader "Time" AlignLeft,
                  ColumnHeader "Show" AlignLeft,
                  ColumnHeader "Host(s)" AlignLeft
                ],
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          (mapM_ renderMissedRow missed)

renderMissedRow :: PastSlot -> Lucid.Html ()
renderMissedRow slot =
  let showDetailUri = Links.linkURI $ dashboardShowsLinks.detail slot.psShowId slot.psShowSlug Nothing
      rowId =
        "missed-slot-"
          <> display (unTemplateId slot.psTemplateId)
          <> "-"
          <> Text.pack (formatTime defaultTimeLocale "%Y%m%d" slot.psAirDate)
      start = utcToPacific slot.psStart
   in Lucid.tr_ (rowAttrs rowId) $ do
        cell $ Lucid.toHtml (formatTime defaultTimeLocale "%a, %b %d, %Y" start)
        cell $ Lucid.toHtml (formatTime defaultTimeLocale "%l:%M %p" start)
        Lucid.td_ (clickableCellAttrs [i|/#{showDetailUri}|]) $
          Lucid.span_ [class_ $ base [Tokens.fontBold]] $
            Lucid.toHtml slot.psShowTitle
        cell $ case slot.psHosts of
          [] -> Lucid.span_ [class_ $ base [Tokens.fgMuted]] "No host"
          hosts -> Lucid.toHtml (Text.intercalate ", " (fmap (display . (.pshDisplayName)) hosts))

--------------------------------------------------------------------------------

-- | A tally in words, for example "3 of 20".
missedOfTotal :: Tally -> Text
missedOfTotal tally = display tally.tMissed <> " of " <> display tally.tTotal

panelTitle :: Text -> Lucid.Html ()
panelTitle = Lucid.h2_ [class_ $ base [Tokens.p4, Tokens.textLg, Tokens.fontBold]] . Lucid.toHtml

cell :: Lucid.Html () -> Lucid.Html ()
cell = Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]]

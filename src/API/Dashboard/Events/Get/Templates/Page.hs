{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Get.Templates.Page
  ( template,
    renderEventRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks, eventsLinks)
import API.Types
import Component.ActionsDropdown qualified as ActionsDropdown
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    clickableCellAttrs,
    renderIndexTable,
    rowAttrs,
  )
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Servant.Links qualified as Links

-- | Events list template
template ::
  [Events.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template events currentPage hasMore = do
  -- Events table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8]] $
    if null events
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "events-table-body",
              itcHeaders =
                [ ColumnHeader "Title" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Start Date" AlignLeft,
                  ColumnHeader "Location" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                      pcCurrentPage = currentPage
                    }
            }
          (mapM_ renderEventRow events)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEventsLinks.list (Just (currentPage + 1))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardEventsLinks.list (Just (currentPage - 1))

renderEventRow :: Events.Model -> Lucid.Html ()
renderEventRow event =
  let eventId = event.emId
      eventSlug = event.emSlug
      title = event.emTitle
      status = event.emStatus
      startsAt = event.emStartsAt
      locationName = event.emLocationName
      detailUri = Links.linkURI $ dashboardEventsLinks.detail eventId eventSlug
      detailUrl = [i|/#{detailUri}|]
      editUrl = Links.linkURI $ dashboardEventsLinks.editGet eventId eventSlug
      viewUrl = Links.linkURI $ eventsLinks.detailWithSlug eventId eventSlug
      deleteUrl = Links.linkURI $ dashboardEventsLinks.delete eventId eventSlug
      eventIdText = display eventId
      rowId = [i|event-row-#{eventIdText}|]
      deleteConfirmMessage =
        "Are you sure you want to delete the event \""
          <> display title
          <> "\"? This action cannot be undone."
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          Lucid.td_ (clickableCellAttrs detailUrl) $
            Lucid.span_ [Lucid.class_ Tokens.fontBold] $
              Lucid.toHtml title

          Lucid.td_ (clickableCellAttrs detailUrl) $
            renderStatusBadge status

          Lucid.td_ (clickableCellAttrs detailUrl) $ do
            Lucid.div_ [Lucid.class_ Tokens.textSm] $ Lucid.toHtml (formatDateTime startsAt)

          Lucid.td_ (clickableCellAttrs detailUrl) $ do
            Lucid.div_ [Lucid.class_ Tokens.textSm] $ Lucid.toHtml locationName

          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            ActionsDropdown.render
              [ ActionsDropdown.navigateAction "edit" "Edit" [i|/#{editUrl}|],
                ActionsDropdown.navigateAction "view" "View" [i|/#{viewUrl}|],
                ActionsDropdown.htmxDeleteAction
                  "delete"
                  "Delete"
                  [i|/#{deleteUrl}|]
                  "#main-content"
                  ActionsDropdown.SwapInnerHTML
                  deleteConfirmMessage
              ]

renderStatusBadge :: Events.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Events.Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Events.Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")

  Lucid.span_
    [class_ $ base ["inline-block", "px-3", "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
    $ Lucid.toHtml statusText

renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base ["bg-gray-50", Tokens.border2, "border-gray-300", "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] "No events found."
    Lucid.p_ [class_ $ base ["text-gray-500", "mt-2"]] "Create a new event to get started."

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

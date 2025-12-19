{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Get.Templates.Page where

--------------------------------------------------------------------------------

import API.Links (dashboardEventsLinks, eventsLinks)
import API.Types
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

-- | Events list template
template ::
  [Events.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template events currentPage hasMore = do
  -- Events table or empty state
  if null events
    then renderEmptyState
    else do
      Lucid.div_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8, Tokens.fullWidth]] $
        Lucid.table_ [Lucid.class_ Tokens.fullWidth] $ do
          Lucid.thead_ [class_ $ base [Tokens.bgGray800, Tokens.textWhite]] $
            Lucid.tr_ $ do
              Lucid.th_ [class_ $ base [Tokens.p4, "text-left"]] "Title"
              Lucid.th_ [class_ $ base [Tokens.p4, "text-left"]] "Status"
              Lucid.th_ [class_ $ base [Tokens.p4, "text-left"]] "Start Date"
              Lucid.th_ [class_ $ base [Tokens.p4, "text-left"]] "Location"
              Lucid.th_ [class_ $ base [Tokens.p4, "text-center", "w-24"]] ""
          Lucid.tbody_ $
            mapM_ renderEventRow events

      renderPagination currentPage hasMore

renderEventRow :: Events.Model -> Lucid.Html ()
renderEventRow event =
  let eventId = event.emId
      eventSlug = event.emSlug
      title = event.emTitle
      status = event.emStatus
      startsAt = event.emStartsAt
      locationName = event.emLocationName
      detailUrl = Links.linkURI $ dashboardEventsLinks.detail eventId eventSlug
      editUrl = Links.linkURI $ dashboardEventsLinks.editGet eventId eventSlug
      viewUrl = Links.linkURI $ eventsLinks.detailWithSlug eventId eventSlug
      deleteUrl = Links.linkURI $ dashboardEventsLinks.delete eventId eventSlug
      cellLinkAttrs =
        [ class_ $ base [Tokens.p4, "cursor-pointer"],
          hxGet_ [i|/#{detailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
      eventIdText = display eventId
      rowId = [i|event-row-#{eventIdText}|]
      deleteConfirmMessage =
        "Are you sure you want to delete the event \""
          <> display title
          <> "\"? This action cannot be undone."
   in do
        Lucid.tr_
          [ Lucid.id_ rowId,
            class_ $ base [Tokens.border2, "border-gray-200", "hover:bg-gray-50"]
          ]
          $ do
            Lucid.td_ cellLinkAttrs $
              Lucid.span_ [Lucid.class_ Tokens.fontBold] $
                Lucid.toHtml title

            Lucid.td_ cellLinkAttrs $
              renderStatusBadge status

            Lucid.td_ cellLinkAttrs $ do
              Lucid.div_ [Lucid.class_ Tokens.textSm] $ Lucid.toHtml (formatDateTime startsAt)

            Lucid.td_ cellLinkAttrs $ do
              Lucid.div_ [Lucid.class_ Tokens.textSm] $ Lucid.toHtml locationName

            Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]]
              $ Lucid.select_
                [ class_ $ base ["p-2", "border", "border-gray-400", "text-xs", Tokens.bgWhite],
                  xData_ "{}",
                  xOnChange_
                    [i|
                    const action = $el.value;
                    $el.value = '';
                    if (action === 'edit') {
                      window.location.href = '/#{editUrl}';
                    } else if (action === 'view') {
                      window.location.href = '/#{viewUrl}';
                    } else if (action === 'delete') {
                      if (confirm('#{deleteConfirmMessage}')) {
                        htmx.ajax('DELETE', '/#{deleteUrl}', {target: '\#main-content', swap: 'innerHTML'});
                      }
                    }
                  |],
                  xOnClick_ "event.stopPropagation()"
                ]
              $ do
                Lucid.option_ [Lucid.value_ ""] "Actions..."
                Lucid.option_ [Lucid.value_ "edit"] "Edit"
                Lucid.option_ [Lucid.value_ "view"] "View"
                Lucid.option_ [Lucid.value_ "delete"] "Delete"

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

renderPagination :: Int64 -> Bool -> Lucid.Html ()
renderPagination currentPage hasMore = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center"]] $ do
    -- Previous button
    if currentPage > 1
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{prevPageUrl}|],
            hxGet_ [i|/#{prevPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "<- PREVIOUS"
      else
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [class_ $ base [Tokens.textGray600, Tokens.fontBold]] $
      Lucid.toHtml $
        "Page " <> show currentPage

    -- Next button
    if hasMore
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{nextPageUrl}|],
            hxGet_ [i|/#{nextPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [Tokens.bgGray800, Tokens.textWhite, Tokens.px6, "py-3", Tokens.fontBold, "hover:bg-gray-700"]
          ]
          "NEXT ->"
      else
        Lucid.div_ [] mempty
  where
    prevPageUrl = Links.linkURI $ dashboardEventsLinks.list (Just (currentPage - 1))
    nextPageUrl = Links.linkURI $ dashboardEventsLinks.list (Just (currentPage + 1))

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

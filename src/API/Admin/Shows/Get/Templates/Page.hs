{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Admin.Shows.Get.Templates.Page where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (adminShowsGetLink, adminShowsNewGetLink, showGetLink)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Filter (Filter (..))
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Extras
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

template ::
  [Shows.ShowWithHostInfo] ->
  Int64 ->
  Bool ->
  Maybe Text ->
  Maybe Shows.Status ->
  Lucid.Html ()
template theShowList currentPage hasMore maybeQuery maybeStatusFilter = do
  -- Page header
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
      Lucid.div_ [] $ do
        Lucid.h1_ [Lucid.class_ "text-3xl font-bold"] "SHOW MANAGEMENT"
        Lucid.p_ [Lucid.class_ "text-gray-600 mt-2"] "Manage all shows and their hosts"
      Lucid.a_
        [ Lucid.href_ [i|/#{adminShowsNewGetUrl}|],
          hxGet_ [i|/#{adminShowsNewGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "+ NEW SHOW"

  -- Filters section
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-8 w-full"] $ do
    Lucid.form_
      [ hxGet_ "/admin/shows",
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "flex flex-col md:flex-row gap-4 items-end"
      ]
      $ do
        -- Search input
        Lucid.div_ [Lucid.class_ "flex-1"] $ do
          Lucid.label_ [Lucid.for_ "search", Lucid.class_ "block font-bold mb-2"] "Search"
          Lucid.input_
            [ Lucid.type_ "search",
              Lucid.name_ "q",
              Lucid.id_ "search",
              Lucid.value_ (fromMaybe "" maybeQuery),
              Lucid.placeholder_ "Search by title, description, or genre...",
              Lucid.class_ "w-full p-3 border-2 border-gray-800"
            ]

        -- Status filter
        Lucid.div_ [Lucid.class_ "flex-1"] $ do
          Lucid.label_ [Lucid.for_ "status", Lucid.class_ "block font-bold mb-2"] "Status"
          Lucid.select_
            [ Lucid.name_ "status",
              Lucid.id_ "status",
              Lucid.class_ "w-full p-3 border-2 border-gray-800"
            ]
            $ do
              Lucid.option_ [Lucid.value_ "", selectedIf (isNothing maybeStatusFilter)] "All Statuses"
              Lucid.option_ [Lucid.value_ "active", selectedIf (maybeStatusFilter == Just Shows.Active)] "Active"
              Lucid.option_ [Lucid.value_ "inactive", selectedIf (maybeStatusFilter == Just Shows.Inactive)] "Inactive"

        -- Submit button
        Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
            ]
            "SEARCH"
          when (isJust maybeQuery || isJust maybeStatusFilter) $
            Lucid.a_
              [ Lucid.href_ "/admin/shows",
                hxGet_ "/admin/shows",
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-300 text-gray-800 px-6 py-3 font-bold hover:bg-gray-400"
              ]
              "CLEAR"

  -- Shows table or empty state
  if null theShowList
    then renderEmptyState maybeQuery
    else do
      Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 overflow-hidden mb-8 w-full"] $
        Lucid.table_ [Lucid.class_ "w-full"] $ do
          Lucid.thead_ [Lucid.class_ "bg-gray-800 text-white"] $
            Lucid.tr_ $ do
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Title"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Status"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Hosts"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Genre"
              Lucid.th_ [Lucid.class_ "p-4 text-left"] "Duration"
          Lucid.tbody_ $
            mapM_ renderShowRow theShowList

      renderPagination currentPage hasMore maybeQuery maybeStatusFilter
  where
    selectedIf condition = if condition then Lucid.selected_ "selected" else mempty
    when cond action = if cond then action else mempty
    adminShowsNewGetUrl = Links.linkURI adminShowsNewGetLink

renderShowRow :: Shows.ShowWithHostInfo -> Lucid.Html ()
renderShowRow showInfo =
  let showDetailUrl = Links.linkURI $ showGetLink showInfo.swhiSlug
      cellLinkAttrs =
        [ Lucid.class_ "p-4 cursor-pointer",
          hxGet_ [i|/#{showDetailUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true"
        ]
   in do
        Lucid.tr_
          [Lucid.class_ "border-b-2 border-gray-200 hover:bg-gray-50"]
          $ do
            Lucid.td_ cellLinkAttrs $ do
              Lucid.span_ [Lucid.class_ "font-bold"] $
                Lucid.toHtml showInfo.swhiTitle
              Lucid.div_ [Lucid.class_ "text-sm text-gray-500"] $
                Lucid.toHtml $
                  "/" <> display showInfo.swhiSlug

            Lucid.td_ cellLinkAttrs $
              renderStatusBadge showInfo.swhiStatus

            Lucid.td_ cellLinkAttrs $ do
              case showInfo.swhiHostNames of
                Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400 italic"] "No hosts"
                Just names -> do
                  Lucid.span_ [] $ Lucid.toHtml names
                  Lucid.span_ [Lucid.class_ "text-gray-500 ml-2"] $
                    Lucid.toHtml $
                      "(" <> show showInfo.swhiHostCount <> ")"

            Lucid.td_ cellLinkAttrs $
              Lucid.toHtml $
                fromMaybe "-" showInfo.swhiGenre

            Lucid.td_ cellLinkAttrs $
              case showInfo.swhiDurationMinutes of
                Nothing -> "-"
                Just mins -> Lucid.toHtml $ show mins <> " min"

renderStatusBadge :: Shows.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Shows.Active -> ("bg-green-100", "text-green-800", "Active") :: (Text, Text, Text)
        Shows.Inactive -> ("bg-gray-100", "text-gray-800", "Inactive")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml statusText

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState maybeQuery = do
  Lucid.div_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-12 text-center"] $ do
    Lucid.p_ [Lucid.class_ "text-xl text-gray-600"] $
      case maybeQuery of
        Nothing -> "No shows found. Create your first show!"
        Just query -> Lucid.toHtml $ "No shows found matching \"" <> query <> "\"."

renderPagination :: Int64 -> Bool -> Maybe Text -> Maybe Shows.Status -> Lucid.Html ()
renderPagination currentPage hasMore (Just . Filter -> maybeQuery) (Just . Filter -> maybeStatusFilter) = do
  Lucid.div_ [Lucid.class_ "flex justify-between items-center"] $ do
    -- Previous button
    if currentPage > 1
      then
        Lucid.a_
          [ Lucid.href_ [i|/#{prevPageUrl}|],
            hxGet_ [i|/#{prevPageUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "<- PREVIOUS"
      else
        Lucid.div_ [] mempty

    -- Page indicator
    Lucid.span_ [Lucid.class_ "text-gray-600 font-bold"] $
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
            Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
          ]
          "NEXT ->"
      else
        Lucid.div_ [] mempty
  where
    prevPageUrl = Links.linkURI $ adminShowsGetLink (Just (currentPage - 1)) maybeQuery maybeStatusFilter
    nextPageUrl = Links.linkURI $ adminShowsGetLink (Just (currentPage + 1)) maybeQuery maybeStatusFilter

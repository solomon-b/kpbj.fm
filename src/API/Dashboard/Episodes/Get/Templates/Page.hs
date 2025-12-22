{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Get.Templates.Episode (renderEpisodeTableRow)
import API.Links (dashboardEpisodesLinks)
import API.Types
import Component.InfiniteScroll (renderEndOfContent, renderLoadingIndicator, renderSentinel)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTableWithBodyId)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Episodes dashboard template (stats are now in the top bar)
template ::
  UserMetadata.Model ->
  Maybe Shows.Model ->
  [Episodes.Model] ->
  Int64 ->
  Bool ->
  Lucid.Html ()
template userMeta selectedShow episodes currentPage hasMore =
  renderEpisodesSection userMeta selectedShow episodes currentPage hasMore

-- | Episodes table section
renderEpisodesSection :: UserMetadata.Model -> Maybe Shows.Model -> [Episodes.Model] -> Int64 -> Bool -> Lucid.Html ()
renderEpisodesSection userMeta selectedShow episodes currentPage hasMore = do
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden"]] $ do
    case episodes of
      [] ->
        Lucid.div_ [class_ $ base [Tokens.textGray600, "text-center", "p-8"]] $ do
          Lucid.p_ "No episodes uploaded yet."
          Lucid.p_ [class_ $ base [Tokens.textSm, "mt-2"]] "Use 'New Episode' to upload your first episode."
      _ ->
        case selectedShow of
          Nothing -> mempty
          Just showModel ->
            renderTableWithBodyId
              "episodes-table-body"
              TableConfig
                { headers =
                    [ ColumnHeader "EPISODE" AlignLeft,
                      ColumnHeader "SCHEDULED" AlignLeft,
                      ColumnHeader "STATUS" AlignLeft,
                      ColumnHeader "ACTIONS" AlignRight
                    ],
                  wrapperClass = "overflow-x-auto",
                  tableClass = "w-full"
                }
              $ do
                mapM_ (renderEpisodeTableRow userMeta showModel) episodes
                -- Sentinel row for infinite scroll
                if hasMore
                  then
                    Lucid.tr_ [Lucid.id_ "load-more-sentinel-row"] $
                      Lucid.td_ [Lucid.colspan_ "4"] $
                        renderSentinel [i|/#{nextPageUrl showModel}|] "#episodes-table-body"
                  else
                    Lucid.tr_ [] $
                      Lucid.td_ [Lucid.colspan_ "4"] $
                        renderEndOfContent

  -- Loading indicator (hidden by default, shown during HTMX requests)
  renderLoadingIndicator

  -- Fallback pagination for browsers without JavaScript
  Lucid.noscript_ $
    case selectedShow of
      Nothing -> mempty
      Just showModel -> renderPagination showModel currentPage hasMore
  where
    nextPageUrl :: Shows.Model -> Links.URI
    nextPageUrl showModel = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage + 1))

-- | Pagination controls for noscript fallback
renderPagination :: Shows.Model -> Int64 -> Bool -> Lucid.Html ()
renderPagination showModel currentPage hasMore = do
  Lucid.div_ [class_ $ base ["flex", "justify-between", "items-center", "mt-4"]] $ do
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
    prevPageUrl = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage - 1))
    nextPageUrl = Links.linkURI $ dashboardEpisodesLinks.list showModel.slug (Just (currentPage + 1))

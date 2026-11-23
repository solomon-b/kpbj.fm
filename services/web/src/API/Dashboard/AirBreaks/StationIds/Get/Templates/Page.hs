{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.AirBreaks.StationIds.Get.Templates.Page
  ( template,
    renderStationIdRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationIdsLinks)
import API.Types
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.InlineAudioPlayer (renderInlineAudioPlayer)
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Station IDs list template
template ::
  StorageBackend ->
  [StationIds.StationIdWithCreator] ->
  PageNumber ->
  Bool ->
  UserMetadata.Model ->
  Lucid.Html ()
template backend stationIds (PageNumber pageNum) hasMore _userMeta = do
  -- Station IDs table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null stationIds
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "station-ids-table-body",
              itcHeaders =
                [ ColumnHeader "Title" AlignLeft,
                  ColumnHeader "Creator" AlignLeft,
                  ColumnHeader "Created" AlignLeft,
                  ColumnHeader "Play" AlignCenter,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if pageNum > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                      pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                      pcCurrentPage = pageNum
                    }
            }
          (mapM_ (renderStationIdRow backend) stationIds)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardStationIdsLinks.list (Just (PageNumber (pageNum + 1)))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardStationIdsLinks.list (Just (PageNumber (pageNum - 1)))

-- | Render a single station ID row with audio player
renderStationIdRow :: StorageBackend -> StationIds.StationIdWithCreator -> Lucid.Html ()
renderStationIdRow backend stationId =
  let stationIdId = stationId.siwcId
      title = stationId.siwcTitle
      creatorName = stationId.siwcCreatorDisplayName
      createdAt = stationId.siwcCreatedAt
      audioPath = stationId.siwcAudioFilePath
      stationIdIdText = display stationIdId
      rowId = [i|station-id-row-#{stationIdIdText}|]
      deleteUrl = Links.linkURI $ dashboardStationIdsLinks.delete stationIdId
      deleteConfirmMessage =
        "Are you sure you want to delete the station ID \""
          <> display title
          <> "\"? This action cannot be undone."
      audioUrl = buildMediaUrl backend audioPath
   in do
        Lucid.tr_ (rowAttrs rowId) $ do
          -- Title cell
          Lucid.td_ [class_ $ base [Tokens.p4]] $
            Lucid.span_ [Lucid.class_ Tokens.fontBold] $
              Lucid.toHtml title

          -- Creator cell
          Lucid.td_ [class_ $ base [Tokens.p4]] $
            Lucid.span_ [Lucid.class_ Tokens.textSm] $
              Lucid.toHtml creatorName

          -- Created date cell
          Lucid.td_ [class_ $ base [Tokens.p4]] $
            Lucid.div_ [Lucid.class_ Tokens.textSm] $
              Lucid.toHtml (formatDateTime createdAt)

          -- Audio player cell
          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            renderInlineAudioPlayer audioUrl

          -- Actions cell
          Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
            Button.group $ do
              Button.downloadLink audioUrl
              Button.render
                ( Button.defaults
                    & Button.danger
                    & Button.with
                      [ hxDelete_ [i|/#{deleteUrl}|],
                        hxTarget_ ("#" <> rowId),
                        hxSwap_ "outerHTML",
                        hxConfirm_ deleteConfirmMessage
                      ]
                )
                "Delete"

-- | Render empty state when no station IDs exist
renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No station IDs uploaded yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Upload a new station ID to get started."

-- | Format a datetime for display
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.StationIds.Get.Templates.Page
  ( template,
    renderStationIdRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardStationIdsLinks)
import API.Types
import Component.ActionsDropdown qualified as ActionsDropdown
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
    rowAttrs,
  )
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Effects.Database.Tables.StationIds qualified as StationIds
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Station IDs list template
template ::
  [StationIds.StationIdWithCreator] ->
  PageNumber ->
  Bool ->
  UserMetadata.Model ->
  Lucid.Html ()
template stationIds (PageNumber pageNum) hasMore _userMeta = do
  -- Station IDs table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgWhite, "rounded", "overflow-hidden", Tokens.mb8]] $
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
          (mapM_ renderStationIdRow stationIds)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardStationIdsLinks.list (Just (PageNumber (pageNum + 1)))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardStationIdsLinks.list (Just (PageNumber (pageNum - 1)))

-- | Render a single station ID row with audio player
renderStationIdRow :: StationIds.StationIdWithCreator -> Lucid.Html ()
renderStationIdRow stationId =
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
      audioUrl = [i|/media/#{audioPath}|] :: Text
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
            renderInlineAudioPlayer audioUrl stationIdIdText

          -- Actions cell (Download link + Delete dropdown)
          Lucid.td_ [class_ $ base [Tokens.p4, "text-center", "flex", "items-center", "justify-center", Tokens.gap2]] $ do
            -- Download button
            Lucid.a_
              [ Lucid.href_ audioUrl,
                Lucid.download_ "",
                Lucid.class_ "p-2 rounded hover:bg-gray-100 dark:hover:bg-gray-700 transition-colors",
                Lucid.title_ "Download"
              ]
              $ Lucid.i_ [Lucid.class_ "fa-solid fa-download text-gray-600 dark:text-gray-400"] mempty
            -- Delete dropdown
            ActionsDropdown.render
              [ ActionsDropdown.htmxDeleteAction
                  "delete"
                  "Delete"
                  [i|/#{deleteUrl}|]
                  ("#" <> rowId)
                  ActionsDropdown.SwapOuterHTML
                  deleteConfirmMessage
              ]

-- | Render an inline audio player using Alpine.js
renderInlineAudioPlayer :: Text -> Text -> Lucid.Html ()
renderInlineAudioPlayer audioUrl _uniqueId =
  Lucid.div_
    [ xData_ [i|{ playing: false, audio: null }|],
      class_ $ base ["inline-flex", "items-center", Tokens.gap2]
    ]
    $ do
      -- Hidden audio element
      Lucid.audio_
        [ xRef_ "audioPlayer",
          Lucid.src_ audioUrl,
          Lucid.class_ "hidden"
        ]
        mempty
      -- Play/Pause button
      Lucid.button_
        [ Lucid.type_ "button",
          xOnClick_
            [i|
            if (!$refs.audioPlayer) return;
            if (playing) {
              $refs.audioPlayer.pause();
              playing = false;
            } else {
              $refs.audioPlayer.play();
              playing = true;
              $refs.audioPlayer.onended = () => { playing = false; };
            }
          |],
          class_ $ base ["p-2", "rounded", "hover:bg-gray-100 dark:hover:bg-gray-700", "transition-colors"]
        ]
        $ do
          -- Play icon (shown when not playing)
          Lucid.i_
            [ Lucid.class_ "fa-solid fa-play text-gray-600 dark:text-gray-400",
              Lucid.term "x-show" "!playing"
            ]
            mempty
          -- Pause icon (shown when playing)
          Lucid.i_
            [ Lucid.class_ "fa-solid fa-pause text-gray-600 dark:text-gray-400",
              Lucid.term "x-show" "playing"
            ]
            mempty

-- | Render empty state when no station IDs exist
renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No station IDs uploaded yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Upload a new station ID to get started."

-- | Format a datetime for display
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

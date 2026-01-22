{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.EphemeralUploads.Get.Templates.Page
  ( template,
    renderEphemeralUploadRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardEphemeralUploadsLinks)
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
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (xData_, xOnClick_, xRef_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Ephemeral uploads list template
template ::
  StorageBackend ->
  [EphemeralUploads.EphemeralUploadWithCreator] ->
  PageNumber ->
  Bool ->
  UserMetadata.Model ->
  Lucid.Html ()
template backend ephemeralUploads (PageNumber pageNum) hasMore _userMeta = do
  -- Ephemeral uploads table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgWhite, Tokens.cardBorder, "overflow-hidden", Tokens.mb8]] $
    if null ephemeralUploads
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "ephemeral-uploads-table-body",
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
          (mapM_ (renderEphemeralUploadRow backend) ephemeralUploads)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list (Just (PageNumber (pageNum + 1)))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list (Just (PageNumber (pageNum - 1)))

-- | Render a single ephemeral upload row with audio player
renderEphemeralUploadRow :: StorageBackend -> EphemeralUploads.EphemeralUploadWithCreator -> Lucid.Html ()
renderEphemeralUploadRow backend ephemeralUpload =
  let ephemeralUploadId = ephemeralUpload.euwcId
      title = ephemeralUpload.euwcTitle
      creatorName = ephemeralUpload.euwcCreatorDisplayName
      createdAt = ephemeralUpload.euwcCreatedAt
      audioPath = ephemeralUpload.euwcAudioFilePath
      ephemeralUploadIdText = display ephemeralUploadId
      rowId = [i|ephemeral-upload-row-#{ephemeralUploadIdText}|]
      deleteUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.delete ephemeralUploadId
      deleteConfirmMessage =
        "Are you sure you want to delete the ephemeral upload \""
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
            renderInlineAudioPlayer audioUrl ephemeralUploadIdText

          -- Actions cell (Delete dropdown only - no download for ephemeral uploads)
          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
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

-- | Render empty state when no ephemeral uploads exist
renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base ["bg-gray-50 dark:bg-gray-700", Tokens.border2, "border-gray-300 dark:border-gray-600", "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.textGray600]] "No ephemeral uploads yet."
    Lucid.p_ [class_ $ base ["text-gray-500 dark:text-gray-400", "mt-2"]] "Upload a new ephemeral clip to get started."

-- | Format a datetime for display
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

{-# LANGUAGE OverloadedRecordDot #-}
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
import Data.Maybe (isJust)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
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
template backend ephemeralUploads (PageNumber pageNum) hasMore userMeta = do
  let isStaffOrAdmin = UserMetadata.isStaffOrHigher userMeta.mUserRole
  -- Ephemeral uploads table or empty state
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
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
          (mapM_ (renderEphemeralUploadRow backend isStaffOrAdmin) ephemeralUploads)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list (Just (PageNumber (pageNum + 1)))
    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.list (Just (PageNumber (pageNum - 1)))

-- | Render a single ephemeral upload row with audio player
renderEphemeralUploadRow ::
  -- | Storage backend for building media URLs
  StorageBackend ->
  -- | Whether the current user is staff or admin (can edit/flag)
  Bool ->
  -- | The ephemeral upload to render
  EphemeralUploads.EphemeralUploadWithCreator ->
  Lucid.Html ()
renderEphemeralUploadRow backend isStaffOrAdmin ephemeralUpload =
  let ephemeralUploadId = ephemeralUpload.euwcId
      title = ephemeralUpload.euwcTitle
      desc = ephemeralUpload.euwcDescription
      creatorName = ephemeralUpload.euwcCreatorDisplayName
      createdAt = ephemeralUpload.euwcCreatedAt
      audioPath = ephemeralUpload.euwcAudioFilePath
      isFlagged = isJust ephemeralUpload.euwcFlaggedAt
      flagReason = maybe "" EphemeralUploads.flagReasonToText ephemeralUpload.euwcFlagReason
      ephemeralUploadIdText = display ephemeralUploadId
      rowId = [i|ephemeral-upload-row-#{ephemeralUploadIdText}|]
      rowTarget = "#" <> rowId
      editUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.editGet ephemeralUploadId
      deleteUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.delete ephemeralUploadId
      flagUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.flagPost ephemeralUploadId
      unflagUrl = Links.linkURI $ dashboardEphemeralUploadsLinks.unflagPost ephemeralUploadId
      deleteConfirmMessage =
        "Are you sure you want to delete the ephemeral upload \""
          <> display title
          <> "\"? This action cannot be undone."
      audioUrl = buildMediaUrl backend audioPath
      -- Flag/unflag actions for staff/admin
      flagActions
        | not isStaffOrAdmin = []
        | isFlagged =
            [ ActionsDropdown.htmxPostAction
                "unflag"
                "Unflag"
                [i|/#{unflagUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just [i|Unflag "#{title}"?|])
                []
            ]
        | otherwise =
            [ ActionsDropdown.htmxPostAction
                "inappropriate"
                "Flag: Inappropriate"
                [i|/#{flagUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just [i|Flag "#{title}" as inappropriate content?|])
                [("reason", "Inappropriate content")],
              ActionsDropdown.htmxPostAction
                "audioquality"
                "Flag: Audio quality"
                [i|/#{flagUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just [i|Flag "#{title}" for poor audio quality?|])
                [("reason", "Poor audio quality")],
              ActionsDropdown.htmxPostAction
                "copyright"
                "Flag: Copyright"
                [i|/#{flagUrl}|]
                rowTarget
                ActionsDropdown.SwapOuterHTML
                (Just [i|Flag "#{title}" for copyright concern?|])
                [("reason", "Copyright concern")]
            ]
      -- Build actions list based on permissions
      actions =
        [ActionsDropdown.navigateAction "edit" "Edit" [i|/#{editUrl}|] | isStaffOrAdmin]
          <> [ ActionsDropdown.htmxDeleteAction
                 "delete"
                 "Delete"
                 [i|/#{deleteUrl}|]
                 rowTarget
                 ActionsDropdown.SwapOuterHTML
                 deleteConfirmMessage
             ]
          <> flagActions
      -- Dim flagged rows
      rowOpacity = if isFlagged then "opacity-60" else ""
   in do
        Lucid.tr_ (rowAttrs rowId <> [Lucid.class_ rowOpacity | isFlagged]) $ do
          -- Title cell
          Lucid.td_ [class_ $ base [Tokens.p4]] $ do
            Lucid.span_ [Lucid.class_ Tokens.fontBold] $
              Lucid.toHtml title
            if isFlagged
              then
                Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.warningText, Tokens.fontBold, "ml-2"]] $
                  Lucid.toHtml ("[FLAGGED: " <> flagReason <> "]")
              else mempty
            Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "truncate", "max-w-xs"]] $
              Lucid.toHtml (Text.take 100 desc)

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

          -- Actions cell
          Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
            ActionsDropdown.render actions

-- | Render an inline audio player with download button using Alpine.js
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
          class_ $ base ["p-2", "rounded", Tokens.hoverBg, "transition-colors"],
          Lucid.title_ "Play/Pause"
        ]
        $ do
          -- Play icon (shown when not playing)
          Lucid.i_
            [ Lucid.class_ [i|fa-solid fa-play #{Tokens.fgMuted}|],
              Lucid.term "x-show" "!playing"
            ]
            mempty
          -- Pause icon (shown when playing)
          Lucid.i_
            [ Lucid.class_ [i|fa-solid fa-pause #{Tokens.fgMuted}|],
              Lucid.term "x-show" "playing"
            ]
            mempty
      -- Download button
      Lucid.a_
        [ Lucid.href_ audioUrl,
          Lucid.download_ "",
          class_ $ base ["p-2", "rounded", Tokens.hoverBg, "transition-colors"],
          Lucid.title_ "Download"
        ]
        $ Lucid.i_ [Lucid.class_ [i|fa-solid fa-download #{Tokens.fgMuted}|]] mempty

-- | Render empty state when no ephemeral uploads exist
renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No ephemeral uploads yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Upload a new ephemeral clip to get started."

-- | Format a datetime for display
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

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
import Control.Monad (forM_, when)
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
import Lucid.HTMX
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
  Card.section Card.TablePanel [Tokens.mb8] $
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
      editButton =
        when isStaffOrAdmin $
          Button.render (Button.defaults & Button.dashboardLink [i|/#{editUrl}|]) "Edit"
      deleteButton =
        Button.render
          ( Button.defaults
              & Button.danger
              & Button.with
                [ hxDelete_ [i|/#{deleteUrl}|],
                  hxTarget_ rowTarget,
                  hxSwap_ "outerHTML",
                  hxConfirm_ deleteConfirmMessage
                ]
          )
          "Delete"
      -- Staff and admins can flag an upload, or unflag a flagged one.
      actions
        | not isStaffOrAdmin =
            Button.group $ do
              editButton
              deleteButton
        | isFlagged =
            Button.group $ do
              editButton
              deleteButton
              Button.render
                ( Button.defaults
                    & Button.with
                      [ hxPost_ [i|/#{unflagUrl}|],
                        hxTarget_ rowTarget,
                        hxSwap_ "outerHTML",
                        hxConfirm_ [i|Unflag "#{title}"?|]
                      ]
                )
                "Unflag"
        | otherwise =
            renderFlagActions ephemeralUploadIdText [i|/#{flagUrl}|] rowTarget $ do
              editButton
              deleteButton
   in do
        -- Dim flagged rows
        Lucid.tr_ (rowAttrs rowId <> [class_ $ base ["opacity-60"] | isFlagged]) $ do
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
            renderPlayAndDownload audioUrl

          -- Actions cell
          Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] actions

-- | The actions of an unflagged row, with a Flag button.
--
-- The Flag button opens a reason picker below the buttons, the same as the
-- Reject button of the promo review page.
renderFlagActions ::
  -- | Upload id, for unique form field ids
  Text ->
  -- | Flag URL
  Text ->
  -- | Row selector
  Text ->
  -- | The other buttons of the row
  Lucid.Html () ->
  Lucid.Html ()
renderFlagActions uploadIdText flagUrl rowTarget otherButtons =
  Lucid.div_ [xData_ "{ flagging: false }"] $ do
    Button.group $ do
      otherButtons
      Button.render
        (Button.defaults & Button.with [xOnClick_ "flagging = !flagging"])
        "Flag"

    Lucid.form_
      [ xShow_ "flagging",
        xCloak_,
        hxPost_ flagUrl,
        hxTarget_ rowTarget,
        hxSwap_ "outerHTML",
        class_ $ base ["mt-2", "flex", "flex-col", "text-left", Tokens.gap2]
      ]
      $ do
        Lucid.fieldset_ [class_ $ base ["flex", "flex-col", Tokens.gap2]] $ do
          Lucid.legend_ [Lucid.class_ Tokens.fontBold] "Reason"
          forM_ [minBound .. maxBound :: EphemeralUploads.FlagReason] $ \reason -> do
            let reasonText = EphemeralUploads.flagReasonToText reason
                fieldId = [i|flag-reason-#{uploadIdText}-#{fromEnum reason}|] :: Text
            Lucid.label_ [Lucid.for_ fieldId, class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
              Lucid.input_
                [ Lucid.type_ "radio",
                  Lucid.id_ fieldId,
                  Lucid.name_ "reason",
                  Lucid.value_ reasonText,
                  Lucid.required_ "required"
                ]
              Lucid.toHtml reasonText
        Button.render (Button.defaults & Button.primary & Button.submit) "Send Flag"

-- | Render the inline audio player with a download link beside it.
renderPlayAndDownload :: Text -> Lucid.Html ()
renderPlayAndDownload audioUrl =
  Lucid.div_ [class_ $ base ["inline-flex", "items-center", Tokens.gap2]] $ do
    renderInlineAudioPlayer audioUrl
    Button.downloadLink audioUrl

-- | Render empty state when no ephemeral uploads exist
renderEmptyState :: Lucid.Html ()
renderEmptyState = do
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No ephemeral uploads yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Upload a new ephemeral clip to get started."

-- | Format a datetime for display
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

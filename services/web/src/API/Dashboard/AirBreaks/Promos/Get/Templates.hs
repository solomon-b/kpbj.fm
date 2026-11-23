{-# LANGUAGE QuasiQuotes #-}

-- | The staff review page for show promos.
--
-- Hosts submit promos. Staff play each one, then approve or reject it here.
-- Staff can delete any promo. The approve and reject actions re-render one row
-- with 'renderPromoRow'.
module API.Dashboard.AirBreaks.Promos.Get.Templates
  ( template,
    renderItemsFragment,
    renderPromoRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardPromosLinks, dashboardShowsLinks)
import API.Types
import Component.BreakItems (formatDuration, formatRange)
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
    renderTableFragment,
    rowAttrs,
  )
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | DOM id of the table body, which the infinite scroll appends into.
tableBodyId :: Text
tableBodyId = "promos-table-body"

-- | The review page.
template ::
  -- | Builds the audio URL of each promo
  StorageBackend ->
  -- | The shows the promos name, by id
  Map Shows.Id Shows.Model ->
  [BreakItems.Model] ->
  PageNumber ->
  -- | Whether a further page exists
  Bool ->
  Lucid.Html ()
template backend showsById promos (PageNumber pageNum) hasMore =
  Card.section Card.TablePanel [Tokens.mb8] $
    if null promos
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = tableBodyId,
              itcHeaders = columnHeaders,
              itcNextPageUrl = nextPageUrl,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl = if pageNum > 1 then Just (pageUrl (pageNum - 1)) else Nothing,
                      pcNextPageUrl = nextPageUrl,
                      pcCurrentPage = pageNum
                    }
            }
          (mapM_ (renderPromoRow backend showsById) promos)
  where
    nextPageUrl = if hasMore then Just (pageUrl (pageNum + 1)) else Nothing

-- | The new rows alone, for an infinite scroll append.
renderItemsFragment ::
  -- | Builds the audio URL of each promo
  StorageBackend ->
  Map Shows.Id Shows.Model ->
  [BreakItems.Model] ->
  PageNumber ->
  -- | Whether a further page exists
  Bool ->
  Lucid.Html ()
renderItemsFragment backend showsById promos (PageNumber pageNum) hasMore =
  renderTableFragment
    (length columnHeaders)
    ("#" <> tableBodyId)
    (if hasMore then Just (pageUrl (pageNum + 1)) else Nothing)
    (mapM_ (renderPromoRow backend showsById) promos)

columnHeaders :: [ColumnHeader]
columnHeaders =
  [ ColumnHeader "Title" AlignLeft,
    ColumnHeader "Show" AlignLeft,
    ColumnHeader "On Air" AlignLeft,
    ColumnHeader "Length" AlignLeft,
    ColumnHeader "Status" AlignLeft,
    ColumnHeader "Play" AlignCenter,
    ColumnHeader "" AlignCenter
  ]

-- | The review page URL for one page number.
pageUrl :: Int64 -> Text
pageUrl pageNum =
  let uri = Links.linkURI $ dashboardPromosLinks.list (Just (PageNumber pageNum))
   in [i|/#{uri}|]

renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No show promos yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Hosts submit promos from their show pages."

--------------------------------------------------------------------------------
-- Rows

-- | One promo row.
--
-- Every row has a play button, a download link, and a Delete button. A pending
-- row also has Approve and Reject buttons. The Reject button opens a note
-- field inside the row, because the host needs to know what to change.
renderPromoRow ::
  -- | Builds the audio URL of the promo
  StorageBackend ->
  Map Shows.Id Shows.Model ->
  BreakItems.Model ->
  Lucid.Html ()
renderPromoRow backend showsById promo =
  Lucid.tr_ (rowAttrs rowId) $ do
    Lucid.td_ [class_ $ base [Tokens.p4]] $
      Lucid.span_ [Lucid.class_ Tokens.fontBold] $
        Lucid.toHtml promo.bimTitle

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $
      renderShowCell mShow

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $
      Lucid.toHtml (formatRange promo.bimStartsOn promo.bimEndsOn)

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $
      Lucid.toHtml (formatDuration promo.bimDurationSeconds)

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $ do
      Lucid.span_ [Lucid.class_ Tokens.fontBold] $
        Lucid.toHtml (display promo.bimReviewStatus)
      case promo.bimReviewNote of
        Nothing -> mempty
        Just note ->
          Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-1"]] $ Lucid.toHtml note

    -- Audio player cell
    Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
      renderInlineAudioPlayer audioUrl

    -- Actions cell
    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
      case promo.bimReviewStatus of
        BreakItems.Pending -> renderPendingActions
        BreakItems.Approved -> renderActionBar renderDeleteButton
        BreakItems.Rejected -> renderActionBar renderDeleteButton
  where
    promoId = promo.bimId
    promoIdText = display promoId
    rowId = [i|promo-row-#{promoIdText}|] :: Text
    rowTarget = "#" <> rowId

    mShow = promo.bimShowId >>= \showId -> Map.lookup showId showsById

    audioUrl = buildMediaUrl backend promo.bimAudioFilePath

    approveUri = Links.linkURI $ dashboardPromosLinks.approvePost promoId
    rejectUri = Links.linkURI $ dashboardPromosLinks.rejectPost promoId
    deleteUri = Links.linkURI $ dashboardPromosLinks.delete promoId

    deleteConfirmMessage =
      "Delete the promo \""
        <> promo.bimTitle
        <> "\"? It will stop airing immediately."

    -- The download link and the given buttons, in one centered line.
    renderActionBar :: Lucid.Html () -> Lucid.Html ()
    renderActionBar buttons =
      Button.group $ do
        Button.downloadLink audioUrl
        buttons

    -- Only a pending promo can be approved or rejected. The Reject button
    -- opens the note form below the buttons.
    renderPendingActions :: Lucid.Html ()
    renderPendingActions =
      Lucid.div_ [xData_ "{ rejecting: false }"] $ do
        renderActionBar $ do
          Button.render
            ( Button.defaults
                & Button.primary
                & Button.with
                  [ hxPost_ [i|/#{approveUri}|],
                    hxTarget_ rowTarget,
                    hxSwap_ "outerHTML"
                  ]
            )
            "Approve"
          Button.render
            (Button.defaults & Button.with [xOnClick_ "rejecting = !rejecting"])
            "Reject"
          renderDeleteButton

        Lucid.form_
          [ xShow_ "rejecting",
            xCloak_,
            hxPost_ [i|/#{rejectUri}|],
            hxTarget_ rowTarget,
            hxSwap_ "outerHTML",
            class_ $ base ["mt-2", "flex", "flex-col", "text-left", Tokens.gap2]
          ]
          $ do
            Lucid.label_ [Lucid.for_ [i|reject-note-#{promoIdText}|], Lucid.class_ Tokens.fontBold] "Note to the host"
            Lucid.textarea_
              [ Lucid.id_ [i|reject-note-#{promoIdText}|],
                Lucid.name_ "note",
                Lucid.rows_ "3",
                Lucid.required_ "required",
                Lucid.placeholder_ "Tell the host what to change.",
                class_ $ base [Tokens.p2, "border", Tokens.borderDefault, Tokens.bgMain, Tokens.fgPrimary]
              ]
              ""
            Button.render (Button.defaults & Button.primary & Button.submit) "Send Rejection"

    renderDeleteButton :: Lucid.Html ()
    renderDeleteButton =
      Button.render
        ( Button.defaults
            & Button.danger
            & Button.with
              [ hxDelete_ [i|/#{deleteUri}|],
                hxTarget_ rowTarget,
                hxSwap_ "outerHTML",
                hxConfirm_ deleteConfirmMessage
              ]
        )
        "Delete"

-- | The show title, linked to the show's dashboard page.
--
-- The show is absent when it was deleted after the host submitted the promo.
renderShowCell :: Maybe Shows.Model -> Lucid.Html ()
renderShowCell = \case
  Nothing -> Lucid.span_ [Lucid.class_ Theme.fgMuted] "Deleted show"
  Just showModel ->
    let showUri = Links.linkURI $ dashboardShowsLinks.detail (Shows.id showModel) showModel.slug Nothing
     in Lucid.a_ [Lucid.href_ [i|/#{showUri}|], class_ $ base ["underline"]] $
          Lucid.toHtml showModel.title

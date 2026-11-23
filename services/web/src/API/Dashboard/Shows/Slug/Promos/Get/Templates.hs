{-# LANGUAGE QuasiQuotes #-}

-- | The promos page of one show, for its hosts.
--
-- The page lists the show's promos with their review status, and holds the
-- form that submits a new promo. Staff review each promo before it airs.
module API.Dashboard.Shows.Slug.Promos.Get.Templates
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types
import Component.BreakItems (audioHint, audioMaxSizeMb, formatDay, formatDuration, formatRange)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.InlineAudioPlayer (renderInlineAudioPlayer)
import Component.Table (ColumnAlign (..), ColumnHeader (..), TableConfig (..), renderTableWithBodyId, rowAttrs)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (Day)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Lucid qualified
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | The promos page.
template ::
  -- | Builds the audio URL of each promo
  StorageBackend ->
  Shows.Model ->
  -- | Staged upload endpoint, which bypasses Cloudflare in production
  Text ->
  -- | Today in Pacific time, the default first air date
  Day ->
  -- | The show's promos, newest first
  [BreakItems.Model] ->
  Lucid.Html ()
template backend showModel uploadUrl today promos = do
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null promos
      then renderEmptyState
      else
        renderTableWithBodyId
          "show-promos-table-body"
          TableConfig
            { headers = columnHeaders,
              wrapperClass = "overflow-x-auto",
              tableClass = "w-full"
            }
          (mapM_ (renderPromoRow backend showModel.slug) promos)

  renderSubmitForm showModel.slug uploadUrl today

columnHeaders :: [ColumnHeader]
columnHeaders =
  [ ColumnHeader "Title" AlignLeft,
    ColumnHeader "On Air" AlignLeft,
    ColumnHeader "Length" AlignLeft,
    ColumnHeader "Status" AlignLeft,
    ColumnHeader "Play" AlignCenter,
    ColumnHeader "" AlignCenter
  ]

renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Card.card Card.EmptyState [] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] "No promos for this show yet."
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] "Submit one below. It airs in breaks after staff approve it."

-- | One promo row, with the rejection note when staff rejected it.
--
-- The row has a play button, a download link, and a Withdraw button.
renderPromoRow ::
  -- | Builds the audio URL of the promo
  StorageBackend ->
  Slug ->
  BreakItems.Model ->
  Lucid.Html ()
renderPromoRow backend showSlug promo =
  Lucid.tr_ (rowAttrs rowId) $ do
    Lucid.td_ [class_ $ base [Tokens.p4]] $
      Lucid.span_ [Lucid.class_ Tokens.fontBold] $
        Lucid.toHtml promo.bimTitle

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $
      Lucid.toHtml (formatRange promo.bimStartsOn promo.bimEndsOn)

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $
      Lucid.toHtml (formatDuration promo.bimDurationSeconds)

    Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm]] $ do
      Lucid.span_ [Lucid.class_ Tokens.fontBold] $
        Lucid.toHtml (statusLabel promo.bimReviewStatus)
      case promo.bimReviewNote of
        Nothing -> mempty
        Just note ->
          Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-1"]] $ do
            "Staff note: "
            Lucid.toHtml note

    -- Audio player cell
    Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
      renderInlineAudioPlayer audioUrl

    -- Actions cell (Download link + Withdraw button)
    Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
      Button.group $ do
        Button.downloadLink audioUrl
        Button.render
          ( Button.defaults
              & Button.danger
              & Button.with
                [ hxDelete_ [i|/#{withdrawUri}|],
                  hxTarget_ ("#" <> rowId),
                  hxSwap_ "outerHTML",
                  hxConfirm_ withdrawConfirmMessage
                ]
          )
          "Withdraw"
  where
    promoIdText = display promo.bimId
    rowId = [i|promo-row-#{promoIdText}|] :: Text
    audioUrl = buildMediaUrl backend promo.bimAudioFilePath
    withdrawUri = Links.linkURI $ dashboardShowsLinks.promoDelete showSlug promo.bimId
    withdrawConfirmMessage =
      "Withdraw the promo \""
        <> promo.bimTitle
        <> "\"? It will not air."

-- | What a host reads in the status column.
statusLabel :: BreakItems.ReviewStatus -> Text
statusLabel = \case
  BreakItems.Pending -> "Waiting for review"
  BreakItems.Approved -> "Approved"
  BreakItems.Rejected -> "Rejected"

-- | The form that submits a new promo.
renderSubmitForm :: Slug -> Text -> Day -> Lucid.Html ()
renderSubmitForm showSlug uploadUrl today =
  renderForm config form
  where
    newPostUri = Links.linkURI $ dashboardShowsLinks.promoNewPost showSlug

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = [i|/#{newPostUri}|],
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcFooterLook = DashboardFooter,
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      section "SUBMIT A PROMO" $ do
        textField "title" $ do
          label "Title"
          placeholder "Enter a name for this promo..."
          hint "A short, descriptive name. Staff and reports go by this."
          required
          maxLength 200

        stagedAudioField "audio_file" uploadUrl "break_item_audio" $ do
          label "Audio"
          hint (audioHint BreakItems.ShowPromo)
          maxSize audioMaxSizeMb

      section "SCHEDULING" $ do
        dateField "starts_on" $ do
          label "First Air Date"
          hint "The first day this may air, in Pacific time."
          value (formatDay today)
          required

        dateField "ends_on" $ do
          label "Last Air Date"
          hint "The last day this may air. Leave empty to run until you withdraw it."

      submitButton "SUBMIT FOR REVIEW"

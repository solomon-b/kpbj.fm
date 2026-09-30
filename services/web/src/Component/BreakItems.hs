{-# LANGUAGE QuasiQuotes #-}

-- | Shared rendering for the two break item dashboard sections.
--
-- PSAs and underwriting announcements are one table and one playout rotation. They are
-- two dashboard sections so each can carry its own permission gate. Everything
-- they draw is identical apart from the URLs they point at and the noun they
-- call a row, so both sections render through here.
--
-- A section supplies a 'SectionUrls' and gets the list page, the infinite
-- scroll fragment, the upload form, and the edit form.
module Component.BreakItems
  ( -- * Section Description
    SectionUrls (..),
    UnderwritingFields (..),

    -- * Rendering
    renderListPage,
    renderItemsFragment,
    renderUploadForm,
    renderEditForm,
    renderActionButton,

    -- * Shared Constants
    audioMaxSizeMb,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardUnderwritersLinks, rootLink)
import API.Types (DashboardUnderwritersRoutes (..))
import Component.ActionsDropdown qualified as ActionsDropdown
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
    renderTableFragment,
    rowAttrs,
  )
import Control.Monad (forM_)
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (Day, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Timezone (formatDate)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Lucid.Form.Builder
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Everything one break item section needs in order to draw itself.
--
-- The two sections differ only in these values.
data SectionUrls = SectionUrls
  { -- | What one row is called, singular. For example @\"PSA\"@.
    suNoun :: Text,
    -- | What many rows are called. For example @\"PSAs\"@.
    suNounPlural :: Text,
    -- | The list page for a given page number.
    suListUrl :: PageNumber -> Links.URI,
    -- | The upload form.
    suNewGetUrl :: Links.URI,
    -- | Where the upload form posts.
    suNewPostUrl :: Links.URI,
    -- | The edit form for one row.
    suEditGetUrl :: BreakItems.Id -> Links.URI,
    -- | Where the edit form posts.
    suEditPostUrl :: BreakItems.Id -> Links.URI,
    -- | Where a row delete goes.
    suDeleteUrl :: BreakItems.Id -> Links.URI,
    -- | DOM id for the table body, which the infinite scroll appends into.
    --
    -- Must be unique on the page and must match between 'renderListPage' and
    -- 'renderItemsFragment'.
    suTableBodyId :: Text,
    -- | The staged upload resource type this section claims against.
    suUploadType :: Text
  }

-- | What the underwriting form adds. The PSA form has none of it.
data UnderwritingFields = UnderwritingFields
  { ufUnderwriters :: [Underwriters.Model],
    ufSelected :: Maybe Underwriters.Id,
    ufSpotsPerMonth :: Text,
    -- | Spots owed this month, and the month's show breaks.
    ufCapacity :: (Int64, Int64)
  }

-- | The largest audio file a break item may carry, in megabytes.
--
-- Break items run for seconds, not minutes, so this is the station ID limit
-- rather than the ephemeral upload limit.
audioMaxSizeMb :: Int
audioMaxSizeMb = 50

--------------------------------------------------------------------------------
-- List Page

-- | The list page for one section.
renderListPage ::
  SectionUrls ->
  -- | Underwriter names by id. Present for the underwriting section, which
  -- adds the underwriter and spots per month columns.
  Maybe (Map Underwriters.Id Text) ->
  [BreakItems.Model] ->
  PageNumber ->
  -- | Whether a further page exists
  Bool ->
  Lucid.Html ()
renderListPage urls mNames items (PageNumber pageNum) hasMore =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null items
      then renderEmptyState urls
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = urls.suTableBodyId,
              itcHeaders = columnHeaders mNames,
              itcNextPageUrl = if hasMore then Just (nextPageUrl urls pageNum) else Nothing,
              itcPaginationConfig =
                Just
                  PaginationConfig
                    { pcPrevPageUrl =
                        if pageNum > 1 then Just (prevPageUrl urls pageNum) else Nothing,
                      pcNextPageUrl = if hasMore then Just (nextPageUrl urls pageNum) else Nothing,
                      pcCurrentPage = pageNum
                    }
            }
          (mapM_ (renderRow urls mNames) items)

-- | The new rows alone, for an infinite scroll append.
renderItemsFragment ::
  SectionUrls ->
  -- | Underwriter names by id, as for 'renderListPage'
  Maybe (Map Underwriters.Id Text) ->
  [BreakItems.Model] ->
  PageNumber ->
  -- | Whether a further page exists
  Bool ->
  Lucid.Html ()
renderItemsFragment urls mNames items (PageNumber pageNum) hasMore =
  renderTableFragment
    (length (columnHeaders mNames))
    ("#" <> urls.suTableBodyId)
    (if hasMore then Just (nextPageUrl urls pageNum) else Nothing)
    (mapM_ (renderRow urls mNames) items)

-- | The table columns. The underwriting section adds two.
columnHeaders :: Maybe (Map Underwriters.Id Text) -> [ColumnHeader]
columnHeaders mNames =
  [ColumnHeader "Title" AlignLeft]
    <> maybe
      []
      (const [ColumnHeader "Underwriter" AlignLeft, ColumnHeader "Spots / Month" AlignLeft])
      mNames
    <> [ ColumnHeader "Length" AlignLeft,
         ColumnHeader "On Air" AlignLeft,
         ColumnHeader "" AlignCenter
       ]

-- | The list page URL for one page number.
--
-- The URI is bound before the quasiquote because the interpolation splice reads
-- @urls.suListUrl@ as function composition rather than as a record field.
pageUrl :: SectionUrls -> Int64 -> Text
pageUrl urls pageNum =
  let uri = urls.suListUrl (PageNumber pageNum)
   in [i|/#{uri}|]

nextPageUrl :: SectionUrls -> Int64 -> Text
nextPageUrl urls pageNum = pageUrl urls (pageNum + 1)

prevPageUrl :: SectionUrls -> Int64 -> Text
prevPageUrl urls pageNum = pageUrl urls (pageNum - 1)

-- | One table row.
renderRow :: SectionUrls -> Maybe (Map Underwriters.Id Text) -> BreakItems.Model -> Lucid.Html ()
renderRow urls mNames item =
  let itemId = item.bimId
      itemIdText = display itemId
      rowId = [i|break-item-row-#{itemIdText}|]
      -- Built outside the quasiquote: the interpolation splice does not parse
      -- record dot syntax.
      editUri = urls.suEditGetUrl itemId
      deleteUri = urls.suDeleteUrl itemId
      editUrl = [i|/#{editUri}|] :: Text
      deleteUrl = [i|/#{deleteUri}|] :: Text
      deleteConfirmMessage =
        "Are you sure you want to delete the "
          <> Text.toLower urls.suNoun
          <> " \""
          <> display item.bimTitle
          <> "\"? It will stop airing immediately."
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.fontBold] $
            Lucid.toHtml item.bimTitle

        forM_ mNames $ \names -> do
          Lucid.td_ [class_ $ base [Tokens.p4]] $
            Lucid.span_ [Lucid.class_ Tokens.textSm] $
              Lucid.toHtml (maybe "" (\uw -> Map.findWithDefault "" uw names) item.bimUnderwriterId)
          Lucid.td_ [class_ $ base [Tokens.p4]] $
            Lucid.span_ [Lucid.class_ Tokens.textSm] $
              Lucid.toHtml (maybe "" (Text.pack . show) item.bimSpotsPerMonth)

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (formatDuration item.bimDurationSeconds)

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (formatRange item.bimStartsOn item.bimEndsOn)

        Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
          ActionsDropdown.render
            [ ActionsDropdown.navigateAction "edit" "Edit" editUrl,
              ActionsDropdown.htmxDeleteAction
                "delete"
                "Delete"
                deleteUrl
                ("#" <> rowId)
                ActionsDropdown.SwapOuterHTML
                deleteConfirmMessage
            ]

renderEmptyState :: SectionUrls -> Lucid.Html ()
renderEmptyState urls =
  Lucid.div_ [class_ $ base [Theme.bgAlt, Tokens.border2, Theme.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Theme.fgMuted]] $
      Lucid.toHtml ("No " <> Text.toLower urls.suNounPlural <> " uploaded yet.")
    Lucid.p_ [class_ $ base [Theme.fgMuted, "mt-2"]] $
      Lucid.toHtml ("Upload one to start filling break windows." :: Text)

-- | The button that sits in the dashboard top bar.
renderActionButton :: SectionUrls -> Lucid.Html ()
renderActionButton urls = renderLinkButton ("Upload " <> urls.suNoun) newUrl
  where
    newUri = urls.suNewGetUrl
    newUrl = [i|/#{newUri}|] :: Text

-- | A top bar link styled as a button.
renderLinkButton ::
  -- | Label
  Text ->
  -- | Target URL
  Text ->
  Lucid.Html ()
renderLinkButton buttonLabel url =
  Lucid.a_
    [ Lucid.href_ url,
      Lucid.class_ "bg-[var(--theme-bg-inverse)] text-[var(--theme-fg-inverse)] px-4 py-2 text-sm font-bold hover:opacity-80"
    ]
    (Lucid.toHtml buttonLabel)

--------------------------------------------------------------------------------
-- Forms

-- | The upload form.
renderUploadForm ::
  SectionUrls ->
  -- | Staged upload endpoint, which bypasses Cloudflare in production
  Text ->
  -- | The default first air date
  Day ->
  -- | Present for the underwriting section
  Maybe UnderwritingFields ->
  Lucid.Html ()
renderUploadForm urls uploadUrl startsOn mUf =
  renderBreakItemForm
    urls
    [i|/#{newPostUri}|]
    "UPLOAD"
    (Just uploadUrl)
    (formatDay startsOn)
    ""
    ""
    mUf
  where
    -- Built outside the quasiquote: the interpolation splice does not parse
    -- record dot syntax.
    newPostUri = urls.suNewPostUrl

-- | The edit form, filled in from an existing row.
--
-- The audio field is absent. Replacing the audio of a spot that is already
-- airing would change what ran under a title the reports already name, so a new
-- recording is a new row.
renderEditForm ::
  SectionUrls ->
  BreakItems.Model ->
  -- | Present for the underwriting section
  Maybe UnderwritingFields ->
  Lucid.Html ()
renderEditForm urls item mUf =
  renderBreakItemForm
    urls
    [i|/#{editPostUri}|]
    "SAVE"
    Nothing
    (formatDay item.bimStartsOn)
    (maybe "" formatDay item.bimEndsOn)
    item.bimTitle
    mUf
  where
    -- Built outside the quasiquote: the interpolation splice does not parse
    -- record dot syntax.
    editPostUri = urls.suEditPostUrl item.bimId

-- | The body both forms share.
renderBreakItemForm ::
  SectionUrls ->
  -- | Where to post
  Text ->
  -- | Submit button text
  Text ->
  -- | Staged upload endpoint. Nothing omits the audio field entirely
  Maybe Text ->
  -- | Initial @starts_on@
  Text ->
  -- | Initial @ends_on@
  Text ->
  -- | Initial title
  Text ->
  -- | Present for the underwriting section
  Maybe UnderwritingFields ->
  Lucid.Html ()
renderBreakItemForm urls postUrl submitLabel mUploadUrl startsOn endsOn title mUf = do
  renderForm config form
  where
    cancelUrl = pageUrl urls 1
    nounLower = Text.toLower urls.suNoun
    underwritersUrl = rootLink dashboardUnderwritersLinks.list

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      case mUploadUrl of
        Nothing -> pure ()
        Just _ -> hidden "duration_seconds" ""

      section (Text.toUpper urls.suNoun <> " DETAILS") $ do
        textField "title" $ do
          label "Title"
          placeholder [i|Enter a name for this #{nounLower}...|]
          hint "A short, descriptive name. Staff and reports go by this."
          value title
          required
          maxLength 200

      forM_ mUf $ \uf ->
        section "UNDERWRITING" $ do
          selectField "underwriter_id" $ do
            label "Underwriter"
            description $ do
              "Not in the list? Add it on the "
              Lucid.a_ [Lucid.href_ underwritersUrl, Lucid.class_ "underline"] "underwriters page"
              "."
            required
            forM_ uf.ufUnderwriters $ \u ->
              if Just u.uwId == uf.ufSelected
                then addOptionSelected (display u.uwId) u.uwName
                else addOption (display u.uwId) u.uwName
          let (owed, showBreaks) = uf.ufCapacity
          numberField "spots_per_month" (Just 1) Nothing (Just 1) $ do
            label "Spots per month"
            hint [i|This month: #{owed} spots owed across #{showBreaks} show breaks.|]
            value uf.ufSpotsPerMonth
            required

      case mUploadUrl of
        Nothing -> pure ()
        Just uploadUrl -> section "AUDIO FILE" $ do
          stagedAudioField "audio_file" uploadUrl urls.suUploadType $ do
            label "Audio"
            let fileHint = [i|Upload an MP3, WAV, or other audio file. Maximum #{audioMaxSizeMb}MB.|] :: Text
            hint $ case mUf of
              Nothing -> fileHint
              Just _ ->
                fileHint
                  <> " Noncommercial rules apply. No call to action, no price or discount, no quality or comparison claim, and no inducement to buy. A staff member reviews the copy before it airs."
            maxSize audioMaxSizeMb

      section "SCHEDULING" $ do
        dateField "starts_on" $ do
          label "First Air Date"
          hint $ case mUf of
            Nothing -> "The first day this may air, in Pacific time."
            Just _ -> "The first day this may air, in Pacific time. A partial month owes that part of the monthly spots, rounded up."
          value startsOn
          required

        dateField "ends_on" $ do
          label "Last Air Date"
          hint "The last day this may air. Leave empty to run until you remove it."
          value endsOn

      cancelButton cancelUrl "CANCEL"
      submitButton submitLabel

--------------------------------------------------------------------------------
-- Formatting

-- | Render a length in seconds as @m:ss@, or as @Ns@ under a minute.
formatDuration :: Int64 -> Text
formatDuration secs
  | secs < 60 = Text.pack (show secs) <> "s"
  | otherwise =
      let (m, s) = secs `divMod` 60
       in Text.pack (show m) <> ":" <> Text.justifyRight 2 '0' (Text.pack (show s))

-- | Render an on-air range, leaving an open end open.
formatRange :: Day -> Maybe Day -> Text
formatRange startsOn Nothing = formatDate startsOn <> " onward"
formatRange startsOn (Just endsOn)
  | startsOn == endsOn = formatDate startsOn
  | otherwise = formatDate startsOn <> " to " <> formatDate endsOn

-- | @YYYY-MM-DD@, which is what a date input reads and writes.
formatDay :: Day -> Text
formatDay = Text.pack . formatTime defaultTimeLocale "%Y-%m-%d"

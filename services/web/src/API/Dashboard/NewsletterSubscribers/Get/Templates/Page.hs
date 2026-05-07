{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.NewsletterSubscribers.Get.Templates.Page
  ( template,
    renderSubscriberRow,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardNewsletterSubscribersLinks)
import API.Types
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
    rowAttrs,
  )
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Newsletter subscribers list template.
template ::
  -- | Subscribers on the current page.
  [NewsletterSubscribers.Model] ->
  -- | Total subscriber count for the current search filter.
  Int64 ->
  -- | Current page number (1-indexed).
  Int64 ->
  -- | Whether another page exists.
  Bool ->
  -- | Current search term, if any.
  Maybe Text ->
  Lucid.Html ()
template subscribers total currentPage hasMore mSearch = do
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.mb4]] $
    renderSearchBar mSearch total
  Lucid.div_ [Lucid.id_ "subscribers-list"] $
    Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
      if total == 0
        then renderEmptyState mSearch
        else
          renderIndexTable
            IndexTableConfig
              { itcBodyId = "subscribers-table-body",
                itcHeaders =
                  [ ColumnHeader "Email" AlignLeft,
                    ColumnHeader "Subscribed On" AlignLeft,
                    ColumnHeader "Mailchimp" AlignLeft,
                    ColumnHeader "" AlignCenter
                  ],
                itcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                itcPaginationConfig =
                  Just
                    PaginationConfig
                      { pcPrevPageUrl = if currentPage > 1 then Just [i|/#{prevPageUrl}|] else Nothing,
                        pcNextPageUrl = if hasMore then Just [i|/#{nextPageUrl}|] else Nothing,
                        pcCurrentPage = currentPage
                      }
              }
            (mapM_ renderSubscriberRow subscribers)
  where
    nextPageUrl :: Links.URI
    nextPageUrl = Links.linkURI $ dashboardNewsletterSubscribersLinks.list mSearch (Just (currentPage + 1))

    prevPageUrl :: Links.URI
    prevPageUrl = Links.linkURI $ dashboardNewsletterSubscribersLinks.list mSearch (Just (currentPage - 1))

-- | Search input bar that re-fetches the list on submit.
renderSearchBar :: Maybe Text -> Int64 -> Lucid.Html ()
renderSearchBar mSearch total =
  let listUri = Links.linkURI $ dashboardNewsletterSubscribersLinks.list Nothing Nothing
      listUrl :: Text
      listUrl = [i|/#{listUri}|]
   in Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.gap4, Tokens.p4]] $ do
        Lucid.form_
          [ hxGet_ listUrl,
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["flex", "items-center", Tokens.gap4, "flex-1"]
          ]
          $ do
            Lucid.input_
              [ Lucid.type_ "search",
                Lucid.name_ "search",
                Lucid.value_ (fromMaybe "" mSearch),
                Lucid.placeholder_ "Search by email...",
                class_ $ base ["px-3", "py-2", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "w-64"]
              ]
            Lucid.button_
              [ Lucid.type_ "submit",
                class_ $ base [Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold, "hover:opacity-80"]
              ]
              "Search"
        Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
          Lucid.toHtml $
            (show total :: String) <> " subscriber" <> (if total == 1 then "" else "s")

-- | Render a single subscriber row, with a per-row delete button.
renderSubscriberRow :: NewsletterSubscribers.Model -> Lucid.Html ()
renderSubscriberRow subscriber =
  let subId = NewsletterSubscribers.mId subscriber
      subIdText = display subId
      rowId :: Text
      rowId = [i|subscriber-row-#{subIdText}|]
      rowSelector :: Text
      rowSelector = "#" <> rowId
      deleteUri = Links.linkURI $ dashboardNewsletterSubscribersLinks.delete subId
      deleteUrl :: Text
      deleteUrl = [i|/#{deleteUri}|]
      confirmMessage :: Text
      confirmMessage =
        "Delete subscriber \"" <> display (NewsletterSubscribers.mEmail subscriber) <> "\"? This cannot be undone."
   in Lucid.tr_ (rowAttrs rowId) $ do
        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [Lucid.class_ Tokens.textSm] $
            Lucid.toHtml (display (NewsletterSubscribers.mEmail subscriber))

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
            Lucid.toHtml (formatDateTime (NewsletterSubscribers.mCreatedAt subscriber))

        Lucid.td_ [class_ $ base [Tokens.p4]] $
          renderMailchimpBadge (NewsletterSubscribers.mMailchimpStatus subscriber)

        Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
          Lucid.button_
            [ hxDelete_ deleteUrl,
              hxTarget_ rowSelector,
              hxSwap_ "outerHTML",
              hxConfirm_ confirmMessage,
              class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.errorBg, Tokens.errorText, "border", Tokens.borderMuted, "hover:opacity-80"]
            ]
            "Delete"

-- | Render a colored pill for the Mailchimp sync status.
--
--  * @\"subscribed\"@ → green Success
--  * @\"pending\"@    → yellow Warning
--  * @\"error\"@      → red Error
--  * @\"unsubscribed\"@ / @\"cleaned\"@ → muted Info (only briefly visible
--    before the reconcile job removes the row)
--  * @Nothing@        → muted em-dash (never synced; pre-bootstrap rows)
renderMailchimpBadge :: Maybe Text -> Lucid.Html ()
renderMailchimpBadge = \case
  Nothing ->
    Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
      Lucid.toHtml @Text "—"
  Just status ->
    let (bgClass, textClass, label) = badgeStyle status
     in Lucid.span_
          [class_ $ base ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
          $ Lucid.toHtml label
  where
    badgeStyle :: Text -> (Text, Text, Text)
    badgeStyle = \case
      "subscribed" -> (Tokens.successBg, Tokens.successText, "Subscribed")
      "pending" -> (Tokens.warningBg, Tokens.warningText, "Pending")
      "error" -> (Tokens.errorBg, Tokens.errorText, "Error")
      "unsubscribed" -> (Tokens.bgAlt, Tokens.fgMuted, "Unsubscribed")
      "cleaned" -> (Tokens.bgAlt, Tokens.fgMuted, "Cleaned")
      other -> (Tokens.bgAlt, Tokens.fgMuted, other)

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState mSearch =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted, "p-12", "text-center"]] $ do
    case mSearch of
      Just search ->
        Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted]] $
          Lucid.toHtml $
            "No subscribers match \"" <> search <> "\"."
      Nothing -> do
        Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted]] "No newsletter subscribers yet."
        Lucid.p_ [class_ $ base [Tokens.fgMuted, "mt-2"]] "Bulk add subscribers to get started."

-- | Format a UTC timestamp as a human-readable date string.
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

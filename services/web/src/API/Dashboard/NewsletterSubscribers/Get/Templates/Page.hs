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
import Component.Badge qualified as Badge
import Component.Button ((&))
import Component.Button qualified as Button
import Component.Card qualified as Card
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    PaginationConfig (..),
    renderIndexTable,
    rowAttrs,
  )
import Data.Int (Int64)
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
template subscribers total currentPage hasMore mSearch =
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

        Lucid.td_ [class_ $ base [Tokens.p4, Tokens.textSm, "text-center"]] $
          Button.group $
            Button.render
              ( Button.defaults
                  & Button.danger
                  & Button.with
                    [ hxDelete_ deleteUrl,
                      hxTarget_ rowSelector,
                      hxSwap_ "outerHTML",
                      hxConfirm_ confirmMessage
                    ]
              )
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
    let (tone, label) = badgeStyle status
     in Badge.badge Badge.Regular tone $ Lucid.toHtml label
  where
    badgeStyle :: Text -> (Badge.Tone, Text)
    badgeStyle = \case
      "subscribed" -> (Badge.Success, "Subscribed")
      "pending" -> (Badge.Warning, "Pending")
      "error" -> (Badge.Error, "Error")
      "unsubscribed" -> (Badge.Neutral, "Unsubscribed")
      "cleaned" -> (Badge.Neutral, "Cleaned")
      other -> (Badge.Neutral, other)

renderEmptyState :: Maybe Text -> Lucid.Html ()
renderEmptyState mSearch =
  Card.card Card.EmptyState [] $ do
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

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.NewsletterSubscribers.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.NewsletterSubscribers.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardNewsletterSubscribersLinks)
import API.Types
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Number of subscribers per page.
pageSize :: Int64
pageSize = 50

-- | Handler for GET /dashboard/newsletter-subscribers.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  Maybe Text ->
  Maybe Int64 ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) mSearchRaw mPage =
  handleHtmlErrors "Newsletter subscribers list" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "Only staff can view newsletter subscribers." userMetadata

    let mSearch = normalizeSearch mSearchRaw
        page = max 1 (fromMaybe 1 mPage)
        offset = (page - 1) * pageSize
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    subscribers <- fromRightM throwDatabaseError $ execQuery (NewsletterSubscribers.getPaginated mSearch pageSize offset)
    total <- fromRightM throwDatabaseError $ execQuery (NewsletterSubscribers.countAll mSearch)

    let hasMore = total > page * pageSize

    if isAppendRequest
      then pure $ renderItemsFragment subscribers page hasMore mSearch
      else
        lift $
          renderDashboardTemplate
            hxRequest
            userMetadata
            []
            Nothing
            NavNewsletterSubscribers
            Nothing
            (Just actionButton)
            (template subscribers total page hasMore mSearch)

-- | Drop empty/whitespace-only search strings.
normalizeSearch :: Maybe Text -> Maybe Text
normalizeSearch =
  (>>= \t -> let stripped = Text.strip t in if Text.null stripped then Nothing else Just stripped)

-- | "BULK ADD" action button rendered in the dashboard top bar.
actionButton :: Lucid.Html ()
actionButton =
  let bulkUri = Links.linkURI dashboardNewsletterSubscribersLinks.bulkGet
      bulkUrl :: Text
      bulkUrl = [i|/#{bulkUri}|]
   in Lucid.a_
        [ Lucid.href_ bulkUrl,
          hxGet_ bulkUrl,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
        ]
        (Lucid.toHtml ("BULK ADD" :: Text))

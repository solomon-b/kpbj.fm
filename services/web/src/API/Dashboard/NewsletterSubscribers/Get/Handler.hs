{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.NewsletterSubscribers.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.NewsletterSubscribers.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.NewsletterSubscribers.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardNewsletterSubscribersLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.Button ((&))
import Component.Button qualified as Button
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Effects.Database.Tables.UserMetadata (UserRole (..))
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
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  Maybe Text ->
  Maybe Int64 ->
  AppM (Lucid.Html ())
handler (Satisfies _ auth) (foldHxReq -> hxRequest) mSearchRaw mPage =
  handleHtmlErrors "Newsletter subscribers list" apiLinks.rootGet $ do
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
            auth.raMetadata
            NoShow
            NavNewsletterSubscribers
            (Just (filtersUI mSearch total))
            (Just actionButton)
            (template subscribers total page hasMore mSearch)

-- | Drop empty/whitespace-only search strings.
normalizeSearch :: Maybe Text -> Maybe Text
normalizeSearch =
  (>>= \t -> let stripped = Text.strip t in if Text.null stripped then Nothing else Just stripped)

-- | Search form and subscriber count for the top bar.
filtersUI ::
  -- | Current search term, if any
  Maybe Text ->
  -- | Total subscriber count for the current search
  Int64 ->
  Lucid.Html ()
filtersUI mSearch total = do
  let listUri = Links.linkURI $ dashboardNewsletterSubscribersLinks.list Nothing Nothing
  Lucid.form_
    [ hxGet_ [i|/#{listUri}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "flex items-center gap-3"
    ]
    $ do
      Lucid.input_
        [ Lucid.type_ "search",
          Lucid.name_ "search",
          Lucid.value_ (fromMaybe "" mSearch),
          Lucid.placeholder_ "Search by email...",
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "w-48"]
        ]
      Button.render (Button.defaults & Button.small & Button.submit) "Search"
      Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
        Lucid.toHtml $
          (show total :: String) <> " subscriber" <> (if total == 1 then "" else "s")

-- | "BULK ADD" action button rendered in the dashboard top bar.
actionButton :: Lucid.Html ()
actionButton =
  let bulkUri = Links.linkURI dashboardNewsletterSubscribersLinks.bulkGet
      bulkUrl :: Text
      bulkUrl = [i|/#{bulkUri}|]
   in Button.render (Button.defaults & Button.dashboardLink bulkUrl) "BULK ADD"

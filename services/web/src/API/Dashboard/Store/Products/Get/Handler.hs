{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Store.Products.Get.Handler (handler, action, ProductListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Store.Products.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Maybe (listToMaybe)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the products list page.
data ProductListViewData = ProductListViewData
  { plvUserMetadata :: UserMetadata.Model,
    plvAllShows :: [Shows.Model],
    plvSelectedShow :: Maybe Shows.Model,
    plvProducts :: [Products.Model]
  }

-- | Business logic: fetch shows for sidebar, fetch all products.
action ::
  User.Model ->
  UserMetadata.Model ->
  ExceptT HandlerError AppM ProductListViewData
action user userMetadata = do
  -- 1. Fetch shows for sidebar
  showsResult <-
    if UserMetadata.isAdmin userMetadata.mUserRole
      then execQuery Shows.getAllActiveShows
      else execQuery (Shows.getShowsForUser (User.mId user))
  let allShows = fromRight [] showsResult
      selectedShow = listToMaybe allShows

  -- 2. Fetch all products
  products <- fromRightM throwDatabaseError $ execQuery Products.getAll

  pure
    ProductListViewData
      { plvUserMetadata = userMetadata,
        plvAllShows = allShows,
        plvSelectedShow = selectedShow,
        plvProducts = products
      }

-- | Servant handler: thin glue composing action + render with dashboard chrome.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Products list" apiLinks.rootGet $ do
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata
    lift $
      renderDashboardTemplate
        hxRequest
        vd.plvUserMetadata
        vd.plvAllShows
        vd.plvSelectedShow
        NavStoreProducts
        Nothing
        (Just actionButton)
        (template vd.plvProducts)

-- | Action button rendered in the dashboard top bar.
--
-- Dispatches a custom 'show-create' event that the template listens for
-- via @\@show-create.window@ to toggle the inline create row.
actionButton :: Lucid.Html ()
actionButton =
  Lucid.button_
    [ Lucid.type_ "button",
      Lucid.onclick_ "window.dispatchEvent(new CustomEvent('show-create'))",
      class_ $ base [Tokens.bgAlt, Tokens.fgPrimary, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, Tokens.hoverBg]
    ]
    "+ NEW"

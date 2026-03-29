{-# LANGUAGE OverloadedRecordDot #-}

module API.Store.List.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Store.List.Get.Templates (template)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Domain.Types.HxRequest qualified as HxRequest
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Products qualified as Products
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

data StoreListViewData = StoreListViewData
  { slvStorageBackend :: StorageBackend,
    slvProducts :: [Products.ProductWithHeroImage]
  }

--------------------------------------------------------------------------------

-- | Handler for the public store listing page.
--
-- Fetches all active products with their hero images and renders
-- a responsive grid of product cards.
handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie hxRequest =
  handleHtmlErrors "Store list" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action
    let hxReq = HxRequest.foldHxReq hxRequest
    lift $ renderTemplate hxReq mUserInfo (template vd.slvStorageBackend vd.slvProducts)

--------------------------------------------------------------------------------

-- | Business logic: fetch active products with hero images.
action :: ExceptT HandlerError AppM StoreListViewData
action = do
  storageBackend <- asks getter
  products <- fromRightM throwDatabaseError $ execQuery Products.getActiveWithHeroImage
  pure
    StoreListViewData
      { slvStorageBackend = storageBackend,
        slvProducts = products
      }

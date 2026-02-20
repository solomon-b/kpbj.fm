{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Get.Handler where

--------------------------------------------------------------------------------

import API.Get.Templates (template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (getUserInfo, renderTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

data HomeViewData = HomeViewData
  { hvdStorageBackend :: StorageBackend,
    hvdFeaturedEvent :: Maybe Events.Model
  }

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Home" apiLinks.rootGet $ do
    mUserInfo <- lift $ getUserInfo cookie <&> fmap snd
    vd <- action
    lift $ renderTemplate hxRequest mUserInfo (template vd.hvdStorageBackend vd.hvdFeaturedEvent)

--------------------------------------------------------------------------------

-- | Business logic: fetch featured event for home page.
action :: ExceptT HandlerError AppM HomeViewData
action = do
  storageBackend <- asks getter
  mFeaturedEvent <-
    execQuery Events.getFeaturedEvent >>= \case
      Left _err -> pure Nothing
      Right mEvent -> pure mEvent
  pure
    HomeViewData
      { hvdStorageBackend = storageBackend,
        hvdFeaturedEvent = mFeaturedEvent
      }

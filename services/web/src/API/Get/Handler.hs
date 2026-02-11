{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}

module API.Get.Handler where

--------------------------------------------------------------------------------

import API.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Control.Monad.Reader (asks)
import Data.Functor ((<&>))
import Data.Has (getter)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Events qualified as Events
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  storageBackend <- asks getter
  mFeaturedEvent <-
    execQuery Events.getFeaturedEvent >>= \case
      Left _err -> pure Nothing
      Right mEvent -> pure mEvent
  renderTemplate hxRequest mUserInfo (template storageBackend mFeaturedEvent)

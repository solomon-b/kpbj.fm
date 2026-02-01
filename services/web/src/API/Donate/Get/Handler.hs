{-# LANGUAGE ViewPatterns #-}

module API.Donate.Get.Handler where

--------------------------------------------------------------------------------

import API.Donate.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Data.Functor ((<&>))
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  renderTemplate hxRequest mUserInfo template

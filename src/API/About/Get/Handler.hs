{-# LANGUAGE ViewPatterns #-}

module API.About.Get.Handler where

--------------------------------------------------------------------------------

import API.About.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- fmap snd <$> getUserInfo cookie
  renderTemplate hxRequest mUserInfo template

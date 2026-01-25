{-# LANGUAGE ViewPatterns #-}

module API.PrivacyPolicy.Get.Handler where

--------------------------------------------------------------------------------

import API.PrivacyPolicy.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Data.Functor ((<&>))
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.SitePages qualified as SitePages
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  -- Fetch page content from database
  pageResult <- execQuerySpan (SitePages.getPageBySlug "privacy-policy")
  mPage <- case pageResult of
    Left err -> do
      Log.logAttention "Failed to fetch privacy policy page from database" (show err)
      pure Nothing
    Right p -> pure p
  renderTemplate hxRequest mUserInfo (template mPage)

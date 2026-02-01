{-# LANGUAGE ViewPatterns #-}

module API.TermsOfService.Get.Handler where

--------------------------------------------------------------------------------

import API.TermsOfService.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
import Data.Functor ((<&>))
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.SitePages qualified as SitePages
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  -- Fetch page content from database
  pageResult <- execQuery (SitePages.getPageBySlug "terms-of-service")
  mPage <- case pageResult of
    Left err -> do
      Log.logAttention "Failed to fetch terms of service page from database" (show err)
      pure Nothing
    Right p -> pure p
  renderTemplate hxRequest mUserInfo (template mPage)

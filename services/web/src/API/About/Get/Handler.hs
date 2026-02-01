{-# LANGUAGE ViewPatterns #-}

module API.About.Get.Handler where

--------------------------------------------------------------------------------

import API.About.Get.Templates (template)
import App.Common (getUserInfo, renderTemplate)
import App.Monad (AppM)
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
  mUserInfo <- fmap snd <$> getUserInfo cookie
  -- Fetch page content from database
  pageResult <- execQuery (SitePages.getPageBySlug "about")
  mPage <- case pageResult of
    Left err -> do
      Log.logAttention "Failed to fetch about page from database" (show err)
      pure Nothing
    Right p -> pure p
  renderTemplate hxRequest mUserInfo (template mPage)

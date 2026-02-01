{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

-- | Handler for GET /user/forgot-password
--
-- Shows the forgot password form for users to request a password reset link.
-- Redirects logged-in users to the dashboard.
module API.User.ForgotPassword.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Links (dashboardLinks)
import API.Types
import API.User.ForgotPassword.Get.Templates.Page qualified as Templates
import App.Common (getUserInfo, renderUnauthTemplate)
import App.Monad (AppM)
import Component.Redirect (redirectTemplate)
import Data.String.Interpolate (i)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Lucid qualified
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardGetUrl :: Links.URI
dashboardGetUrl = Links.linkURI dashboardLinks.home

--------------------------------------------------------------------------------

handler ::
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler (foldHxReq -> hxRequest) = do
  getUserInfo Nothing >>= \case
    Just _ ->
      -- Logged in users should change their password in settings
      pure $ redirectTemplate [i|/#{dashboardGetUrl}|]
    Nothing -> do
      let content = Templates.template
      renderUnauthTemplate hxRequest content

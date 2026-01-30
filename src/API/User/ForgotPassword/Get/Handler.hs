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
import App.Common (getUserInfo)
import App.Monad (AppM)
import Component.Frame (loadContentOnly, loadFrame)
import Component.Redirect (redirectTemplate)
import Control.Monad.Reader (asks)
import Data.Has (getter)
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
  mGoogleAnalyticsId <- asks getter
  getUserInfo Nothing >>= \case
    Just _ ->
      -- Logged in users should change their password in settings
      pure $ redirectTemplate [i|/#{dashboardGetUrl}|]
    Nothing -> do
      let content = Templates.template
      case hxRequest of
        IsHxRequest -> loadContentOnly content
        IsNotHxRequest -> loadFrame mGoogleAnalyticsId content

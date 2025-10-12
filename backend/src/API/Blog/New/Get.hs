{-# LANGUAGE ViewPatterns #-}

module API.Blog.New.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (blogGetLink, blogNewPostLink, userLoginGetLink)
import API.Blog.New.Get.Templates.Error (loginRequiredTemplate, permissionDeniedTemplate)
import API.Blog.New.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing

blogNewPostUrl :: Links.URI
blogNewPostUrl = Links.linkURI blogNewPostLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /blog/new"
    ( "blog"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing ->
      renderTemplate hxRequest Nothing loginRequiredTemplate
    Just (_user, userMetadata) -> do
      if UserMetadata.isStaffOrHigher userMetadata.mUserRole
        then renderTemplate hxRequest (Just userMetadata) (template userMetadata)
        else renderTemplate hxRequest Nothing permissionDeniedTemplate

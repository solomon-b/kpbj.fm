{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:slug"
    ( "shows"
        :> Servant.Capture "slug" Slug
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
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult
  execQuerySpan (Shows.getShowBySlug slug) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Show not found: " <> display slug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just showModel) -> do
      fetchShowDetails showModel >>= \case
        Left err -> do
          Log.logAttention "Failed to fetch show details from database" (show err)
          let showTemplate = template showModel [] [] [] []
          renderTemplate hxRequest mUserInfo showTemplate
        Right (hosts, schedule, episodes, blogPosts) -> do
          let showTemplate = template showModel episodes hosts schedule blogPosts
          renderTemplate hxRequest mUserInfo showTemplate

fetchShowDetails ::
  (MonadDB m) =>
  Shows.Model ->
  m (Either HSQL.Pool.UsageError ([ShowHost.ShowHostWithUser], [ShowSchedule.ScheduleTemplate], [Episodes.Model], [ShowBlogPosts.Model]))
fetchShowDetails showModel = runDBTransaction $ do
  episodes <- TRX.statement () $ Episodes.getEpisodesById showModel.id
  hosts <- TRX.statement () $ ShowHost.getShowHostsWithUsers showModel.id
  blogPosts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0
  schedule <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showModel.id
  pure (hosts, schedule, episodes, blogPosts)

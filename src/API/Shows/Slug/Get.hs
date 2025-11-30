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
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
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
        :> Servant.QueryParam "page" Int
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Number of episodes to show per page
episodesPerPage :: Int64
episodesPerPage = 10

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
  Maybe Int ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug mPage cookie (foldHxReq -> hxRequest) = do
  userInfoResult <- getUserInfo cookie
  let mUserInfo = fmap snd userInfoResult
      page = fromMaybe 1 mPage
      limit = fromIntegral episodesPerPage
      offset = fromIntegral (page - 1) * fromIntegral episodesPerPage
  execQuerySpan (Shows.getShowBySlug slug) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
    Right Nothing -> do
      Log.logInfo ("Show not found: " <> display slug) ()
      renderTemplate hxRequest mUserInfo (notFoundTemplate slug)
    Right (Just showModel) -> do
      fetchShowDetails showModel limit offset >>= \case
        Left err -> do
          Log.logAttention "Failed to fetch show details from database" (show err)
          let showTemplate = template showModel [] [] [] [] page
          renderTemplate hxRequest mUserInfo showTemplate
        Right (hosts, schedule, episodes, blogPosts) -> do
          let showTemplate = template showModel episodes hosts schedule blogPosts page
          renderTemplate hxRequest mUserInfo showTemplate

fetchShowDetails ::
  (MonadDB m) =>
  Shows.Model ->
  Limit ->
  Offset ->
  m (Either HSQL.Pool.UsageError ([ShowHost.ShowHostWithUser], [ShowSchedule.ScheduleTemplate], [Episodes.Model], [ShowBlogPosts.Model]))
fetchShowDetails showModel limit offset = runDBTransaction $ do
  episodes <- TRX.statement () $ Episodes.getPublishedEpisodesForShow showModel.id limit offset
  hosts <- TRX.statement () $ ShowHost.getShowHostsWithUsers showModel.id
  blogPosts <- TRX.statement () $ ShowBlogPosts.getPublishedShowBlogPosts showModel.id 3 0
  schedule <- TRX.statement () $ ShowSchedule.getActiveScheduleTemplatesForShow showModel.id
  pure (hosts, schedule, episodes, blogPosts)

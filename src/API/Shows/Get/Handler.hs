{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Get.Handler where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Shows.Get.Templates.Page (template)
import API.Types
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Genre (Genre)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Search (Search (..))
import Domain.Types.ShowSortBy (ShowSortBy (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

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
  Maybe PageNumber ->
  Maybe Genre ->
  Maybe Shows.Status ->
  Maybe Search ->
  Maybe ShowSortBy ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (fromMaybe 1 -> page) maybeGenre maybeStatus maybeSearch maybeSortBy (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  getUserInfo cookie >>= \(fmap snd -> mUserInfo) -> do
    let limit = 12 :: Limit
        offset = fromIntegral $ ((coerce page :: Int64) - 1) * fromIntegral limit :: Offset
        sortBy = fromMaybe NameAZ maybeSortBy

    -- Fetch limit + 1 to check if there are more results
    getShows (limit + 1) offset maybeSearch maybeGenre maybeStatus sortBy >>= \case
      Left err -> do
        Log.logInfo "Failed to fetch shows from database" (Aeson.object ["error" .= show err])
        let banner = BannerParams Error "Error" "Failed to load shows. Please try again."
        renderTemplate htmxRequest mUserInfo (redirectWithBanner [i|/#{rootGetUrl}|] banner)
      Right allShows -> do
        let someShows = take (fromIntegral limit) allShows
            hasMore = length allShows > fromIntegral limit
            showsTemplate = template someShows page hasMore maybeGenre maybeStatus maybeSearch maybeSortBy
        renderTemplate htmxRequest mUserInfo showsTemplate

getShows ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Limit ->
  Offset ->
  Maybe Search ->
  Maybe Genre ->
  Maybe Shows.Status ->
  ShowSortBy ->
  m (Either HSQL.Pool.UsageError [Shows.Model])
getShows limit offset maybeSearch maybeGenre maybeStatus sortBy = do
  case maybeSearch of
    Just (Search searchTerm)
      | not (Text.null $ Text.strip searchTerm) ->
          -- If search term is provided, use search function (search has its own relevance-based sorting)
          execQuerySpan (Shows.searchShows (Search $ Text.strip searchTerm) limit offset)
    _ ->
      -- No search term, use filter and sort logic
      execQuerySpan (Shows.getShowsFiltered maybeGenre maybeStatus sortBy limit offset)

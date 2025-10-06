{-# LANGUAGE ViewPatterns #-}

module API.Shows.Get where

--------------------------------------------------------------------------------

import API.Shows.Get.Templates.Error (errorTemplate)
import API.Shows.Get.Templates.Page (template)
import App.Common (getUserInfo, renderTemplate)
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
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Genre (Genre)
import Domain.Types.HxRequest (HxRequest (..))
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.Search (Search (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Show qualified as Show
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Text.HTML (HTML)

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows"
    ( "shows"
        :> Servant.QueryParam "page" PageNumber
        :> Servant.QueryParam "genre" Genre
        :> Servant.QueryParam "status" Show.ShowStatus
        :> Servant.QueryParam "search" Search
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
  Maybe PageNumber ->
  Maybe Genre ->
  Maybe Show.ShowStatus ->
  Maybe Search ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (fromMaybe 1 -> page) maybeGenre maybeStatus maybeSearch (coerce -> cookie) (fromMaybe IsNotHxRequest -> htmxRequest) = do
  getUserInfo cookie $ \(fmap snd -> mUserInfo) -> do
    let limit = 12
        offset = (coerce page - 1) * limit

    -- Fetch limit + 1 to check if there are more results
    showsResult <- getShows (limit + 1) offset maybeSearch maybeGenre maybeStatus

    case showsResult of
      Left err -> do
        Log.logInfo "Failed to fetch shows from database" (Aeson.object ["error" .= show err])
        renderTemplate htmxRequest mUserInfo (errorTemplate "Failed to load shows. Please try again.")
      Right allShows -> do
        let someShows = take (fromIntegral limit) allShows
            hasMore = length allShows > fromIntegral limit
            showsTemplate = template someShows page hasMore maybeGenre maybeStatus maybeSearch
        renderTemplate htmxRequest mUserInfo showsTemplate

getShows ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Int64 ->
  Int64 ->
  Maybe Search ->
  Maybe Genre ->
  Maybe Show.ShowStatus ->
  m (Either HSQL.Pool.UsageError [Show.ShowModel])
getShows limit offset maybeSearch maybeGenre maybeStatus = do
  case maybeSearch of
    Just (Search searchTerm)
      | not (Text.null $ Text.strip searchTerm) ->
          -- If search term is provided, use search function
          execQuerySpan (Show.searchShows (Search $ Text.strip searchTerm) limit offset)
    _ ->
      -- No search term, use existing filter logic
      case (maybeGenre, maybeStatus) of
        (Just genre, Nothing) ->
          execQuerySpan (Show.getShowsByGenre genre limit offset)
        (Nothing, Just status) ->
          case status of
            Show.Active ->
              execQuerySpan Show.getActiveShows
            Show.Inactive ->
              execQuerySpan (Show.getAllShows limit offset)
        (Just genre, Just status) ->
          execQuerySpan (Show.getShowsByGenreAndStatus genre status limit offset)
        (Nothing, Nothing) ->
          execQuerySpan (Show.getAllShows limit offset)

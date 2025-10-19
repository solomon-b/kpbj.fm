{-# LANGUAGE ViewPatterns #-}

module API.Archive.Get where

--------------------------------------------------------------------------------

import API.Archive.Get.Templates.Error (errorTemplate)
import API.Archive.Get.Templates.Page (episodeCardsOnly, template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
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
    "GET /archive"
    ( "archive"
        :> Servant.QueryParam "q" Text
        :> Servant.QueryParam "genre" Text
        :> Servant.QueryParam "year" Int
        :> Servant.QueryParam "page" Int64
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
  Maybe Text ->
  Maybe Text ->
  Maybe Int ->
  Maybe Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer (normalize -> normalizedSearch) (normalize -> normalizedGenre) mYear maybePage cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      limit = 12
      offset = (page - 1) * limit
      (mDateFrom, mDateTo) = convertYear mYear

  mUserInfo <- getUserInfo cookie <&> fmap snd
  episodesResult <- execQuerySpan $ Episodes.getPublishedEpisodesWithFilters normalizedSearch normalizedGenre mDateFrom mDateTo "newest" limit offset
  totalCountResult <- execQuerySpan $ Episodes.countPublishedEpisodesWithFilters normalizedSearch normalizedGenre mDateFrom mDateTo

  case (episodesResult, totalCountResult) of
    (Left err, _) -> do
      Log.logInfo "Failed to fetch episodes from database" (show err)
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load episodes. Please try again.")
    (_, Left _err) -> do
      Log.logInfo "Failed to count episodes from database" ()
      renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load episodes. Please try again.")
    (Right episodes, Right totalCount) -> do
      let hasMore = (offset + limit) < totalCount
      if page > 1
        then renderTemplate hxRequest mUserInfo (episodeCardsOnly episodes page hasMore totalCount normalizedSearch normalizedGenre mYear)
        else do
          let archiveTemplate = template episodes page hasMore totalCount normalizedSearch normalizedGenre mYear
          renderTemplate hxRequest mUserInfo archiveTemplate

normalize :: Maybe Text -> Maybe Text
normalize = \case
  Just "" -> Nothing
  other -> other

convertYear :: Maybe Int -> (Maybe UTCTime, Maybe UTCTime)
convertYear = \case
  Just year ->
    ( Just $ UTCTime (fromGregorian (fromIntegral year) 1 1) (secondsToDiffTime 0),
      Just $ UTCTime (fromGregorian (fromIntegral year) 12 31) (secondsToDiffTime 86399)
    )
  Nothing -> (Nothing, Nothing)

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Archive.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (rootGetLink)
import API.Archive.Get.Templates.Page (episodeCardsOnly, template)
import App.Common (getUserInfo, renderTemplate)
import Component.Banner (BannerType (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Time (UTCTime (..), fromGregorian, secondsToDiffTime)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Effects.Clock (MonadClock (..))
import Effects.Database.Class (MonadDB (..))
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as TRX
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI rootGetLink

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
    Has HSQL.Pool.Pool env,
    MonadClock m
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
      limit = 12 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      (mDateFrom, mDateTo) = convertYear mYear

  now <- currentSystemTime
  mUserInfo <- getUserInfo cookie <&> fmap snd
  dbResult <- execTransactionSpan $ do
    episodesResult <- TRX.statement () $ Episodes.getPublishedEpisodesWithFilters now normalizedSearch normalizedGenre mDateFrom mDateTo "newest" limit offset
    totalCountResult <- TRX.statement () $ Episodes.countPublishedEpisodesWithFilters normalizedSearch normalizedGenre mDateFrom mDateTo
    pure (episodesResult, totalCountResult)

  case dbResult of
    Left err -> do
      Log.logInfo "DB Usage Error" (show err)
      let banner = BannerParams Error "Error" "Failed to load episodes. Please try again."
      renderTemplate hxRequest mUserInfo (redirectWithBanner [i|/#{rootGetUrl}|] banner)
    Right (episodes, totalCount) -> do
      let hasMore = (fromIntegral offset + fromIntegral limit) < totalCount
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

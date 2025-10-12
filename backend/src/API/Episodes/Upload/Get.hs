{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Upload.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeUploadGetLink)
import API.Episodes.Upload.Get.Templates.Error (notLoggedInTemplate, showLoadErrorTemplate)
import API.Episodes.Upload.Get.Templates.Form (episodeUploadForm)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
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
episodeUploadGetUrl :: Links.URI
episodeUploadGetUrl = Links.linkURI episodeUploadGetLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /episodes/upload"
    ( "episodes"
        :> "upload"
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
    Nothing -> do
      -- Redirect to login
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      showsResult <- execQuerySpan (Shows.getShowsForUser user.mId)
      case showsResult of
        Left _err -> do
          Log.logInfo "Failed to fetch user's shows" ()
          renderTemplate hxRequest Nothing showLoadErrorTemplate
        Right userShows -> do
          -- TODO: This is wrong! We need to conditioanly fetch upcoming dates based on what show the user selects!
          --
          -- Get upcoming dates for the user's first show (primary show)
          upcomingDates <- case userShows of
            [] -> pure []
            (primaryShow : _) -> do
              datesResult <- execQuerySpan (Shows.getUpcomingShowDates primaryShow.id 4)
              case datesResult of
                Left _err -> do
                  Log.logInfo "Failed to fetch upcoming show dates" primaryShow.id
                  pure []
                Right dates -> pure dates

          renderTemplate hxRequest (Just userMetadata) $ episodeUploadForm userShows upcomingDates

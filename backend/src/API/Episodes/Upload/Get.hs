{-# LANGUAGE OverloadedRecordDot #-}

module API.Episodes.Upload.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeUploadGetLink)
import API.Episodes.Upload.Get.Templates.Form (episodeUploadForm)
import App.Common (getUserInfo)
import Component.Frame (loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Show qualified as Show
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
  m (Lucid.Html ())
handler _tracer cookie = do
  getUserInfo cookie $ \case
    Nothing -> do
      -- Redirect to login
      loadFrame $ do
        Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
          Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to upload episodes."
          Lucid.a_ [Lucid.href_ "/user/login", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Login"
    Just (user, userMetadata) -> do
      showsResult <- execQuerySpan (Show.getShowsForUser user.mId)
      case showsResult of
        Left _err -> do
          Log.logInfo "Failed to fetch user's shows" ()
          loadFrame $ do
            Lucid.div_ [Lucid.class_ "bg-white border-2 border-red-600 p-8 text-center"] $ do
              Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 text-red-600"] "Error"
              Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "Failed to load your shows."
              Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Back to Dashboard"
        Right userShows -> do
          -- Get upcoming dates for the user's first show (primary show)
          upcomingDates <- case userShows of
            [] -> pure []
            (primaryShow : _) -> do
              datesResult <- execQuerySpan (Show.getUpcomingShowDates primaryShow.id 4)
              case datesResult of
                Left _err -> do
                  Log.logInfo "Failed to fetch upcoming show dates" primaryShow.id
                  pure []
                Right dates -> pure dates

          loadFrameWithUser userMetadata (episodeUploadForm userShows upcomingDates)

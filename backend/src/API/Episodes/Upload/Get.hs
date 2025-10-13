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
import Data.Text (Text)
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
episodeUploadGetUrl :: Text -> Links.URI
episodeUploadGetUrl showSlug = Links.linkURI $ episodeUploadGetLink showSlug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /shows/:show_slug/episodes/upload"
    ( "shows"
        :> Servant.Capture "show_slug" Text
        :> "episodes"
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
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      -- Redirect to login
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- Fetch the specific show by slug
      showResult <- execQuerySpan (Shows.getShowBySlug showSlug)
      case showResult of
        Left _err -> do
          Log.logInfo "Failed to fetch show" showSlug
          renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
        Right Nothing -> do
          Log.logInfo "Show not found" showSlug
          renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
        Right (Just showModel) -> do
          -- Verify user is host of this show
          isHostResult <- execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id)
          case isHostResult of
            Left _err -> do
              Log.logInfo "Failed to check host permissions" showSlug
              renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
            Right False -> do
              Log.logInfo "User is not host of show" (user.mId, showSlug)
              renderTemplate hxRequest (Just userMetadata) showLoadErrorTemplate
            Right True -> do
              -- Get upcoming dates for this show
              datesResult <- execQuerySpan (Shows.getUpcomingShowDates showModel.id 4)
              let upcomingDates = case datesResult of
                    Left _err -> []
                    Right dates -> dates

              renderTemplate hxRequest (Just userMetadata) $ episodeUploadForm showModel upcomingDates

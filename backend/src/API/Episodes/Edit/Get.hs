{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Edit.Get where

--------------------------------------------------------------------------------

import API.Episodes.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Episodes.Edit.Get.Templates.Form (template)
import App.Auth qualified as Auth
import App.Common (renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
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
    "GET /episodes/:id/edit"
    ( "episodes"
        :> Servant.Capture "id" Episode.EpisodeId
        :> "edit"
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
  Episode.EpisodeId ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer episodeId cookie (foldHxReq -> hxRequest) = do
  checkAuth hxRequest cookie $ \user userMetadata -> do
    -- Fetch the episode
    episodeResult <- execQuerySpan (Episode.getEpisodeById episodeId)
    case episodeResult of
      Left _err -> do
        renderTemplate hxRequest (Just userMetadata) notFoundTemplate
      Right Nothing ->
        renderTemplate hxRequest (Just userMetadata) notFoundTemplate
      Right (Just episode) ->
        -- Check if user is authorized to edit this episode
        if episode.createdBy == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
          then do
            -- Fetch the show for this episode
            showResult <- execQuerySpan (Show.getShowById episode.showId)
            case showResult of
              Left _err ->
                renderTemplate hxRequest (Just userMetadata) notFoundTemplate
              Right Nothing ->
                renderTemplate hxRequest (Just userMetadata) notFoundTemplate
              Right (Just s) -> do
                Log.logInfo "Authorized user accessing episode edit form" episode.id
                let editTemplate = template episode s userMetadata
                renderTemplate hxRequest (Just userMetadata) editTemplate
          else do
            Log.logInfo "User tried to edit episode they don't own" episode.id
            renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

checkAuth ::
  ( MonadDB m,
    MonadCatch m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  HxRequest ->
  Maybe Cookie ->
  (User.Model -> UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
checkAuth hxRequest cookie k = do
  Auth.userLoginState (coerce cookie) >>= \case
    Auth.IsNotLoggedIn -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right (Just userMetadata) -> k user userMetadata
        _ -> do
          Log.logInfo "Failed to fetch user metadata for episode edit" user.mId
          renderTemplate hxRequest Nothing notLoggedInTemplate

{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episode.Edit.Get where

--------------------------------------------------------------------------------

import API.Episode.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Episode.Edit.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
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
    "GET /shows/:show_slug/episodes/:episode_slug/edit"
    ( "shows"
        :> Servant.Capture "show_slug" Text
        :> "episodes"
        :> Servant.Capture "episode_slug" Text
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
  Text ->
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      execQuerySpan (Episodes.getEpisodeBySlug showSlug episodeSlug) >>= \case
        Left err -> do
          Log.logAttention "getEpisodeBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No episode with slugs: '" <> showSlug <> "' / '" <> episodeSlug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just episode) -> do
          -- Fetch show info
          execQuerySpan (Shows.getShowById episode.showId) >>= \case
            Left err -> do
              Log.logAttention "getShowById execution error" (show err)
              renderTemplate hxRequest (Just userMetadata) notFoundTemplate
            Right Nothing -> do
              Log.logInfo "Episode's show not found" episode.showId
              renderTemplate hxRequest (Just userMetadata) notFoundTemplate
            Right (Just showModel) -> do
              -- Check if user is the creator, a host of this show, or is staff+
              execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id) >>= \case
                Left err -> do
                  Log.logAttention "isUserHostOfShow execution error" (show err)
                  renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate
                Right True -> do
                  -- User is a host, fetch tracks and show form
                  Log.logInfo "Authorized user accessing episode edit form" episode.id
                  tracksResult <- execQuerySpan (Episodes.getTracksForEpisode episode.id)
                  let tracks = case tracksResult of
                        Right ts -> ts
                        Left _ -> []
                      isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                      editTemplate = template showModel episode tracks userMetadata isStaff
                  renderTemplate hxRequest (Just userMetadata) editTemplate
                Right False ->
                  -- Check if staff or creator
                  if UserMetadata.isStaffOrHigher userMetadata.mUserRole || episode.createdBy == user.mId
                    then do
                      Log.logInfo "Staff/Creator user accessing episode edit form" episode.id
                      tracksResult <- execQuerySpan (Episodes.getTracksForEpisode episode.id)
                      let tracks = case tracksResult of
                            Right ts -> ts
                            Left _ -> []
                          isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                          editTemplate = template showModel episode tracks userMetadata isStaff
                      renderTemplate hxRequest (Just userMetadata) editTemplate
                    else do
                      Log.logInfo "User tried to edit episode they didn't create" episode.id
                      renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

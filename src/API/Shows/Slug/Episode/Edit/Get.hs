{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Edit.Get where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Shows.Slug.Episode.Edit.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Slug (Slug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowHost qualified as ShowHost
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Hasql.Transaction qualified as HT
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
        :> Servant.Capture "show_slug" Slug
        :> "episodes"
        :> Servant.Capture "episode_slug" Slug
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
  Slug ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer showSlug episodeSlug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      -- NOTE: Experimental use of a 'HT.Transaction' here. We aren't applying
      -- these globally yet due to difficulty logging the raw sql. However, I
      -- want a reference in the repo in case we end up needing a transaction
      -- somewhere.
      mResult <- execTransactionSpan $ runMaybeT $ do
        episode <- MaybeT $ HT.statement () (Episodes.getEpisodeBySlug showSlug episodeSlug)
        showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
        tracks <- lift $ HT.statement () (Episodes.getTracksForEpisode episode.id)
        -- Admins don't need explicit host check since they have access to all shows
        isHost <-
          if UserMetadata.isAdmin userMetadata.mUserRole
            then pure True
            else lift $ HT.statement () (ShowHost.isUserHostOfShow user.mId episode.showId)
        MaybeT $ pure $ Just (episode, showResult, tracks, isHost)

      case mResult of
        Left err -> do
          Log.logAttention "getEpisodeById execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No episode : '" <> display episodeSlug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (episode, showModel, tracks, isHost)) ->
          if episode.createdBy == user.mId || isHost || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              Log.logInfo "Authorized user accessing episode edit form" episode.id
              let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole
                  editTemplate = template showModel episode tracks userMetadata isStaff
              renderTemplate hxRequest (Just userMetadata) editTemplate
            else do
              Log.logInfo "User tried to edit episode they don't own" episode.id
              renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

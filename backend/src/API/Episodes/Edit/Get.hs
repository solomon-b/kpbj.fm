{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Edit.Get where

--------------------------------------------------------------------------------

import API.Episodes.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Episodes.Edit.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Episodes qualified as Episodes
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
    "GET /episodes/:id/edit"
    ( "episodes"
        :> Servant.Capture "id" Episodes.Id
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
  Episodes.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer episodeId cookie (foldHxReq -> hxRequest) = do
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
        episode <- MaybeT $ HT.statement () (Episodes.getEpisodeById episodeId)
        showResult <- MaybeT $ HT.statement () (Shows.getShowById episode.showId)
        MaybeT $ pure $ Just (episode, showResult)

      case mResult of
        Left err -> do
          Log.logAttention "getEpisodeById execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No episode with ID: '" <> display episodeId <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (episode, showResult)) ->
          if episode.createdBy == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              Log.logInfo "Authorized user accessing episode edit form" episode.id
              let editTemplate = template episode showResult userMetadata
              renderTemplate hxRequest (Just userMetadata) editTemplate
            else do
              Log.logInfo "User tried to edit episode they don't own" episode.id
              renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

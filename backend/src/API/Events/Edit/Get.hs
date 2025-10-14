{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Events.Edit.Get where

--------------------------------------------------------------------------------

import API.Events.Edit.Get.Templates.Error (notAuthorizedTemplate, notFoundTemplate, notLoggedInTemplate)
import API.Events.Edit.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Maybe
import Data.Has (Has)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execTransactionSpan)
import Effects.Database.Tables.Events qualified as Events
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
    "GET /events/:slug/edit"
    ( "events"
        :> Servant.Capture "slug" Text
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
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized access to event edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Just (user, userMetadata) -> do
      mResult <- execTransactionSpan $ runMaybeT $ do
        event <- MaybeT $ HT.statement () (Events.getEventBySlug slug)
        tags <- lift $ HT.statement () (Events.getEventTags event.emId)
        MaybeT $ pure $ Just (event, tags)

      case mResult of
        Left err -> do
          Log.logAttention "getEventBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No event with slug: '" <> display slug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just (event, tags)) ->
          if event.emAuthorId == User.mId user || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              Log.logInfo "Authorized user accessing event edit form" event.emId
              let editTemplate = template event tags userMetadata
              renderTemplate hxRequest (Just userMetadata) editTemplate
            else do
              Log.logInfo "User tried to edit event they don't own" event.emId
              renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

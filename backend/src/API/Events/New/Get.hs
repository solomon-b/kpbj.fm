{-# LANGUAGE ViewPatterns #-}

module API.Events.New.Get where

--------------------------------------------------------------------------------

import API.Events.New.Get.Templates.Auth (notAuthorizedTemplate)
import API.Events.New.Get.Templates.Form (template)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
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
    "GET /events/new"
    ( "events"
        :> "new"
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
  getUserInfo cookie $ \case
    Nothing -> do
      Log.logInfo "Unauthorized access to event creation form" ()
      renderTemplate hxRequest Nothing notAuthorizedTemplate
    Just (_user, userMetadata) ->
      if UserMetadata.isStaffOrHigher userMetadata.mUserRole
        then do
          Log.logInfo "Authorized user accessing event creation form" userMetadata.mDisplayName
          let formTemplate = template userMetadata
          renderTemplate hxRequest (Just userMetadata) formTemplate
        else do
          Log.logInfo "User without Staff/Admin role tried to create event" userMetadata.mDisplayName
          renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

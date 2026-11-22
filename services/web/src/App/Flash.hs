{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

-- | A route combinator that gives the handler the flash message of the
-- request.
--
-- A redirect with a flash message sets the @_flash@ cookie. The next page
-- reads it with this combinator and gives it to its frame, which renders the
-- banner. "Middleware.Flash" then deletes the cookie.
--
-- > type Route =
-- >   "dashboard" :> "episodes"
-- >     :> RequireRole "cookie-auth" 'Host
-- >     :> ReadFlash
-- >     :> Servant.Header "HX-Request" HxRequest
-- >     :> Servant.Get '[HTML] (Lucid.Html ())
--
-- The handler gets a @Maybe FlashMessage@. A missing or malformed cookie
-- gives 'Nothing', and never fails the request.
module App.Flash
  ( ReadFlash,
  )
where

--------------------------------------------------------------------------------

import Component.Flash (FlashMessage, lookupFlash)
import Data.Proxy (Proxy (..))
import Network.Wai qualified as Wai
import Servant ((:>))
import Servant qualified
import Servant.Server.Internal (Delayed, Router, passToServer)

--------------------------------------------------------------------------------

-- | Pass the flash message of the request to the handler.
data ReadFlash

instance (Servant.HasServer api context) => Servant.HasServer (ReadFlash :> api) context where
  type ServerT (ReadFlash :> api) m = Maybe FlashMessage -> Servant.ServerT api m

  hoistServerWithContext ::
    Proxy (ReadFlash :> api) ->
    Proxy context ->
    (forall x. m x -> n x) ->
    (Maybe FlashMessage -> Servant.ServerT api m) ->
    Maybe FlashMessage ->
    Servant.ServerT api n
  hoistServerWithContext _ context nt server =
    Servant.hoistServerWithContext (Proxy @api) context nt . server

  route ::
    Proxy (ReadFlash :> api) ->
    Servant.Context context ->
    Delayed env (Maybe FlashMessage -> Servant.Server api) ->
    Router env
  route _ context server =
    Servant.route (Proxy @api) context $
      passToServer server (lookupFlash . Wai.requestHeaders)

-- | The marker adds nothing to a link.
instance (Servant.HasLink api) => Servant.HasLink (ReadFlash :> api) where
  type MkLink (ReadFlash :> api) a = Servant.MkLink api a

  toLink ::
    (Servant.Link -> a) ->
    Proxy (ReadFlash :> api) ->
    Servant.Link ->
    Servant.MkLink api a
  toLink toA _ = Servant.toLink toA (Proxy @api)

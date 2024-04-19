{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth where

--------------------------------------------------------------------------------

import DB.Utils qualified
import Data.Text.Encoding qualified as Text.Encoding
import Effects.User (EmailAddress (..), Password (..), User, UserF (..), userFSchema)
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 ((&&.), (==.))
import Rel8 qualified
import Servant.Auth.Server qualified as Servant.Auth

--------------------------------------------------------------------------------
-- User Auth

type instance Servant.Auth.BasicAuthCfg = Servant.Auth.BasicAuthData -> IO (Servant.Auth.AuthResult User)

instance Servant.Auth.FromBasicAuthData User where
  fromBasicAuthData authData authCheckFunction = authCheckFunction authData

-- TODO: Hash password!
checkAuth :: HSQL.Pool -> Log.Logger -> Servant.Auth.BasicAuthData -> IO (Servant.Auth.AuthResult User)
checkAuth pool logger (Servant.Auth.BasicAuthData email' pass') =
  let email = EmailAddress $ Text.Encoding.decodeUtf8 email'
      pass = Password $ Text.Encoding.decodeUtf8 pass'
   in HSQL.use pool (HSQL.statement () (userByEmailQuery email pass)) >>= \case
        Left err -> do
          Log.runLogT "kpbj-backend" logger Log.defaultLogLevel $
            Log.logAttention "SQL Error" (show err)
          pure Servant.Auth.Indefinite
        Right (Just (DB.Utils.parseModel -> user)) ->
          pure $ Servant.Auth.Authenticated user
        Right Nothing ->
          pure Servant.Auth.NoSuchUser

userByEmailQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (UserF Rel8.Result))
userByEmailQuery (EmailAddress email) (Password pass) =
  Rel8.runMaybe . Rel8.select $ do
    userF <- Rel8.each userFSchema
    Rel8.where_ $
      userFEmail userF ==. Rel8.litExpr email &&. Rel8.litExpr pass ==. userFPassword userF
    pure userF

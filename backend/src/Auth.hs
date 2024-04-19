{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Auth where

--------------------------------------------------------------------------------

import DB.Utils qualified
import Data.Text.Encoding qualified as Text.Encoding
import Effects.User (EmailAddress (..), Password (..), User, selectUserByCredentialQuery)
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Log qualified
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
   in HSQL.use pool (HSQL.statement () (selectUserByCredentialQuery email pass)) >>= \case
        Left err -> do
          Log.runLogT "kpbj-backend" logger Log.defaultLogLevel $
            Log.logAttention "SQL Error" (show err)
          pure Servant.Auth.Indefinite
        Right (Just (DB.Utils.parseModel -> user)) ->
          pure $ Servant.Auth.Authenticated user
        Right Nothing ->
          pure Servant.Auth.NoSuchUser

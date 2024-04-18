{-# LANGUAGE ViewPatterns #-}

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
import Servant qualified

--------------------------------------------------------------------------------
-- User Auth

checkBasicAuth :: HSQL.Pool -> Log.Logger -> Servant.BasicAuthCheck User
checkBasicAuth pool logger = Servant.BasicAuthCheck $ \basicAuthData ->
  let email = EmailAddress $ Text.Encoding.decodeUtf8 (Servant.basicAuthUsername basicAuthData)
      pass = Password $ Text.Encoding.decodeUtf8 (Servant.basicAuthPassword basicAuthData)
   in HSQL.use pool (HSQL.statement () (userByEmailQuery email pass)) >>= \case
        Left err -> do
          Log.runLogT "kpbj-backend" logger Log.defaultLogLevel $
            Log.logAttention "SQL Error" (show err)
          pure Servant.Unauthorized
        Right (Just (DB.Utils.parseModel -> user)) ->
          pure $ Servant.Authorized user
        Right Nothing ->
          pure Servant.Unauthorized

userByEmailQuery :: EmailAddress -> Password -> HSQL.Statement () (Maybe (UserF Rel8.Result))
userByEmailQuery (EmailAddress email) (Password pass) =
  Rel8.runMaybe . Rel8.select $ do
    userF <- Rel8.each userFSchema
    Rel8.where_ $
      userFEmail userF ==. Rel8.litExpr email &&. Rel8.litExpr pass ==. userFPassword userF
    pure userF

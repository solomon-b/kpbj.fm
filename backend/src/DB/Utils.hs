module DB.Utils where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.Has (Has)
import Data.Has qualified as Has
import Data.Text qualified as Text
import Hasql.Pool qualified as HSQL
import Hasql.Session qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Servant qualified

--------------------------------------------------------------------------------
-- DB

execQuerySpan ::
  ( Log.MonadLog m,
    MonadIO m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuerySpan statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" $ Text.pack $ Char8.unpack bs
  conn <- Reader.asks Has.getter
  liftIO $ HSQL.use conn (HSQL.statement () statement)

execQuerySpanThrow ::
  ( Log.MonadLog m,
    Show result,
    MonadError Servant.ServerError m,
    MonadIO m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  HSQL.Statement () result ->
  m result
execQuerySpanThrow statement = do
  execQuerySpan statement >>= \case
    Left _err -> throwError $ Servant.err500 {Servant.errBody = "Something went wrong"}
    Right res -> pure res

execQuerySpanThrowMessage ::
  ( Log.MonadLog m,
    Show result,
    MonadError Servant.ServerError m,
    MonadIO m,
    MonadReader env m,
    Has HSQL.Pool env
  ) =>
  BL.ByteString ->
  HSQL.Statement () result ->
  m result
execQuerySpanThrowMessage msg statement = do
  execQuerySpan statement >>= \case
    Left _err -> throwError $ Servant.err500 {Servant.errBody = msg}
    Right res -> pure res

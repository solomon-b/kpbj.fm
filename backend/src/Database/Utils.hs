{-# LANGUAGE KindSignatures #-}

module Database.Utils where

--------------------------------------------------------------------------------

import Control.Monad.Except (MonadError, throwError)
import Data.ByteString.Char8 qualified as Char8
import Data.ByteString.Lazy qualified as BL
import Data.Kind (Type)
import Data.Text qualified as Text
import Database.Class
import Hasql.Pool qualified as HSQL
import Hasql.Statement qualified as HSQL
import Log qualified
import Rel8 qualified
import Servant qualified

--------------------------------------------------------------------------------
-- Working with Models and Domains

class ModelParser (model :: (Type -> Type) -> Type) (domain :: Type) where
  parseModel :: model Rel8.Result -> domain

class ModelPrinter (model :: (Type -> Type) -> Type) (domain :: Type) where
  printModel :: domain -> model Rel8.Expr

--------------------------------------------------------------------------------
-- Query Execution

execQuerySpan ::
  ( Log.MonadLog m,
    MonadDB m
  ) =>
  HSQL.Statement () result ->
  m (Either HSQL.UsageError result)
execQuerySpan statement@(HSQL.Statement bs _ _ _) = do
  Log.logInfo "db query" $ Text.pack $ Char8.unpack bs
  execStatement statement

execQuerySpanThrow ::
  ( Log.MonadLog m,
    Show result,
    MonadError Servant.ServerError m,
    MonadDB m
  ) =>
  HSQL.Statement () result ->
  m result
execQuerySpanThrow statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throwError $ Servant.err500 {Servant.errBody = "Something went wrong"}
    Right res -> pure res

execQuerySpanThrowMessage ::
  ( Log.MonadLog m,
    MonadError Servant.ServerError m,
    MonadDB m
  ) =>
  BL.ByteString ->
  HSQL.Statement () result ->
  m result
execQuerySpanThrowMessage msg statement = do
  execQuerySpan statement >>= \case
    Left err -> do
      Log.logAttention "Query Execution Error" (show err)
      throwError $ Servant.err500 {Servant.errBody = msg}
    Right res -> pure res

execQuerySpanThrowMessage' ::
  ( Log.MonadLog m,
    MonadError Servant.ServerError m,
    MonadDB m,
    ModelParser result domain
  ) =>
  BL.ByteString ->
  HSQL.Statement () (result Rel8.Result) ->
  m domain
execQuerySpanThrowMessage' msg statement =
  parseModel <$> execQuerySpanThrowMessage msg statement

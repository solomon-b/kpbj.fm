module Tracing where

--------------------------------------------------------------------------------

import Control.Exception (bracket)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchAll)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Has qualified as Has
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import OpenTelemetry.Exporter.Handle (defaultFormatter, stdoutExporter')
import OpenTelemetry.Processor.Simple (SimpleProcessorConfig (SimpleProcessorConfig), simpleProcessor)
import OpenTelemetry.Trace qualified as OTEL

--------------------------------------------------------------------------------

withTracer :: (OTEL.TracerProvider -> (OTEL.TracerOptions -> OTEL.Tracer) -> IO c) -> IO c
withTracer f =
  bracket
    -- Install the SDK, pulling configuration from the environment
    ( do
        providerOpts <- snd <$> OTEL.getTracerProviderInitializationOptions
        processor <-
          simpleProcessor . SimpleProcessorConfig $
            stdoutExporter' (pure . defaultFormatter)
        OTEL.createTracerProvider [processor] providerOpts
    )
    -- Ensure that any spans that haven't been exported yet are flushed
    OTEL.shutdownTracerProvider
    -- Get a tracer so you can create spans
    (\tracerProvider -> f tracerProvider $ OTEL.makeTracer tracerProvider "kpbj-fm")

handlerSpan ::
  ( MonadReader env m,
    Show req,
    Show res,
    Has.Has OTEL.Tracer env,
    MonadIO m,
    Display req,
    MonadCatch m,
    MonadUnliftIO m,
    Display res
  ) =>
  Text ->
  req ->
  (a -> res) ->
  m a ->
  m a
handlerSpan handlerName req getRes handlerAction = do
  tracer <- Reader.asks Has.getter
  OTEL.inSpan' tracer ("handler " <> handlerName) OTEL.defaultSpanArguments $ \reqSpan -> do
    OTEL.addEvent reqSpan $
      OTEL.NewEvent
        { newEventName = "handler request",
          newEventAttributes = HashMap.fromList [("request", OTEL.toAttribute . display $ req)],
          newEventTimestamp = Nothing
        }

    handlerResult <-
      handlerAction `catchAll` \exception -> do
        OTEL.addEvent reqSpan $
          OTEL.NewEvent
            { newEventName = "handler error",
              newEventAttributes = HashMap.fromList [("error", OTEL.toAttribute . Text.pack . show $ exception)],
              newEventTimestamp = Nothing
            }
        throwM exception

    OTEL.addEvent reqSpan $
      OTEL.NewEvent
        { newEventName = "handler success",
          newEventAttributes = HashMap.fromList [("response", OTEL.toAttribute . display . getRes $ handlerResult)],
          newEventTimestamp = Nothing
        }

    pure handlerResult

module Tracing where

--------------------------------------------------------------------------------

import Config (Environment (..))
import Control.Exception (bracket)
import Control.Monad.Catch (MonadCatch, MonadThrow (..), catchAll)
import Control.Monad.IO.Unlift
import Control.Monad.Reader (MonadReader)
import Control.Monad.Reader qualified as Reader
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Coerce (coerce)
import Data.Has qualified as Has
import Data.HashMap.Strict qualified as HashMap
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display
import Data.Text.Lazy qualified as Lazy
import Data.Text.Lazy.Encoding qualified as Lazy.Text.Encoding
import Data.Time.Clock.POSIX qualified as Time
import Log qualified
import OpenTelemetry.Common (Timestamp (..))
import OpenTelemetry.Exporter.Handle (stdoutExporter')
import OpenTelemetry.Internal.Trace.Id (Base (..), spanIdBaseEncodedText, traceIdBaseEncodedText)
import OpenTelemetry.Processor.Simple (SimpleProcessorConfig (SimpleProcessorConfig), simpleProcessor)
import OpenTelemetry.Trace (ImmutableSpan (..))
import OpenTelemetry.Trace qualified as OTEL
import System.Clock (toNanoSecs)

--------------------------------------------------------------------------------

formatter :: Text -> OTEL.ImmutableSpan -> IO Lazy.Text
formatter component ImmutableSpan {..} = do
  now <- Time.getPOSIXTime
  pure $
    Lazy.Text.Encoding.decodeUtf8 $
      Aeson.encode $
        Aeson.object
          [ "component" .= component,
            "time" .= now,
            "level" .= Log.LogInfo,
            "message" .= spanName,
            "data"
              .= Aeson.object
                [ "traceId" .= Text.unpack (traceIdBaseEncodedText Base16 $ OTEL.traceId spanContext),
                  "spanId" .= Text.unpack (spanIdBaseEncodedText Base16 $ OTEL.spanId spanContext),
                  "spanStart" .= show (toNanoSecs $ coerce spanStart),
                  "spanName" .= spanName,
                  "spanParent" .= show spanParent,
                  "spanAttributes" .= show spanAttributes
                ]
          ]

withTracer :: Environment -> (OTEL.TracerProvider -> (OTEL.TracerOptions -> OTEL.Tracer) -> IO c) -> IO c
withTracer env f =
  let acquire = case env of
        -- TODO: Use jaeger or some other tracing service in production:
        -- Production -> OTEL.initializeGlobalTracerProvider
        _ -> do
          providerOpts <- snd <$> OTEL.getTracerProviderInitializationOptions
          processor <-
            simpleProcessor . SimpleProcessorConfig $
              stdoutExporter' (formatter "kpbj-backend")
          OTEL.createTracerProvider [processor] providerOpts
      release = OTEL.shutdownTracerProvider
      work tracerProvider = f tracerProvider $ OTEL.makeTracer tracerProvider "kpbj-fm"
   in bracket acquire release work

handlerSpan ::
  ( MonadReader env m,
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
        liftIO $ print exception
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

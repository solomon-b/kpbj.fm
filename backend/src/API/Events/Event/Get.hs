{-# LANGUAGE QuasiQuotes #-}

module API.Events.Event.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (eventsGetLink)
import App.Auth qualified as Auth
import Component.Frame (loadContentOnly, loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.Cont (ContT (..), evalContT)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Control.Monad.Trans (MonadTrans (lift))
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventsGetTagUrl :: Text -> Links.URI
eventsGetTagUrl tagName = Links.linkURI $ eventsGetLink (Just tagName) Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /events/:slug"
    ( "events"
        :> Servant.Capture "slug" Text
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render event description with simple paragraph breaks
renderEventDescription :: Text -> Lucid.Html ()
renderEventDescription description = do
  let paragraphs = Text.splitOn "\n\n" description
  Lucid.div_ [Lucid.class_ "prose max-w-none text-gray-700 leading-relaxed space-y-4"] $ do
    mapM_ renderParagraph paragraphs
  where
    renderParagraph para =
      if Text.null (Text.strip para)
        then pure ()
        else Lucid.p_ $ Lucid.toHtml para

-- | Render a single tag
renderTag :: Events.EventTagDomain -> Lucid.Html ()
renderTag tag =
  Lucid.a_
    [ Lucid.href_ [i|/#{eventsGetTagUrl (Events.etdName tag)}|],
      hxGet_ [i|/#{eventsGetTagUrl (Events.etdName tag)}|],
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "bg-gray-200 text-gray-800 px-2 py-1 text-sm font-mono hover:bg-gray-300 no-underline"
    ]
    $ Lucid.toHtml ("#" <> tag.etdName)

-- | Main event template
template :: Events.EventModel -> [Events.EventTagDomain] -> UserMetadata.Domain -> Lucid.Html ()
template event eventTags author = do
  -- Event Header Section
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    Lucid.div_ [Lucid.class_ "grid grid-cols-1 lg:grid-cols-4 gap-8"] $ do
      -- Event Image
      Lucid.div_ [Lucid.class_ "lg:col-span-1"] $ do
        Lucid.div_
          [Lucid.class_ "w-full aspect-square bg-gray-300 border-2 border-gray-600 flex items-center justify-center text-lg"]
          "[CONCERT POSTER]"

      -- Event Header Info
      Lucid.div_ [Lucid.class_ "lg:col-span-3 flex flex-col"] $ do
        -- Title
        Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-6"] $
          Lucid.toHtml event.emTitle

        -- Key Details
        Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6 mb-6"] $ do
          -- Date and Time
          Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
            Lucid.span_ [Lucid.class_ "text-2xl"] "📅"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ "font-bold"] "DATE & TIME"
              Lucid.div_ [Lucid.class_ "text-gray-600"] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%A, %B %d, %Y" event.emStartsAt
              Lucid.div_ [Lucid.class_ "text-gray-600"] $
                Lucid.toHtml $
                  formatTime defaultTimeLocale "%l:%M %p" event.emStartsAt
                    <> " - "
                    <> formatTime defaultTimeLocale "%l:%M %p" event.emEndsAt

          -- Location
          Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
            Lucid.span_ [Lucid.class_ "text-2xl"] "📍"
            Lucid.div_ $ do
              Lucid.div_ [Lucid.class_ "font-bold"] "LOCATION"
              Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationName
              Lucid.div_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml event.emLocationAddress

        -- Tags (pushed to bottom right)
        Lucid.div_ [Lucid.class_ "flex justify-end mt-auto"] $ do
          Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2"] $ do
            mapM_ renderTag eventTags

  -- Event Description Section (Full Width)
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 mb-8"] $ do
    renderEventDescription event.emDescription

    -- Event creator info
    Lucid.div_ [Lucid.class_ "mt-6 p-4 bg-gray-50 border-l-4 border-gray-800"] $ do
      Lucid.div_ [Lucid.class_ "flex items-start gap-4"] $ do
        Lucid.div_ [Lucid.class_ "w-12 h-12 bg-gray-300 rounded-full flex items-center justify-center text-sm"] $
          Lucid.toHtml $
            Text.take 2 (display author.dDisplayName)
        Lucid.div_ $ do
          Lucid.h3_ [Lucid.class_ "font-bold mb-1"] $
            Lucid.toHtml ("Event created by " <> display author.dDisplayName)
          Lucid.p_ [Lucid.class_ "text-sm text-gray-600"] $
            Lucid.toHtml $
              "KPBJ " <> Text.pack (show author.dUserRole) <> " • " <> display author.dFullName

  -- Navigation back to events
  Lucid.div_ [Lucid.class_ "text-center"] $ do
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO EVENTS"

-- | Template for when event is not found
notFoundTemplate :: Text -> Lucid.Html ()
notFoundTemplate slug = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h1_ [Lucid.class_ "text-3xl font-bold mb-4"] "Event Not Found"
    Lucid.p_ [Lucid.class_ "text-gray-600 mb-6"] $ do
      "The event with slug \""
      Lucid.code_ [Lucid.class_ "bg-gray-100 px-2 py-1"] $ Lucid.toHtml slug
      "\" could not be found."
    Lucid.a_
      [ Lucid.href_ [i|/#{eventsGetUrl}|],
        hxGet_ [i|/#{eventsGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO EVENTS"

-- | Render template with proper HTMX handling
renderTemplate :: (Log.MonadLog m, MonadCatch m) => Bool -> Maybe UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
renderTemplate isHtmxRequest mUserInfo templateContent =
  case mUserInfo of
    Just userInfo ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrameWithUser userInfo templateContent
    Nothing ->
      if isHtmxRequest
        then loadContentOnly templateContent
        else loadFrame templateContent

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
  Maybe Text ->
  Maybe Text ->
  m (Lucid.Html ())
handler _tracer slug cookie hxRequest = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  evalContT $ do
    mUserInfo <- ContT $ getUserInfo cookie
    event <- ContT $ getEvent isHtmxRequest mUserInfo slug
    author <- ContT $ getAuthor isHtmxRequest mUserInfo slug event
    lift $ fetchTags isHtmxRequest mUserInfo event author

checkHtmxRequest :: Maybe Text -> Bool
checkHtmxRequest = \case
  Just "true" -> True
  _ -> False

getUserInfo ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  Maybe Text ->
  (Maybe UserMetadata.Model -> m a) ->
  m a
getUserInfo cookie k =
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      k Nothing
    Auth.IsLoggedIn user ->
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right (Just userMetadata) ->
          k $ Just userMetadata
        _ ->
          k Nothing

getEvent ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  Maybe UserMetadata.Model ->
  Text ->
  (Events.EventModel -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
getEvent isHtmxRequest mUserInfo slug k =
  execQuerySpan (Events.getEventBySlug slug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch event from database" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logInfo "Event not found" slug
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just event) ->
      k event

getAuthor ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  -- | Logged in user:
  Maybe UserMetadata.Model ->
  Text ->
  Events.EventModel ->
  -- | Author of the event:
  (UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
getAuthor isHtmxRequest mUserInfo slug event k =
  execQuerySpan (UserMetadata.getUserMetadata event.emAuthorId) >>= \case
    Left err -> do
      Log.logAttention "Failed to fetch event author" (Aeson.object ["id" .= event.emAuthorId, "error" .= show err])
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right Nothing -> do
      Log.logAttention "Event author not found" (Aeson.object ["id" .= event.emAuthorId])
      renderTemplate isHtmxRequest mUserInfo (notFoundTemplate slug)
    Right (Just author) ->
      k author

fetchTags ::
  ( Log.MonadLog m,
    MonadDB m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  Bool ->
  Maybe UserMetadata.Model ->
  Events.EventModel ->
  UserMetadata.Model ->
  m (Lucid.Html ())
fetchTags isHtmxRequest mUserInfo event author =
  execQuerySpan (Events.getEventTags event.emId) >>= \case
    Left err -> do
      Log.logInfo "Failed to fetch event tags" (Aeson.object ["event" .= event.emId, "error" .= show err])
      let eventTemplate = template event [] (UserMetadata.toDomain author)
      renderTemplate isHtmxRequest mUserInfo eventTemplate
    Right eventTagModels -> do
      let eventTags = map Events.toDomainEventTag eventTagModels
      let eventTemplate = template event eventTags (UserMetadata.toDomain author)
      renderTemplate isHtmxRequest mUserInfo eventTemplate

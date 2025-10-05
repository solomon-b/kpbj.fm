{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Edit.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, userLoginGetLink)
import App.Auth qualified as Auth
import App.Common (renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Coerce (coerce)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episode qualified as Episode
import Effects.Database.Tables.Show qualified as Show
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
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLoginGetLink Nothing Nothing

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /episodes/:id/edit"
    ( "episodes"
        :> Servant.Capture "id" Episode.EpisodeId
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Episode edit template
template :: Episode.EpisodeModel -> Show.ShowModel -> UserMetadata.Model -> Lucid.Html ()
template episode s userMeta = do
  let episodeId = episode.id
  -- Form Header
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT EPISODE"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml s.title
          " • "
          Lucid.strong_ "Host: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "text-center"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO DASHBOARD"

  -- Edit Episode Form
  Lucid.form_ [Lucid.action_ [i|/episodes/#{episodeId}/edit|], Lucid.method_ "post", Lucid.class_ "space-y-8 w-full"] $ do
    -- Episode Details
    Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EPISODE DETAILS"

      Lucid.div_ [Lucid.class_ "space-y-6"] $ do
        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Title *"
          Lucid.input_
            [ Lucid.type_ "text",
              Lucid.name_ "title",
              Lucid.required_ "true",
              Lucid.value_ episode.title,
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "e.g. Industrial Depths #087"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Number"
          Lucid.input_
            [ Lucid.type_ "number",
              Lucid.name_ "episode_number",
              Lucid.value_ (display episode.episodeNumber),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "87"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Season Number"
          Lucid.input_
            [ Lucid.type_ "number",
              Lucid.name_ "season_number",
              Lucid.value_ (display episode.seasonNumber),
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
              Lucid.placeholder_ "1"
            ]

        Lucid.div_ $ do
          Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Description"
          Lucid.textarea_
            [ Lucid.name_ "description",
              Lucid.rows_ "6",
              Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono leading-relaxed",
              Lucid.placeholder_ "Describe this episode. What music was featured? Any special guests or themes?"
            ]
            (maybe "" Lucid.toHtml episode.description)

    -- Form Actions
    Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
      Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
        Lucid.button_
          [ Lucid.type_ "submit",
            Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
          ]
          "UPDATE EPISODE"
        Lucid.a_
          [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
            hxGet_ [i|/#{hostDashboardGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
          ]
          "CANCEL"

-- | Template for unauthorized access
notAuthorizedTemplate :: Lucid.Html ()
notAuthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit your own episodes."
    Lucid.div_ [Lucid.class_ "space-x-4"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
          hxGet_ [i|/#{hostDashboardGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
        ]
        "← BACK TO DASHBOARD"

-- | Template for users not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Login Required"
    Lucid.p_ [Lucid.class_ "mb-6"] "Please login to edit episodes."
    Lucid.a_
      [ Lucid.href_ [i|/#{userLoginGetUrl}|],
        hxGet_ [i|/#{userLoginGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"
      ]
      "LOGIN"

-- | Template for episode not found
notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Episode Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The episode you're trying to edit doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700 inline-block"
      ]
      "← BACK TO DASHBOARD"

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
  Episode.EpisodeId ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer episodeId cookie (foldHxReq -> hxRequest) = do
  checkAuth hxRequest cookie $ \user userMetadata -> do
    -- Fetch the episode
    episodeResult <- execQuerySpan (Episode.getEpisodeById episodeId)
    case episodeResult of
      Left _err -> do
        renderTemplate hxRequest (Just userMetadata) notFoundTemplate
      Right Nothing ->
        renderTemplate hxRequest (Just userMetadata) notFoundTemplate
      Right (Just episode) ->
        -- Check if user is authorized to edit this episode
        if episode.createdBy == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
          then do
            -- Fetch the show for this episode
            showResult <- execQuerySpan (Show.getShowById episode.showId)
            case showResult of
              Left _err ->
                renderTemplate hxRequest (Just userMetadata) notFoundTemplate
              Right Nothing ->
                renderTemplate hxRequest (Just userMetadata) notFoundTemplate
              Right (Just s) -> do
                Log.logInfo "Authorized user accessing episode edit form" episode.id
                let editTemplate = template episode s userMetadata
                renderTemplate hxRequest (Just userMetadata) editTemplate
          else do
            Log.logInfo "User tried to edit episode they don't own" episode.id
            renderTemplate hxRequest (Just userMetadata) notAuthorizedTemplate

checkAuth ::
  ( MonadDB m,
    MonadCatch m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env,
    Has Tracer env,
    MonadUnliftIO m
  ) =>
  HxRequest ->
  Maybe Cookie ->
  (User.Model -> UserMetadata.Model -> m (Lucid.Html ())) ->
  m (Lucid.Html ())
checkAuth hxRequest cookie k = do
  Auth.userLoginState (coerce cookie) >>= \case
    Auth.IsNotLoggedIn -> do
      Log.logInfo "Unauthorized access to episode edit" ()
      renderTemplate hxRequest Nothing notLoggedInTemplate
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata user.mId) >>= \case
        Right (Just userMetadata) -> k user userMetadata
        _ -> do
          Log.logInfo "Failed to fetch user metadata for episode edit" user.mId
          renderTemplate hxRequest Nothing notLoggedInTemplate

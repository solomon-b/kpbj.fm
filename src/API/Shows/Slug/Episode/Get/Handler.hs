{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Shows.Slug.Episode.Get.Handler where

--------------------------------------------------------------------------------

import API.Shows.Slug.Episode.Get.Templates.Page (errorTemplate, notFoundTemplate, template)
import App.Common (getUserInfo, renderTemplate)
import Component.Redirect (redirectTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Aeson ((.=))
import Data.Aeson qualified as Aeson
import Data.Either (fromRight)
import Data.Functor ((<&>))
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Slug (Slug, matchSlug, mkSlug)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant qualified

--------------------------------------------------------------------------------

-- | Handler for episode with show slug, episode ID, and slug
handlerWithSlug ::
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
  Slug ->
  Episodes.Id ->
  Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithSlug tracer showSlug episodeId slug = handler tracer showSlug episodeId (Just slug)

-- | Handler for episode with show slug and episode ID only (always redirects)
handlerWithoutSlug ::
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
  Slug ->
  Episodes.Id ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handlerWithoutSlug tracer showSlug episodeId = handler tracer showSlug episodeId Nothing

-- | Shared handler for both routes
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
  Slug ->
  Episodes.Id ->
  Maybe Slug ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
handler _tracer showSlug episodeId mUrlSlug cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd

  -- First fetch the show by slug to verify it exists
  execQuerySpan (Shows.getShowBySlug showSlug) >>= \case
    Left _err -> do
      Log.logInfo "Failed to fetch show from database" showSlug
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show. Please try again.")
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Show not found" showSlug
      html <- renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug (mkSlug "unknown"))
      pure $ Servant.noHeader html
    Right (Just showModel) -> do
      execQuerySpan (Episodes.getEpisodeById episodeId) >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch episode from database" episodeId
          html <- renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load episode. Please try again.")
          pure $ Servant.noHeader html
        Right Nothing -> do
          Log.logInfo "Episode not found" episodeId
          html <- renderTemplate hxRequest mUserInfo (notFoundTemplate showSlug (mkSlug "unknown"))
          pure $ Servant.noHeader html
        Right (Just episode) -> do
          -- Verify episode belongs to the show
          if episode.showId /= showModel.id
            then do
              Log.logInfo "Episode does not belong to show" (showSlug, episodeId)
              html <- renderTemplate hxRequest mUserInfo (errorTemplate "Episode not found in this show.")
              pure $ Servant.noHeader html
            else do
              let canonicalSlug = episode.slug
                  showSlugText = display showSlug
                  episodeIdText = display episodeId
                  slugText = display canonicalSlug
                  canonicalUrl = [i|/shows/#{showSlugText}/episodes/#{episodeIdText}/#{slugText}|]

              if matchSlug canonicalSlug mUrlSlug
                then renderEpisode hxRequest mUserInfo cookie episode
                else renderRedirect hxRequest mUserInfo canonicalUrl

renderEpisode ::
  ( Has Tracer env,
    MonadReader env m,
    Log.MonadLog m,
    MonadDB m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Maybe Cookie ->
  Episodes.Model ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderEpisode hxRequest mUserInfo _cookie episode = do
  showResult <- execQuerySpan (Shows.getShowById episode.showId)
  tracks <- fromRight [] <$> execQuerySpan (Episodes.getTracksForEpisode episode.id)

  case showResult of
    Left err -> do
      Log.logInfo "Failed to fetch show from database" (Aeson.object ["showId" .= episode.showId, "error" .= show err])
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Failed to load show data. Please try again.")
      pure $ Servant.noHeader html
    Right Nothing -> do
      Log.logInfo "Show not found for episode" (Aeson.object ["showId" .= episode.showId])
      html <- renderTemplate hxRequest mUserInfo (errorTemplate "Show not found for this episode.")
      pure $ Servant.noHeader html
    Right (Just showModel) -> do
      let episodeTemplate = template showModel episode tracks
      html <- renderTemplate hxRequest mUserInfo episodeTemplate

      pure $ Servant.noHeader html

renderRedirect ::
  ( MonadCatch m,
    Log.MonadLog m,
    MonadReader env m,
    Has HSQL.Pool.Pool env
  ) =>
  HxRequest ->
  Maybe UserMetadata.Model ->
  Text ->
  m (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
renderRedirect hxRequest mUserInfo canonicalUrl = do
  Log.logInfo "Redirecting to canonical episode URL" canonicalUrl
  html <- renderTemplate hxRequest mUserInfo (redirectTemplate canonicalUrl)
  pure $ Servant.addHeader canonicalUrl html

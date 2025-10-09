{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Episodes.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Read qualified as Text.Read
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Episodes qualified as Episodes
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
import Web.FormUrlEncoded (FromForm (..))
import Web.FormUrlEncoded qualified as Form

--------------------------------------------------------------------------------

-- URL helpers
hostDashboardGetUrl :: Links.URI
hostDashboardGetUrl = Links.linkURI hostDashboardGetLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /episodes/:id/edit"
    ( "episodes"
        :> Servant.Capture "id" Int64
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] EpisodeEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for episode editing
data EpisodeEditForm = EpisodeEditForm
  { eefTitle :: Text,
    eefEpisodeNumber :: Maybe Text,
    eefSeasonNumber :: Maybe Text,
    eefDescription :: Maybe Text
  }
  deriving (Show)

instance FromForm EpisodeEditForm where
  fromForm form = do
    title <- Form.parseUnique "title" form
    episodeNumber <- Form.parseMaybe "episode_number" form
    seasonNumber <- Form.parseMaybe "season_number" form
    description <- Form.parseMaybe "description" form
    pure
      EpisodeEditForm
        { eefTitle = title,
          eefEpisodeNumber = episodeNumber,
          eefSeasonNumber = seasonNumber,
          eefDescription = description
        }

-- | Success template after episode update
successTemplate :: Lucid.Html ()
successTemplate = do
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Episode Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your episode has been updated and saved."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
          hxGet_ [i|/#{hostDashboardGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "← BACK TO DASHBOARD"

-- | Error templates
unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit episodes."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Episode Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The episode you're trying to update doesn't exist."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

forbiddenTemplate :: Lucid.Html ()
forbiddenTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit your own episodes."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

errorTemplate :: Lucid.Html ()
errorTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Update Failed"
    Lucid.p_ [Lucid.class_ "mb-6"] "There was an error updating your episode. Please try again."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
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
  Int64 ->
  Maybe Cookie ->
  Maybe HxRequest ->
  EpisodeEditForm ->
  m (Lucid.Html ())
handler _tracer episodeId cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie $ \case
    Nothing -> do
      Log.logInfo "Unauthorized episode edit attempt" episodeId
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the episode to verify ownership
      episodeResult <- execQuerySpan (Episodes.getEpisodeById (Episodes.Id episodeId))
      case episodeResult of
        Left _err -> do
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing ->
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just episode) ->
          -- Check authorization - user must be episode creator or staff/admin
          if episode.createdBy == user.mId || UserMetadata.isStaffOrHigher userMetadata.mUserRole
            then do
              -- Parse form data
              let newEpisodeNumber = case eefEpisodeNumber editForm of
                    Nothing -> Nothing
                    Just "" -> Nothing
                    Just numStr -> case Text.Read.decimal numStr of
                      Left _ -> Nothing
                      Right (num, _) -> Just (Episodes.EpisodeNumber num)

                  newSeasonNumber = case eefSeasonNumber editForm of
                    Nothing -> episode.seasonNumber
                    Just "" -> episode.seasonNumber
                    Just numStr -> case Text.Read.decimal numStr of
                      Left _ -> episode.seasonNumber
                      Right (num, _) -> num

                  newDescription = case eefDescription editForm of
                    Nothing -> episode.description
                    Just "" -> Nothing
                    Just desc -> Just desc

                  updateData =
                    Episodes.Update
                      { euId = episode.id,
                        euTitle = eefTitle editForm,
                        euDescription = newDescription,
                        euEpisodeNumber = newEpisodeNumber,
                        euSeasonNumber = newSeasonNumber
                      }

              -- Update the episode
              updateResult <- execQuerySpan (Episodes.updateEpisode updateData)
              case updateResult of
                Left _err -> do
                  Log.logInfo "Failed to update episode" episodeId
                  renderTemplate hxRequest (Just userMetadata) errorTemplate
                Right _ -> do
                  Log.logInfo "Successfully updated episode" episodeId
                  renderTemplate hxRequest (Just userMetadata) successTemplate
            else do
              Log.logInfo "User attempted to edit episode they don't own" episodeId
              renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

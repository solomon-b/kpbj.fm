{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Show.Edit.Post where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (hostDashboardGetLink, showGetLink)
import App.Common (getUserInfo, renderTemplate)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Read (decimal)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
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

showGetUrl :: Text -> Links.URI
showGetUrl slug = Links.linkURI $ showGetLink slug

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /shows/:slug/edit"
    ( "shows"
        :> Servant.Capture "slug" Text
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.ReqBody '[Servant.FormUrlEncoded] ShowEditForm
        :> Servant.Post '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Form data for show editing
data ShowEditForm = ShowEditForm
  { sefTitle :: Text,
    sefSlug :: Text,
    sefDescription :: Text,
    sefGenre :: Maybe Text,
    sefLogoUrl :: Maybe Text,
    sefBannerUrl :: Maybe Text,
    sefStatus :: Text,
    sefFrequency :: Text,
    sefDurationMinutes :: Maybe Text
  }
  deriving (Show)

instance FromForm ShowEditForm where
  fromForm form = do
    title <- Form.parseUnique "title" form
    slug <- Form.parseUnique "slug" form
    description <- Form.parseUnique "description" form
    genre <- Form.parseMaybe "genre" form
    logoUrl <- Form.parseMaybe "logo_url" form
    bannerUrl <- Form.parseMaybe "banner_url" form
    status <- Form.parseUnique "status" form
    frequency <- Form.parseUnique "frequency" form
    durationMinutes <- Form.parseMaybe "duration_minutes" form
    pure
      ShowEditForm
        { sefTitle = title,
          sefSlug = slug,
          sefDescription = description,
          sefGenre = emptyToNothing genre,
          sefLogoUrl = emptyToNothing logoUrl,
          sefBannerUrl = emptyToNothing bannerUrl,
          sefStatus = status,
          sefFrequency = frequency,
          sefDurationMinutes = emptyToNothing durationMinutes
        }
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing x = x

-- | Parse show status from text
parseStatus :: Text -> Maybe Shows.Status
parseStatus "active" = Just Shows.Active
parseStatus "inactive" = Just Shows.Inactive
parseStatus _ = Nothing

-- | Parse show frequency from text
parseFrequency :: Text -> Maybe Shows.ShowFrequency
parseFrequency "weekly" = Just Shows.Weekly
parseFrequency "biweekly" = Just Shows.Biweekly
parseFrequency "monthly" = Just Shows.Monthly
parseFrequency "occasional" = Just Shows.Occasional
parseFrequency "one-time" = Just Shows.OneTime
parseFrequency _ = Nothing

-- | Success template after show update
successTemplate :: Text -> Lucid.Html ()
successTemplate showSlug = do
  let showUrl = showGetUrl showSlug
  Lucid.div_ [Lucid.class_ "bg-green-100 border-2 border-green-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-800"] "✓ Show Updated Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6"] "Your show has been updated and saved."
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_
        [ Lucid.href_ [i|/#{showUrl}|],
          hxGet_ [i|/#{showUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
        ]
        "VIEW SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
          hxGet_ [i|/#{hostDashboardGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-6 py-3 font-bold hover:bg-gray-500"
        ]
        "DASHBOARD"

-- | Error templates
unauthorizedTemplate :: Lucid.Html ()
unauthorizedTemplate = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Access Denied"
    Lucid.p_ [Lucid.class_ "mb-6"] "You must be logged in to edit shows."

notFoundTemplate :: Lucid.Html ()
notFoundTemplate = do
  Lucid.div_ [Lucid.class_ "bg-yellow-100 border-2 border-yellow-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-yellow-800"] "Show Not Found"
    Lucid.p_ [Lucid.class_ "mb-6"] "The show you're trying to update doesn't exist."
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
    Lucid.p_ [Lucid.class_ "mb-6"] "You can only edit shows you host or have staff permissions."
    Lucid.a_
      [ Lucid.href_ [i|/#{hostDashboardGetUrl}|],
        hxGet_ [i|/#{hostDashboardGetUrl}|],
        hxTarget_ "#main-content",
        hxPushUrl_ "true",
        Lucid.class_ "bg-gray-800 text-white px-6 py-3 font-bold hover:bg-gray-700"
      ]
      "← BACK TO DASHBOARD"

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-red-100 border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-800"] "Update Failed"
    Lucid.p_ [Lucid.class_ "mb-6"] $ Lucid.toHtml errorMsg
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
  Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  ShowEditForm ->
  m (Lucid.Html ())
handler _tracer slug cookie (foldHxReq -> hxRequest) editForm = do
  getUserInfo cookie >>= \case
    Nothing -> do
      Log.logInfo "Unauthorized show edit attempt" slug
      renderTemplate hxRequest Nothing unauthorizedTemplate
    Just (user, userMetadata) -> do
      -- Fetch the show to verify it exists and check authorization
      execQuerySpan (Shows.getShowBySlug slug) >>= \case
        Left err -> do
          Log.logAttention "getShowBySlug execution error" (show err)
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right Nothing -> do
          Log.logInfo_ $ "No show with slug: '" <> slug <> "'"
          renderTemplate hxRequest (Just userMetadata) notFoundTemplate
        Right (Just showModel) -> do
          -- Check authorization - user must be a host of the show or staff+
          execQuerySpan (Shows.isUserHostOfShow user.mId showModel.id) >>= \case
            Left err -> do
              Log.logAttention "isUserHostOfShow execution error" (show err)
              renderTemplate hxRequest (Just userMetadata) forbiddenTemplate
            Right True -> updateShow hxRequest user userMetadata showModel editForm
            Right False ->
              if UserMetadata.isStaffOrHigher userMetadata.mUserRole
                then updateShow hxRequest user userMetadata showModel editForm
                else do
                  Log.logInfo "User attempted to edit show they don't host" showModel.id
                  renderTemplate hxRequest (Just userMetadata) forbiddenTemplate

updateShow ::
  ( MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env,
    MonadUnliftIO m,
    MonadCatch m
  ) =>
  HxRequest ->
  User.Model ->
  UserMetadata.Model ->
  Shows.Model ->
  ShowEditForm ->
  m (Lucid.Html ())
updateShow hxRequest _user userMetadata showModel editForm = do
  let isStaff = UserMetadata.isStaffOrHigher userMetadata.mUserRole

  -- Parse and validate form data
  case (parseStatus (sefStatus editForm), parseFrequency (sefFrequency editForm)) of
    (Nothing, _) -> do
      Log.logInfo "Invalid status in show edit form" (sefStatus editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid show status value.")
    (_, Nothing) -> do
      Log.logInfo "Invalid frequency in show edit form" (sefFrequency editForm)
      renderTemplate hxRequest (Just userMetadata) (errorTemplate "Invalid frequency value.")
    (Just parsedStatus, Just parsedFrequency) -> do
      -- Parse duration if provided
      let mDuration = case sefDurationMinutes editForm of
            Nothing -> Nothing
            Just durText ->
              case decimal durText of
                Right (n, _) -> Just n
                Left _ -> Nothing

          -- If not staff, preserve original schedule/settings values
          finalStatus = if isStaff then parsedStatus else showModel.status
          finalFrequency = if isStaff then parsedFrequency else showModel.frequency
          finalDuration = if isStaff then mDuration else showModel.durationMinutes

          updateData =
            Shows.Insert
              { siTitle = sefTitle editForm,
                siSlug = sefSlug editForm,
                siDescription = sefDescription editForm,
                siGenre = sefGenre editForm,
                siLogoUrl = sefLogoUrl editForm,
                siBannerUrl = sefBannerUrl editForm,
                siStatus = finalStatus,
                siFrequency = finalFrequency,
                siDurationMinutes = finalDuration
              }

      -- Update the show
      execQuerySpan (Shows.updateShow showModel.id updateData) >>= \case
        Left err -> do
          Log.logInfo "Failed to update show" (showModel.id, show err)
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Database error occurred. Please try again.")
        Right Nothing -> do
          Log.logInfo "Show update returned Nothing" showModel.id
          renderTemplate hxRequest (Just userMetadata) (errorTemplate "Failed to update show. Please try again.")
        Right (Just _) -> do
          Log.logInfo "Successfully updated show" showModel.id
          renderTemplate hxRequest (Just userMetadata) (successTemplate $ sefSlug editForm)

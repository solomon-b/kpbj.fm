{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Feature.Post.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Templates.Page (renderEventRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleBannerErrors, throwDatabaseError, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified

--------------------------------------------------------------------------------

handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler eventId _slug cookie =
  handleBannerErrors "Event feature" $ do
    -- 1. Require authentication and staff role
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to feature events." userMetadata

    -- 2. Fetch event
    event <-
      execQuery (Events.getEventById eventId) >>= \case
        Left err -> throwDatabaseError err
        Right Nothing -> throwNotFound "Event"
        Right (Just e) -> pure e

    -- 3. Toggle featured status
    let newFeatured = not event.emFeaturedOnHomepage

    if newFeatured
      then do
        -- Clear existing featured event, then set this one
        mResult <- execTransaction $ do
          void $ HT.statement () Events.clearFeaturedEvents
          HT.statement () (Events.updateEvent eventId (toInsert event){Events.eiFeaturedOnHomepage = True})
        case mResult of
          Left err -> throwDatabaseError err
          Right Nothing -> throwDatabaseError (error "Event update returned Nothing")
          Right (Just _) -> do
            Log.logInfo "Event promoted to frontpage" eventId
            let updated = event{Events.emFeaturedOnHomepage = True}
            pure $ do
              renderEventRow updated
              renderBanner Success "Promoted" "Event is now featured on the homepage."
      else do
        -- Just unfeature this event
        mResult <- execQuery $ Events.updateEvent eventId (toInsert event){Events.eiFeaturedOnHomepage = False}
        case mResult of
          Left err -> throwDatabaseError err
          Right Nothing -> throwDatabaseError (error "Event update returned Nothing")
          Right (Just _) -> do
            Log.logInfo "Event removed from frontpage" eventId
            let updated = event{Events.emFeaturedOnHomepage = False}
            pure $ do
              renderEventRow updated
              renderBanner Success "Demoted" "Event is no longer featured on the homepage."

-- | Convert a Model back to an Insert for updates.
toInsert :: Events.Model -> Events.Insert
toInsert event =
  Events.Insert
    { Events.eiTitle = event.emTitle,
      Events.eiSlug = event.emSlug,
      Events.eiDescription = event.emDescription,
      Events.eiStartsAt = event.emStartsAt,
      Events.eiEndsAt = event.emEndsAt,
      Events.eiLocationName = event.emLocationName,
      Events.eiLocationAddress = event.emLocationAddress,
      Events.eiStatus = event.emStatus,
      Events.eiAuthorId = event.emAuthorId,
      Events.eiPosterImageUrl = event.emPosterImageUrl,
      Events.eiFeaturedOnHomepage = event.emFeaturedOnHomepage
    }

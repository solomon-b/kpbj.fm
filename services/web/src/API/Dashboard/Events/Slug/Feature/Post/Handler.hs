{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Events.Slug.Feature.Post.Handler (handler, action, FeatureEventViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Events.Get.Templates.Page (renderEventRow)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleBannerErrors, throwDatabaseError, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..), renderBanner)
import Control.Monad (void)
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery, execTransaction)
import Effects.Database.Tables.Events qualified as Events
import Hasql.Transaction qualified as HT
import Log qualified
import Lucid qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the feature toggle response.
data FeatureEventViewData = FeatureEventViewData
  { fevUpdatedEvent :: Events.Model,
    fevBannerType :: BannerType,
    fevBannerTitle :: Text,
    fevBannerMessage :: Text
  }

-- | Business logic: fetch event, toggle featured status.
action ::
  Events.Id ->
  ExceptT HandlerError AppM FeatureEventViewData
action eventId = do
  -- 1. Fetch event
  event <-
    fromMaybeM (throwNotFound "Event") $
      fromRightM throwDatabaseError $
        execQuery (Events.getEventById eventId)

  -- 2. Toggle featured status
  let newFeatured = not event.emFeaturedOnHomepage

  if newFeatured
    then do
      -- Clear existing featured event, then set this one
      _ <-
        fromMaybeM (throwHandlerFailure "Event update returned Nothing") $
          fromRightM throwDatabaseError $
            execTransaction $ do
              void $ HT.statement () Events.clearFeaturedEvents
              HT.statement () (Events.updateEvent eventId (toInsert event) {Events.eiFeaturedOnHomepage = True})
      Log.logInfo "Event promoted to frontpage" eventId
      pure
        FeatureEventViewData
          { fevUpdatedEvent = event {Events.emFeaturedOnHomepage = True},
            fevBannerType = Success,
            fevBannerTitle = "Promoted",
            fevBannerMessage = "Event is now featured on the homepage."
          }
    else do
      -- Just unfeature this event
      _ <-
        fromMaybeM (throwHandlerFailure "Event update returned Nothing") $
          fromRightM throwDatabaseError $
            execQuery $
              Events.updateEvent eventId (toInsert event) {Events.eiFeaturedOnHomepage = False}
      Log.logInfo "Event removed from frontpage" eventId
      pure
        FeatureEventViewData
          { fevUpdatedEvent = event {Events.emFeaturedOnHomepage = False},
            fevBannerType = Success,
            fevBannerTitle = "Demoted",
            fevBannerMessage = "Event is no longer featured on the homepage."
          }

-- | Servant handler: thin glue composing action + rendering row and banner.
handler ::
  Events.Id ->
  Slug ->
  Maybe Cookie ->
  AppM (Lucid.Html ())
handler eventId _slug cookie =
  handleBannerErrors "Event feature" $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to feature events." userMetadata
    vd <- action eventId
    pure $ do
      renderEventRow vd.fevUpdatedEvent
      renderBanner vd.fevBannerType vd.fevBannerTitle vd.fevBannerMessage

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

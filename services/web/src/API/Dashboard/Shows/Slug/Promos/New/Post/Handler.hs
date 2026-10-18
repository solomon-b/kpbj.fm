-- | @POST /dashboard/shows/:show_slug/promos/new@.
--
-- A host submits a promo for one of their shows. Every promo starts as
-- pending, also when a staff member submits it, so staff review each one on
-- the review page before it airs.
module API.Dashboard.Shows.Slug.Promos.New.Post.Handler
  ( handler,
    action,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Form (BreakItemForm (..))
import API.Dashboard.AirBreaks.BreakItems.Shared (ClaimedAudio (..), ParsedBreakItem (..), claimBreakItemAudio, parseBreakItemForm, redirectTo)
import API.Links (dashboardShowsLinks, rootLink)
import API.Types
import App.Handler.Combinators (requireAuth, requireShowHostOrStaff)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure, throwNotFound)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..))
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Servant qualified
import Utils (fromMaybeM, fromRightM)

--------------------------------------------------------------------------------

-- | Servant handler: check the session, run the action, and redirect back to
-- the promos page with a flash.
handler ::
  Slug ->
  Maybe Cookie ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler showSlug cookie form =
  handleRedirectErrors "Show promo submit" promosLink $ do
    (user, userMetadata) <- requireAuth cookie
    _ <- action user userMetadata showSlug form
    pure $
      redirectTo
        (rootLink promosLink)
        (FlashMessage Success "Promo Submitted" "Staff will review it before it airs.")
  where
    promosLink = dashboardShowsLinks.promosGet showSlug

-- | Business logic: check access to the show, parse the form, claim the
-- audio, and save a pending promo.
--
-- Returns the ID of the new promo.
action ::
  User.Model ->
  UserMetadata.Model ->
  Slug ->
  BreakItemForm ->
  ExceptT HandlerError AppM BreakItems.Id
action user userMetadata showSlug form = do
  requireShowHostOrStaff (User.mId user) showSlug userMetadata

  showModel <-
    fromMaybeM (throwNotFound "Show") $
      fromRightM throwDatabaseError $
        execQuery (Shows.getShowBySlug showSlug)

  parsed <- parseBreakItemForm BreakItems.ShowPromo form
  audio <- claimBreakItemAudio user BreakItems.ShowPromo form.bifAudioToken

  promoId <-
    fromMaybeM (throwHandlerFailure "Failed to create show promo record.") $
      fromRightM throwDatabaseError $
        execQuery
          ( BreakItems.insertBreakItem
              BreakItems.Insert
                { biiTitle = parsed.pbiTitle,
                  biiCategory = BreakItems.ShowPromo,
                  biiUnderwriterId = Nothing,
                  biiShowId = Just (Shows.id showModel),
                  biiReviewStatus = BreakItems.Pending,
                  biiAudioFilePath = audio.caStoragePath,
                  biiMimeType = audio.caMimeType,
                  biiFileSize = audio.caFileSize,
                  biiDurationSeconds = audio.caDurationSeconds,
                  biiStartsOn = parsed.pbiStartsOn,
                  biiEndsOn = parsed.pbiEndsOn,
                  biiCreatorId = User.mId user
                }
          )

  Log.logInfo "Show promo submitted" parsed.pbiTitle
  pure promoId

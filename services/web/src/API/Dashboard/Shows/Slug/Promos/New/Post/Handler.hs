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
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Handler.Combinators (authorizeShow)
import App.Handler.Error (HandlerError, handleRedirectErrors, throwDatabaseError, throwHandlerFailure)
import App.Monad (AppM)
import Component.Banner (BannerType (..))
import Component.Flash (FlashMessage (..))
import Control.Monad.Trans.Except (ExceptT)
import Data.Text (Text)
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

-- | Servant handler: check access to the show, run the action, and redirect
-- back to the promos page with a flash.
handler ::
  Slug ->
  Satisfies 'UserMetadata.Host RoleAuth ->
  BreakItemForm ->
  AppM (Servant.Headers '[Servant.Header "HX-Redirect" Text, Servant.Header "Set-Cookie" Text] Servant.NoContent)
handler showSlug host@(Satisfies _ auth) form =
  handleRedirectErrors "Show promo submit" promosLink $ do
    showModel <- authorizeShow host showSlug
    _ <- action auth.raUser showModel form
    pure $
      redirectTo
        (rootLink promosLink)
        (FlashMessage Success "Promo Submitted" "Staff will review it before it airs.")
  where
    promosLink = dashboardShowsLinks.promosGet showSlug

-- | Business logic: parse the form, claim the audio, and save a pending promo
-- for a show that 'authorizeShow' checked.
--
-- Returns the ID of the new promo.
action ::
  -- | The user who submits the promo
  User.Model ->
  Shows.Model ->
  BreakItemForm ->
  ExceptT HandlerError AppM BreakItems.Id
action user showModel form = do
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

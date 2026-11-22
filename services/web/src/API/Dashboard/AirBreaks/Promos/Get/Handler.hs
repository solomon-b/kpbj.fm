{-# LANGUAGE ViewPatterns #-}

-- | @GET /dashboard/air-breaks/promos@, the staff review page for show promos.
module API.Dashboard.AirBreaks.Promos.Get.Handler
  ( handler,
    action,
    PromoListViewData (..),
    fetchShowsById,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Promos.Get.Templates (renderItemsFragment, template)
import API.Links (apiLinks)
import API.Types
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Component.Flash (FlashMessage)
import Control.Monad (forM)
import Control.Monad.Reader (asks)
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Has qualified as Has
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Set qualified as Set
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.PageNumber (PageNumber (..))
import Domain.Types.StorageBackend (StorageBackend)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Everything the review page needs.
data PromoListViewData = PromoListViewData
  { plvPromos :: [BreakItems.Model],
    plvShowsById :: Map Shows.Id Shows.Model,
    plvPage :: PageNumber,
    plvHasMore :: Bool
  }

-- | Servant handler: run the action, and render.
handler :: Maybe PageNumber -> Satisfies 'UserMetadata.Staff RoleAuth -> Maybe FlashMessage -> Maybe HxRequest -> AppM (Lucid.Html ())
handler maybePage (Satisfies _ auth) mFlash (foldHxReq -> hxRequest) =
  handleHtmlErrors "Show promo review list" apiLinks.rootGet $ do
    vd <- action maybePage
    backend <- asks (Has.getter @StorageBackend)
    let PageNumber pageNum = vd.plvPage
    if hxRequest == IsHxRequest && pageNum > 1
      then pure $ renderItemsFragment backend vd.plvShowsById vd.plvPromos vd.plvPage vd.plvHasMore
      else
        lift $
          renderDashboardTemplate
            hxRequest
            mFlash
            auth.raMetadata
            NoShow
            NavPromos
            Nothing
            Nothing
            (template backend vd.plvShowsById vd.plvPromos vd.plvPage vd.plvHasMore)

-- | Business logic: load one page of promos and their shows.
--
-- Page 1 holds every pending promo, oldest first, and then the first page of
-- reviewed promos. Later pages hold only reviewed promos. A review takes a row
-- out of the pending block without a reload. If the pending block were paged,
-- OFFSET would skip the pending rows that moved up into the loaded page.
action :: Maybe PageNumber -> ExceptT HandlerError AppM PromoListViewData
action maybePage = do
  let page@(PageNumber pageNum) = fromMaybe (PageNumber 1) maybePage
      limit = 20 :: Limit
      offset = fromIntegral ((pageNum - 1) * fromIntegral limit) :: Offset

  pending <-
    if pageNum == 1
      then fromRightM throwDatabaseError $ execQuery BreakItems.getPendingPromos
      else pure []

  -- One row past the page, so the handler can tell whether another page
  -- exists without a second count query.
  fetched <-
    fromRightM throwDatabaseError $
      execQuery (BreakItems.getReviewedPromos (limit + 1) offset)
  let promos = pending <> take (fromIntegral limit) fetched
      hasMore = length fetched > fromIntegral limit

  showsById <- fetchShowsById promos

  pure
    PromoListViewData
      { plvPromos = promos,
        plvShowsById = showsById,
        plvPage = page,
        plvHasMore = hasMore
      }

-- | Load the show of each promo, by id.
--
-- A page holds the pending promos and at most 20 reviewed promos, so one query
-- for each distinct show is cheap. A deleted show is absent from the map.
fetchShowsById :: [BreakItems.Model] -> ExceptT HandlerError AppM (Map Shows.Id Shows.Model)
fetchShowsById promos = do
  let showIds = Set.toList (Set.fromList (mapMaybe (.bimShowId) promos))
  mShows <- forM showIds $ \showId ->
    fromRightM throwDatabaseError $ execQuery (Shows.getShowById showId)
  let liveShows = catMaybes mShows
  pure $ Map.fromList [(Shows.id showModel, showModel) | showModel <- liveShows]

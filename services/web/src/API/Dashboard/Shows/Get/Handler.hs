{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get.Handler (handler, action, ShowListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Shows.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks, rootLink)
import API.Types (DashboardShowsRoutes (..), Routes (..))
import App.Auth.Roles (RoleAuth (..), Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Search (Search (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the shows list page.
data ShowListViewData = ShowListViewData
  { slvUserMetadata :: UserMetadata.Model,
    slvShows :: [Shows.ShowWithHostInfo],
    slvPage :: Int64,
    slvHasMore :: Bool,
    slvQueryFilter :: Maybe Text,
    slvStatusFilter :: Maybe Shows.Status,
    slvDeletedView :: Bool,
    slvIsAdmin :: Bool,
    slvIsAppendRequest :: Bool
  }

-- | Business logic: pagination setup, fetch shows.
action ::
  UserMetadata.Model ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  Bool ->
  HxRequest ->
  ExceptT HandlerError AppM ShowListViewData
action userMetadata maybePage queryFilterParam statusFilterParam deletedView hxRequest = do
  -- 1. Set up pagination and filters
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      statusFilter = getFilter =<< statusFilterParam
      queryFilter = getFilter =<< queryFilterParam
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- 2. Check if user is admin (for delete button visibility)
  let isAdmin = UserMetadata.isAdmin userMetadata.mUserRole

  -- 3. Fetch shows for main content
  allShowsResult <- getShowResults limit offset queryFilter statusFilter deletedView

  -- 4. Compute pagination
  let theShows = take (fromIntegral limit) allShowsResult
      hasMore = length allShowsResult > fromIntegral limit

  pure
    ShowListViewData
      { slvUserMetadata = userMetadata,
        slvShows = theShows,
        slvPage = page,
        slvHasMore = hasMore,
        slvQueryFilter = queryFilter,
        slvStatusFilter = statusFilter,
        slvDeletedView = deletedView,
        slvIsAdmin = isAdmin,
        slvIsAppendRequest = isAppendRequest
      }

handler ::
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  Bool ->
  Satisfies 'UserMetadata.Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage queryFilterParam statusFilterParam deletedView (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Shows list" apiLinks.rootGet $ do
    vd <- action auth.raMetadata maybePage queryFilterParam statusFilterParam deletedView hxRequest
    if vd.slvIsAppendRequest
      then pure $ renderItemsFragment vd.slvIsAdmin vd.slvDeletedView vd.slvShows vd.slvPage vd.slvHasMore vd.slvQueryFilter vd.slvStatusFilter
      else do
        let showsTemplate = template vd.slvIsAdmin vd.slvDeletedView vd.slvShows vd.slvPage vd.slvHasMore vd.slvQueryFilter vd.slvStatusFilter
            filtersContent = Just $ filtersUI vd.slvQueryFilter vd.slvStatusFilter vd.slvDeletedView
        lift $ renderDashboardTemplate hxRequest vd.slvUserMetadata NoShow NavShows filtersContent newShowButton showsTemplate
  where
    newShowButton :: Maybe (Lucid.Html ())
    newShowButton =
      let newShowUrl = rootLink dashboardShowsLinks.newGet
       in Just $
            Lucid.a_
              [ Lucid.href_ newShowUrl,
                hxGet_ newShowUrl,
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                class_ $ base [Tokens.bgInverse, Tokens.fgInverse, Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.fontBold, "hover:opacity-80"]
              ]
              "New Show"

-- | Filter UI for top bar
filtersUI :: Maybe Text -> Maybe Shows.Status -> Bool -> Lucid.Html ()
filtersUI queryFilter statusFilter deletedView = do
  let dashboardShowsGetUrl = rootLink $ dashboardShowsLinks.list Nothing Nothing Nothing False
  Lucid.form_
    [ hxGet_ dashboardShowsGetUrl,
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      Lucid.class_ "flex items-center gap-3"
    ]
    $ do
      -- Search input
      Lucid.input_
        [ Lucid.type_ "search",
          Lucid.name_ "q",
          Lucid.value_ (fromMaybe "" queryFilter),
          Lucid.placeholder_ "Search...",
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "w-32"]
        ]
      -- Status filter
      Lucid.select_
        [ Lucid.name_ "status",
          class_ $ base ["px-2", "py-1", Tokens.textSm, "border", Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary]
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> selectedWhen (isNothing statusFilter)) "All Statuses"
          Lucid.option_ ([Lucid.value_ "active"] <> selectedWhen (statusFilter == Just Shows.Active)) "Active"
          Lucid.option_ ([Lucid.value_ "inactive"] <> selectedWhen (statusFilter == Just Shows.Inactive)) "Inactive"
      -- Deleted-only toggle
      Lucid.label_
        [class_ $ base ["flex", "items-center", "gap-1", Tokens.textSm, Tokens.fgPrimary]]
        $ do
          Lucid.input_
            ( [ Lucid.type_ "checkbox",
                Lucid.name_ "deleted",
                Lucid.value_ "true"
              ]
                <> checkedWhen deletedView
            )
          "Deleted only"
      -- Submit button
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold, "hover:opacity-80"]
        ]
        "Filter"
  where
    selectedWhen cond = [Lucid.selected_ "selected" | cond]
    checkedWhen cond = [Lucid.checked_ | cond]

getShowResults ::
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Shows.Status ->
  Bool ->
  ExceptT HandlerError AppM [Shows.ShowWithHostInfo]
getShowResults limit offset queryFilter statusFilter deletedView =
  fromRightM throwDatabaseError $
    if deletedView
      then execQuery (Shows.getDeletedShowsWithHostInfo (limit + 1) offset)
      else case (queryFilter, statusFilter) of
        (Just query, _) ->
          execQuery (Shows.searchShowsWithHostInfo (Search query) (limit + 1) offset)
        (Nothing, Just status) ->
          execQuery (Shows.getShowsByStatusWithHostInfo status (limit + 1) offset)
        (Nothing, Nothing) ->
          execQuery (Shows.getAllShowsWithHostInfo (limit + 1) offset)

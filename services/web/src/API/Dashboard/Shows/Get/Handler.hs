{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get.Handler (handler, action, ShowListViewData (..)) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Shows.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks, rootLink)
import API.Types (DashboardShowsRoutes (..), Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Search (Search (..))
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.HTMX
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | All data needed to render the shows list page.
data ShowListViewData = ShowListViewData
  { slvUserMetadata :: UserMetadata.Model,
    slvSidebarShows :: [Shows.Model],
    slvSelectedShow :: Maybe Shows.Model,
    slvShows :: [Shows.ShowWithHostInfo],
    slvPage :: Int64,
    slvHasMore :: Bool,
    slvQueryFilter :: Maybe Text,
    slvStatusFilter :: Maybe Shows.Status,
    slvIsAdmin :: Bool,
    slvIsAppendRequest :: Bool
  }

-- | Business logic: pagination setup, fetch shows.
action ::
  User.Model ->
  UserMetadata.Model ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  HxRequest ->
  ExceptT HandlerError AppM ShowListViewData
action user userMetadata maybePage queryFilterParam statusFilterParam hxRequest = do
  -- 1. Set up pagination and filters
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      statusFilter = getFilter =<< statusFilterParam
      queryFilter = getFilter =<< queryFilterParam
      isAppendRequest = hxRequest == IsHxRequest && page > 1

  -- 2. Check if user is admin (for delete button visibility)
  let isAdmin = UserMetadata.isAdmin userMetadata.mUserRole

  -- 3. Fetch shows for sidebar
  sidebarShows <- lift $ fetchSidebarShows isAdmin user

  let selectedShow = listToMaybe sidebarShows

  -- 4. Fetch shows for main content
  allShowsResult <- getShowResults limit offset queryFilter statusFilter

  -- 5. Compute pagination
  let theShows = take (fromIntegral limit) allShowsResult
      hasMore = length allShowsResult > fromIntegral limit

  pure
    ShowListViewData
      { slvUserMetadata = userMetadata,
        slvSidebarShows = sidebarShows,
        slvSelectedShow = selectedShow,
        slvShows = theShows,
        slvPage = page,
        slvHasMore = hasMore,
        slvQueryFilter = queryFilter,
        slvStatusFilter = statusFilter,
        slvIsAdmin = isAdmin,
        slvIsAppendRequest = isAppendRequest
      }

handler ::
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler maybePage queryFilterParam statusFilterParam cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Shows list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata
    vd <- action user userMetadata maybePage queryFilterParam statusFilterParam hxRequest
    if vd.slvIsAppendRequest
      then pure $ renderItemsFragment vd.slvIsAdmin vd.slvShows vd.slvPage vd.slvHasMore vd.slvQueryFilter vd.slvStatusFilter
      else do
        let showsTemplate = template vd.slvIsAdmin vd.slvShows vd.slvPage vd.slvHasMore vd.slvQueryFilter vd.slvStatusFilter
            filtersContent = Just $ filtersUI vd.slvQueryFilter vd.slvStatusFilter
        lift $ renderDashboardTemplate hxRequest vd.slvUserMetadata vd.slvSidebarShows vd.slvSelectedShow NavShows filtersContent newShowButton showsTemplate
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

-- | Fetch sidebar shows based on admin status
fetchSidebarShows ::
  Bool ->
  User.Model ->
  AppM [Shows.Model]
fetchSidebarShows isAdmin user =
  if isAdmin
    then fromRight [] <$> execQuery Shows.getAllActiveShows
    else fromRight [] <$> execQuery (Shows.getShowsForUser (User.mId user))

-- | Filter UI for top bar
filtersUI :: Maybe Text -> Maybe Shows.Status -> Lucid.Html ()
filtersUI queryFilter statusFilter = do
  let dashboardShowsGetUrl = rootLink $ dashboardShowsLinks.list Nothing Nothing Nothing
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
      -- Submit button
      Lucid.button_
        [ Lucid.type_ "submit",
          class_ $ base [Tokens.px4, Tokens.py2, Tokens.textSm, Tokens.bgInverse, Tokens.fgInverse, Tokens.fontBold, "hover:opacity-80"]
        ]
        "Filter"
  where
    selectedWhen cond = [Lucid.selected_ "selected" | cond]

getShowResults ::
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Shows.Status ->
  ExceptT HandlerError AppM [Shows.ShowWithHostInfo]
getShowResults limit offset queryFilter statusFilter =
  fromRightM throwDatabaseError $
    case (queryFilter, statusFilter) of
      (Just query, _) ->
        execQuery (Shows.searchShowsWithHostInfo (Search query) (limit + 1) offset)
      (Nothing, Just status) ->
        execQuery (Shows.getShowsByStatusWithHostInfo status (limit + 1) offset)
      (Nothing, Nothing) ->
        execQuery (Shows.getAllShowsWithHostInfo (limit + 1) offset)

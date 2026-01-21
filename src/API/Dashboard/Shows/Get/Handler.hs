{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get.Handler (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Templates.ItemsFragment (renderItemsFragment)
import API.Dashboard.Shows.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks, rootLink)
import API.Types (DashboardShowsRoutes (..), Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
import App.Handler.Error (handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, isNothing, listToMaybe)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Search (Search (..))
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)

--------------------------------------------------------------------------------

handler ::
  Tracer ->
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler _tracer maybePage queryFilterParam statusFilterParam cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Shows list" apiLinks.rootGet $ do
    -- 1. Require authentication and staff role
    (user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to access this page." userMetadata

    -- 2. Set up pagination and filters
    let page = fromMaybe 1 maybePage
        limit = 20 :: Limit
        offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
        statusFilter = getFilter =<< statusFilterParam
        queryFilter = getFilter =<< queryFilterParam
        isAppendRequest = hxRequest == IsHxRequest && page > 1

    -- 3. Fetch shows for sidebar
    sidebarShowsResult <-
      if UserMetadata.isAdmin userMetadata.mUserRole
        then execQuerySpan Shows.getAllActiveShows
        else execQuerySpan (Shows.getShowsForUser (User.mId user))
    let sidebarShows = fromRight [] sidebarShowsResult
        selectedShow = listToMaybe sidebarShows

    -- 4. Fetch shows for main content
    allShowsResult <- getShowResults limit offset queryFilter statusFilter

    -- 5. Render response
    let theShows = take (fromIntegral limit) allShowsResult
        hasMore = length allShowsResult > fromIntegral limit

    if isAppendRequest
      then pure $ renderItemsFragment theShows page hasMore queryFilter statusFilter
      else do
        let showsTemplate = template theShows page hasMore queryFilter statusFilter
            filtersContent = Just $ filtersUI queryFilter statusFilter
        renderDashboardTemplate hxRequest userMetadata sidebarShows selectedShow NavShows filtersContent newShowButton showsTemplate
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
                Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
              ]
              "New Show"

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
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 dark:border-gray-600 w-32"
        ]
      -- Status filter
      Lucid.select_
        [ Lucid.name_ "status",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 dark:border-gray-600"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> selectedWhen (isNothing statusFilter)) "All Statuses"
          Lucid.option_ ([Lucid.value_ "active"] <> selectedWhen (statusFilter == Just Shows.Active)) "Active"
          Lucid.option_ ([Lucid.value_ "inactive"] <> selectedWhen (statusFilter == Just Shows.Inactive)) "Inactive"
      -- Submit button
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "px-4 py-2 text-sm bg-gray-800 text-white font-bold hover:bg-gray-700"
        ]
        "Filter"
  where
    selectedWhen cond = [Lucid.selected_ "selected" | cond]

getShowResults ::
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Shows.Status ->
  AppM [Shows.ShowWithHostInfo]
getShowResults limit offset queryFilter statusFilter = do
  showsResult <-
    case (queryFilter, statusFilter) of
      (Just query, _) ->
        execQuerySpan (Shows.searchShowsWithHostInfo (Search query) (limit + 1) offset)
      (Nothing, Just status) ->
        execQuerySpan (Shows.getShowsByStatusWithHostInfo status (limit + 1) offset)
      (Nothing, Nothing) ->
        execQuerySpan (Shows.getAllShowsWithHostInfo (limit + 1) offset)
  case showsResult of
    Left err -> throwDatabaseError err
    Right s -> pure s

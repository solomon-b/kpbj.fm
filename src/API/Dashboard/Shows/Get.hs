{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Shows.Get (handler) where

--------------------------------------------------------------------------------

import API.Dashboard.Shows.Get.Templates.Page (template)
import API.Links (apiLinks, dashboardShowsLinks, userLinks)
import API.Types (DashboardShowsRoutes (..), Routes (..), UserRoutes (..))
import App.Common (getUserInfo, renderDashboardTemplate)
import Component.Banner (BannerType (..))
import Component.DashboardFrame (DashboardNav (..))
import Component.Redirect (BannerParams (..), redirectWithBanner)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Either (fromRight)
import Data.Has (Has)
import Data.Int (Int64)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Filter (Filter (..))
import Domain.Types.HxRequest (HxRequest (..), foldHxReq)
import Domain.Types.Limit (Limit)
import Domain.Types.Offset (Offset)
import Domain.Types.Search (Search (..))
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata (isSuspended)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import OpenTelemetry.Trace (Tracer)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

rootGetUrl :: Links.URI
rootGetUrl = Links.linkURI apiLinks.rootGet

userLoginGetUrl :: Links.URI
userLoginGetUrl = Links.linkURI $ userLinks.loginGet Nothing Nothing

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
  Maybe Int64 ->
  Maybe (Filter Text) ->
  Maybe (Filter Shows.Status) ->
  Maybe Cookie ->
  Maybe HxRequest ->
  m (Lucid.Html ())
handler _tracer maybePage queryFilterParam statusFilterParam cookie (foldHxReq -> hxRequest) = do
  let page = fromMaybe 1 maybePage
      limit = 20 :: Limit
      offset = fromIntegral $ (page - 1) * fromIntegral limit :: Offset
      statusFilter = getFilter =<< statusFilterParam
      queryFilter = getFilter =<< queryFilterParam

  getUserInfo cookie >>= \case
    Nothing -> do
      let banner = BannerParams Error "Login Required" "You must be logged in to access this page."
      pure $ redirectWithBanner [i|/#{userLoginGetUrl}|] banner
    Just (_user, userMetadata)
      | not (UserMetadata.isStaffOrHigher userMetadata.mUserRole) || isSuspended userMetadata -> do
          let banner = BannerParams Error "Staff Access Required" "You do not have permission to access this page."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
    Just (user, userMetadata) -> do
      -- Fetch shows for sidebar (admins see all, staff see their assigned shows)
      sidebarShowsResult <-
        if UserMetadata.isAdmin userMetadata.mUserRole
          then execQuerySpan Shows.getAllActiveShows
          else execQuerySpan (Shows.getShowsForUser (User.mId user))
      let sidebarShows = fromRight [] sidebarShowsResult
          selectedShow = listToMaybe sidebarShows

      getShowsResults limit offset queryFilter statusFilter >>= \case
        Left _err -> do
          Log.logInfo "Failed to fetch shows from database" ()
          let banner = BannerParams Error "Error" "Failed to load shows. Please try again."
          pure $ redirectWithBanner [i|/#{rootGetUrl}|] banner
        Right allShows -> do
          let theShows = take (fromIntegral limit) allShows
              hasMore = length allShows > fromIntegral limit
              showsTemplate = template theShows page hasMore queryFilter statusFilter
              filtersContent = Just $ filtersUI queryFilter statusFilter
          renderDashboardTemplate hxRequest userMetadata sidebarShows selectedShow NavShows filtersContent newShowButton showsTemplate
  where
    newShowButton :: Maybe (Lucid.Html ())
    newShowButton =
      let newShowUrl = Links.linkURI dashboardShowsLinks.newGet
       in Just $
            Lucid.a_
              [ Lucid.href_ [i|/#{newShowUrl}|],
                hxGet_ [i|/#{newShowUrl}|],
                hxTarget_ "#main-content",
                hxPushUrl_ "true",
                Lucid.class_ "bg-gray-800 text-white px-4 py-2 text-sm font-bold hover:bg-gray-700"
              ]
              "New Show"

-- | Filter UI for top bar
filtersUI :: Maybe Text -> Maybe Shows.Status -> Lucid.Html ()
filtersUI queryFilter statusFilter = do
  let dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing
  Lucid.form_
    [ hxGet_ [i|/#{dashboardShowsGetUrl}|],
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
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300 w-32"
        ]
      -- Status filter
      Lucid.select_
        [ Lucid.name_ "status",
          Lucid.class_ "px-2 py-1 text-sm border border-gray-300"
        ]
        $ do
          Lucid.option_ ([Lucid.value_ ""] <> selectedWhen (statusFilter == Nothing)) "All Statuses"
          Lucid.option_ ([Lucid.value_ "active"] <> selectedWhen (statusFilter == Just Shows.Active)) "Active"
          Lucid.option_ ([Lucid.value_ "inactive"] <> selectedWhen (statusFilter == Just Shows.Inactive)) "Inactive"
      -- Submit button
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "px-3 py-1 text-sm bg-gray-800 text-white font-bold hover:bg-gray-700"
        ]
        "Filter"
  where
    selectedWhen cond = if cond then [Lucid.selected_ "selected"] else []

getShowsResults ::
  ( MonadUnliftIO m,
    MonadDB m,
    Log.MonadLog m,
    MonadReader env m,
    Has Tracer env
  ) =>
  Limit ->
  Offset ->
  Maybe Text ->
  Maybe Shows.Status ->
  m (Either HSQL.Pool.UsageError [Shows.ShowWithHostInfo])
getShowsResults limit offset maybeQuery maybeStatus =
  case (maybeQuery, maybeStatus) of
    (Just query, _) ->
      execQuerySpan (Shows.searchShowsWithHostInfo (Search query) (limit + 1) offset)
    (Nothing, Just status) ->
      execQuerySpan (Shows.getShowsByStatusWithHostInfo status (limit + 1) offset)
    (Nothing, Nothing) ->
      execQuerySpan (Shows.getAllShowsWithHostInfo (limit + 1) offset)

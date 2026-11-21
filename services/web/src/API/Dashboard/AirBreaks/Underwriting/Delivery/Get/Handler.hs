{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.AirBreaks.Underwriting.Delivery.Get.Handler
  ( handler,
    action,
    parseMonth,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.Underwriting.Delivery.Get.Templates.Page (DeliveryRow (..), template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Auth.Roles (RoleAuth, Satisfies (..))
import App.Dashboard.ShowContext (ShowContext (..))
import App.Dashboard.Template (renderDashboardTemplate)
import App.Handler.Error (HandlerError, handleHtmlErrors, throwDatabaseError)
import App.Monad (AppM)
import Component.DashboardFrame (DashboardNav (..))
import Control.Monad.Trans (lift)
import Control.Monad.Trans.Except (ExceptT)
import Data.Containers.ListUtils (nubOrd)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (Day, addDays, fromGregorian, gregorianMonthLength, toGregorian)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Timezone (pacificDay, startOfPacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Effects.Database.Tables.UserMetadata (UserRole (..))
import Lucid qualified
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/air-breaks/underwriting/delivery.
handler ::
  Maybe Text ->
  Satisfies 'Staff RoleAuth ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mMonth (Satisfies _ auth) (foldHxReq -> hxRequest) =
  handleHtmlErrors "Underwriting delivery" apiLinks.rootGet $ do
    today <- pacificDay <$> lift currentSystemTime
    let (y, m, _) = toGregorian today
        firstDay = fromMaybe (fromGregorian y m 1) (mMonth >>= parseMonth)
    rows <- action firstDay
    lift $
      renderDashboardTemplate
        hxRequest
        auth.raMetadata
        NoShow
        NavUnderwritingDelivery
        Nothing
        Nothing
        (template firstDay rows)

-- | Parse @YYYY-MM@ into the first day of that month.
parseMonth :: Text -> Maybe Day
parseMonth txt = case Text.splitOn "-" (Text.strip txt) of
  [yearText, monthText] -> do
    year <- readMaybe (Text.unpack yearText)
    month <- readMaybe (Text.unpack monthText)
    if month >= 1 && month <= 12 then Just (fromGregorian year month 1) else Nothing
  _ -> Nothing

-- | One row per underwriter that was active or aired in the month that starts
-- on the given day.
--
-- Days active counts the days of the month on which at least one of the
-- underwriter's announcements may air. Aired counts the playback rows for the
-- underwriter's break items. Airings of an item deleted since still count.
action ::
  -- | First day of the month
  Day ->
  ExceptT HandlerError AppM [DeliveryRow]
action firstDay = do
  let (y, m, _) = toGregorian firstDay
      lastDay = fromGregorian y m (gregorianMonthLength y m)
  underwriters <- fromRightM throwDatabaseError $ execQuery Underwriters.getAll
  items <- fromRightM throwDatabaseError $ execQuery (BreakItems.getActiveInMonth firstDay lastDay)
  airings <-
    fromRightM throwDatabaseError $
      execQuery
        ( PlaybackHistory.airingsBetween
            (startOfPacificDay firstDay)
            (startOfPacificDay (addDays 1 lastDay))
        )
  airedItems <-
    fromRightM throwDatabaseError $
      execQuery (BreakItems.getByIdsIncludingDeleted (map BreakItems.Id (nubOrd (map fst airings))))
  let airedById = Map.fromList [(BreakItems.unId item.bimId, item) | item <- airedItems]
      activeOn d item = item.bimStartsOn <= d && maybe True (>= d) item.bimEndsOn
      daysActive u =
        length
          [ ()
          | d <- [firstDay .. lastDay],
            any (activeOn d) [item | item <- items, item.bimUnderwriterId == Just u.uwId]
          ]
      rowFor u =
        DeliveryRow
          { drUnderwriter = u,
            drDaysActive = daysActive u,
            drAirings =
              [ (item.bimTitle, startedAt)
              | (itemId, startedAt) <- airings,
                Just item <- [Map.lookup itemId airedById],
                item.bimUnderwriterId == Just u.uwId
              ]
          }
  pure [row | row <- map rowFor underwriters, row.drDaysActive > 0 || not (null row.drAirings)]

{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Underwriting.Delivery.Get.Handler
  ( handler,
    action,
    parseMonth,
  )
where

--------------------------------------------------------------------------------

import API.Dashboard.Underwriting.Delivery.Get.Templates.Page (DeliveryRow (..), template)
import API.Links (apiLinks)
import API.Types (Routes (..))
import App.Common (renderDashboardTemplate)
import App.Handler.Combinators (requireAuth, requireStaffNotSuspended)
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
import Domain.BreakPlanner (owedInMonth)
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest, foldHxReq)
import Domain.Types.Timezone (pacificDay, startOfPacificDay)
import Effects.Clock (currentSystemTime)
import Effects.Database.Execute (execQuery)
import Effects.Database.Tables.BreakItems qualified as BreakItems
import Effects.Database.Tables.PlaybackHistory qualified as PlaybackHistory
import Effects.Database.Tables.Underwriters qualified as Underwriters
import Lucid qualified
import Text.Read (readMaybe)
import Utils (fromRightM)

--------------------------------------------------------------------------------

-- | Handler for GET /dashboard/underwriting/delivery.
handler ::
  Maybe Text ->
  Maybe Cookie ->
  Maybe HxRequest ->
  AppM (Lucid.Html ())
handler mMonth cookie (foldHxReq -> hxRequest) =
  handleHtmlErrors "Underwriting delivery" apiLinks.rootGet $ do
    (_user, userMetadata) <- requireAuth cookie
    requireStaffNotSuspended "You do not have permission to manage underwriting." userMetadata
    today <- pacificDay <$> lift currentSystemTime
    let (y, m, _) = toGregorian today
        firstDay = fromMaybe (fromGregorian y m 1) (mMonth >>= parseMonth)
    rows <- action today firstDay
    lift $
      renderDashboardTemplate
        hxRequest
        userMetadata
        []
        Nothing
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

-- | One row per underwriter with spots owed or aired in the month that starts
-- on the given day.
--
-- Spots per month sums the monthly counts of the underwriter's orders that run
-- in the month. Owed sums what those orders owe in the month, prorated for an
-- order that runs part of it. Due is the same sum for the days up to the end
-- of yesterday. Aired counts the playback rows for the underwriter's break
-- items. Airings of an item deleted since still count.
action ::
  -- | Today in Pacific
  Day ->
  -- | First day of the month
  Day ->
  ExceptT HandlerError AppM [DeliveryRow]
action today firstDay = do
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
      yesterday = addDays (-1) today
      orders u = [(item, n) | item <- items, item.bimUnderwriterId == Just u.uwId, Just n <- [item.bimSpotsPerMonth]]
      owedBy u endsOn = sum [owedInMonth firstDay item.bimStartsOn (endsOn item) n | (item, n) <- orders u]
      rowFor u =
        DeliveryRow
          { drUnderwriter = u,
            drSpotsPerMonth = sum (map snd (orders u)),
            drOwed = owedBy u (.bimEndsOn),
            drDue = owedBy u (\item -> Just (maybe yesterday (min yesterday) item.bimEndsOn)),
            drAirings =
              [ (item.bimTitle, startedAt)
              | (itemId, startedAt) <- airings,
                Just item <- [Map.lookup itemId airedById],
                item.bimUnderwriterId == Just u.uwId
              ]
          }
  pure [row | row <- map rowFor underwriters, row.drOwed > 0 || not (null row.drAirings)]

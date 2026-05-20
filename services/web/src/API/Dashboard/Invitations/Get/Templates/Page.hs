{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Invitations.Get.Templates.Page
  ( template,
    renderInvitationRow,
    renderScheduleSummary,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardInvitationsLinks, inviteLinks, rootLink)
import API.Types (DashboardInvitationsRoutes (..), InviteRoutes (..))
import Component.Table
  ( ColumnAlign (..),
    ColumnHeader (..),
    IndexTableConfig (..),
    renderIndexTable,
  )
import Data.Aeson (Value (..))
import Data.Aeson.KeyMap qualified as KeyMap
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Data.Vector qualified as Vector
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Timezone (utcToPacific)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Lucid.Alpine
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | Template for the invitations list page.
template ::
  -- | Base URL for building invite links
  Text ->
  -- | List of invitations with creator info
  [HostInvitation.ModelWithCreator] ->
  Lucid.Html ()
template appBaseUrl invitations =
  Lucid.section_ [class_ $ base [Tokens.bgMain, "rounded", "overflow-hidden", Tokens.mb8]] $
    if null invitations
      then renderEmptyState
      else
        renderIndexTable
          IndexTableConfig
            { itcBodyId = "invitations-table-body",
              itcHeaders =
                [ ColumnHeader "Schedule" AlignLeft,
                  ColumnHeader "Recipient" AlignLeft,
                  ColumnHeader "Status" AlignLeft,
                  ColumnHeader "Created By" AlignLeft,
                  ColumnHeader "Expires" AlignLeft,
                  ColumnHeader "" AlignCenter
                ],
              itcNextPageUrl = Nothing,
              itcPaginationConfig = Nothing
            }
          (mapM_ (renderInvitationRow appBaseUrl) invitations)

renderInvitationRow :: Text -> HostInvitation.ModelWithCreator -> Lucid.Html ()
renderInvitationRow appBaseUrl invitation =
  Lucid.tr_
    [ Lucid.id_ rowId,
      class_ $ base ["border-b-2", Tokens.borderMuted]
    ]
    $ do
      -- Schedule column
      Lucid.td_ [class_ $ base [Tokens.p4]] $
        Lucid.span_ [Lucid.class_ Tokens.textSm] $
          Lucid.toHtml (renderScheduleSummary invitation.mwcScheduleData)

      -- Recipient column
      Lucid.td_ [class_ $ base [Tokens.p4]] $
        Lucid.span_ [Lucid.class_ Tokens.textSm] $
          Lucid.toHtml (display invitation.mwcRecipientEmail)

      -- Status column
      Lucid.td_ [class_ $ base [Tokens.p4]] $
        renderStatusBadge invitation.mwcStatus

      -- Created By column
      Lucid.td_ [class_ $ base [Tokens.p4]] $
        Lucid.span_ [Lucid.class_ Tokens.textSm] $
          Lucid.toHtml invitation.mwcCreatedByName

      -- Expires column
      Lucid.td_ [class_ $ base [Tokens.p4]] $
        Lucid.span_ [Lucid.class_ Tokens.textSm] $
          Lucid.toHtml (formatDateTime invitation.mwcExpiresAt)

      -- Actions column
      Lucid.td_ [class_ $ base [Tokens.p4, "text-center"]] $
        renderActions appBaseUrl invitation
  where
    invitationIdText = display invitation.mwcId

    rowId :: Text
    rowId = "invitation-row-" <> invitationIdText

renderStatusBadge :: HostInvitation.Status -> Lucid.Html ()
renderStatusBadge status =
  Lucid.span_
    [class_ $ base ["inline-block", Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
    $ Lucid.toHtml statusText
  where
    (bgClass, textClass, statusText) = case status of
      HostInvitation.Pending -> (Tokens.warningBg, Tokens.warningText, "Pending") :: (Text, Text, Text)
      HostInvitation.Claimed -> (Tokens.successBg, Tokens.successText, "Claimed")
      HostInvitation.Expired -> (Tokens.bgAlt, Tokens.fgMuted, "Expired")
      HostInvitation.Revoked -> (Tokens.errorBg, Tokens.errorText, "Revoked")

renderActions :: Text -> HostInvitation.ModelWithCreator -> Lucid.Html ()
renderActions appBaseUrl invitation =
  case invitation.mwcStatus of
    HostInvitation.Pending -> do
      Lucid.div_ [class_ $ base ["flex", "gap-2", "justify-center", "flex-wrap"]] $ do
        -- Copy Link button
        Lucid.button_
          [ xData_ "{ copied: false }",
            xOnClick_ [i|navigator.clipboard.writeText('#{inviteUrl}').then(() => { copied = true; setTimeout(() => copied = false, 2000) })|],
            class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
          ]
          $ do
            Lucid.span_ [xShow_ "!copied"] "Copy Link"
            Lucid.span_ [xShow_ "copied"] "Copied!"
        -- Edit button
        Lucid.button_
          [ hxGet_ editGetUrl,
            hxTarget_ rowSelector,
            hxSwap_ "outerHTML",
            class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
          ]
          "Edit"
        -- Resend button
        Lucid.button_
          [ hxPost_ resendUrl,
            hxSwap_ "none",
            class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
          ]
          "Resend"
        -- Revoke button
        Lucid.button_
          [ hxDelete_ deleteUrl,
            hxTarget_ rowSelector,
            hxSwap_ "delete",
            hxConfirm_ "Are you sure you want to revoke this invitation?",
            class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.errorBg, Tokens.errorText, "border", Tokens.borderMuted, "hover:opacity-80"]
          ]
          "Revoke"
    HostInvitation.Claimed ->
      Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, "italic"]] $
        Lucid.toHtml claimedText
    HostInvitation.Expired ->
      Lucid.button_
        [ hxPost_ regenerateUrl,
          class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
        ]
        "Regenerate"
    HostInvitation.Revoked ->
      Lucid.button_
        [ hxPost_ regenerateUrl,
          class_ $ base [Tokens.px3, "py-1", Tokens.textSm, Tokens.fontBold, Tokens.bgAlt, Tokens.fgPrimary, "border", Tokens.borderMuted, "hover:opacity-80"]
        ]
        "Regenerate"
  where
    inviteUrl :: Text
    inviteUrl =
      let onboardLink = rootLink $ inviteLinks.onboardGet invitation.mwcToken
       in appBaseUrl <> onboardLink

    deleteUrl :: Text
    deleteUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.delete invitation.mwcId
       in [i|/#{link}|]

    regenerateUrl :: Text
    regenerateUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.regenerate invitation.mwcId
       in [i|/#{link}|]

    editGetUrl :: Text
    editGetUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.editGet invitation.mwcId
       in [i|/#{link}|]

    resendUrl :: Text
    resendUrl =
      let link = Links.linkURI $ dashboardInvitationsLinks.resendPost invitation.mwcId
       in [i|/#{link}|]

    rowSelector :: Text
    rowSelector = "#invitation-row-" <> display invitation.mwcId

    claimedText :: Text
    claimedText = case invitation.mwcClaimedByName of
      Just name -> [i|Claimed by #{name}|]
      Nothing -> "Claimed"

renderEmptyState :: Lucid.Html ()
renderEmptyState =
  Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted, "p-12", "text-center"]] $ do
    Lucid.p_ [class_ $ base [Tokens.textXl, Tokens.fgMuted]] "No invitations found."
    Lucid.p_ [class_ $ base [Tokens.fgMuted, "mt-2"]] "Create a new invitation to onboard a host."

-- | Format a UTC timestamp as a human-readable date string.
formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y" . utcToPacific

-- | Render a human-readable summary of schedule data JSON.
--
-- Parses the JSON array of schedule slots. Each slot has:
--   - @dayOfWeek@ (string like "thursday")
--   - @weeksOfMonth@ (array of ints, e.g. [1,2,3,4,5] for weekly)
--   - @startTime@ (string like "19:00")
--   - @duration@ (int, minutes)
--
-- Renders as e.g. "Weekly · Thu 7:00 PM (2hr)" or "1st & 3rd · Sat 2:00 PM (2hr)".
renderScheduleSummary :: Value -> Text
renderScheduleSummary (Array slots) =
  case Vector.toList slots of
    [] -> "No schedule"
    slotList -> Text.intercalate "; " (map renderSlot slotList)
renderScheduleSummary _ = "Invalid schedule"

renderSlot :: Value -> Text
renderSlot (Object obj) =
  let frequency = renderFrequency weeksOfMonth
      day = renderDayAbbrev dayOfWeek
      time = renderTime startTime
      dur = renderDuration duration
      primary = [i|#{frequency} · #{day} #{time} (#{dur})|] :: Text
      replayPart = case replayTime of
        Just rt -> [i|, replay #{renderTime rt}|] :: Text
        Nothing -> ""
   in primary <> replayPart
  where
    dayOfWeek :: Text
    dayOfWeek = case KeyMap.lookup "dayOfWeek" obj of
      Just (String d) -> d
      _ -> "unknown"

    weeksOfMonth :: [Int]
    weeksOfMonth = case KeyMap.lookup "weeksOfMonth" obj of
      Just (Array arr) ->
        [ truncate n | Number n <- Vector.toList arr
        ]
      _ -> []

    startTime :: Text
    startTime = case KeyMap.lookup "startTime" obj of
      Just (String t) -> t
      _ -> "00:00"

    duration :: Int
    duration = case KeyMap.lookup "duration" obj of
      Just (Number n) -> truncate n
      _ -> 0

    replayTime :: Maybe Text
    replayTime = case KeyMap.lookup "replayTime" obj of
      Just (String t) | not (Text.null t) -> Just t
      _ -> Nothing
renderSlot _ = "Invalid slot"

renderFrequency :: [Int] -> Text
renderFrequency weeks
  | weeks == [1, 2, 3, 4, 5] || weeks == [1, 2, 3, 4] = "Weekly"
  | otherwise = Text.intercalate " & " (map ordinal weeks)
  where
    ordinal :: Int -> Text
    ordinal 1 = "1st"
    ordinal 2 = "2nd"
    ordinal 3 = "3rd"
    ordinal n = [i|#{n}th|]

renderDayAbbrev :: Text -> Text
renderDayAbbrev day = case Text.toLower day of
  "monday" -> "Mon"
  "tuesday" -> "Tue"
  "wednesday" -> "Wed"
  "thursday" -> "Thu"
  "friday" -> "Fri"
  "saturday" -> "Sat"
  "sunday" -> "Sun"
  other -> other

renderTime :: Text -> Text
renderTime timeStr =
  case Text.splitOn ":" timeStr of
    [hourStr, minStr] ->
      case (readMaybe (Text.unpack hourStr), readMaybe (Text.unpack minStr)) of
        (Just hour, Just minute) ->
          let (displayHour, amPm) = to12Hour (hour :: Int)
              minPart = if (minute :: Int) == 0 then "" :: Text else [i|:#{padMinute minute}|]
           in [i|#{displayHour}#{minPart} #{amPm}|]
        _ -> timeStr
    _ -> timeStr
  where
    readMaybe :: (Read a) => String -> Maybe a
    readMaybe s = case reads s of
      [(x, "")] -> Just x
      _ -> Nothing

    to12Hour :: Int -> (Int, Text)
    to12Hour 0 = (12, "AM")
    to12Hour 12 = (12, "PM")
    to12Hour h
      | h < 12 = (h, "AM")
      | otherwise = (h - 12, "PM")

    padMinute :: Int -> Text
    padMinute m
      | m < 10 = [i|0#{m}|]
      | otherwise = [i|#{m}|]

renderDuration :: Int -> Text
renderDuration minutes
  | minutes >= 60 && minutes `mod` 60 == 0 = [i|#{minutes `div` 60}hr|]
  | minutes >= 60 =
      let hours = minutes `div` 60
          mins = minutes `mod` 60
       in [i|#{hours}hr #{mins}min|]
  | otherwise = [i|#{minutes}min|]

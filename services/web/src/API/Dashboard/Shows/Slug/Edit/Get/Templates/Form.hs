{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Edit.Get.Templates.Form
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Component.ScheduleEditor (ScheduleEditorData (..), renderScheduleEditor)
import Data.Int (Int64)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (TimeOfDay (..))
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Lucid.Form.Builder
import Lucid.HTMX
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ apiLinks.dashboard.admin.shows.list Nothing Nothing Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ apiLinks.shows.detail slug Nothing

--------------------------------------------------------------------------------

-- | Show edit template using V2 FormBuilder
template :: StorageBackend -> Shows.Model -> UserMetadata.Model -> Bool -> Text -> [UserMetadata.UserWithMetadata] -> Set User.Id -> Text -> Text -> [ShowSchedule.ScheduleTemplate Result] -> [ShowSchedule.ScheduleTemplate Result] -> Lucid.Html ()
template backend showModel userMeta isStaff schedulesJson eligibleHosts currentHostIds existingTags startDate currentScheduleTemplates pendingScheduleTemplates = do
  renderFormHeader userMeta showModel
  renderForm config form
  where
    showSlug = showModel.slug
    postUrl = [i|/dashboard/shows/#{display showSlug}/edit|]
    logoUrl = maybe "" (buildMediaUrl backend) showModel.logoUrl

    config :: FormConfig
    config =
      defaultFormConfig
        { fcAction = postUrl,
          fcMethod = "post",
          fcHtmxTarget = Just "#main-content",
          fcHtmxSwap = Just "innerHTML"
        }

    form :: FormBuilder
    form = do
      -- Basic Information Section
      section "BASIC INFORMATION" $ do
        textField "title" $ do
          label "Show Title"
          placeholder "e.g. Industrial Depths"
          value showModel.title
          required
          minLength 3
          maxLength 200

        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe your show. What kind of music do you play? What's your show's vibe?"
          maybe (pure ()) value showModel.description
          maxLength 5000

        textField "tags" $ do
          label "Tags"
          placeholder "e.g. Techno, Ambient, Experimental, Hip-Hop"
          hint "Comma-separated tags for categorization and filtering"
          unless (Text.null existingTags) $ value existingTags
          maxLength 500

      -- Schedule & Settings Section (staff only)
      when isStaff $ do
        section "SCHEDULE & SETTINGS" $ do
          selectField "status" $ do
            label "Show Status"
            hint "Active shows appear on the shows page"
            required
            if showModel.status == Shows.Active
              then do
                addOptionSelected "active" "Active"
                addOption "inactive" "Inactive"
              else do
                addOption "active" "Active"
                addOptionSelected "inactive" "Inactive"

      -- Hidden status for non-staff
      unless isStaff $ do
        hidden "status" $ case showModel.status of
          Shows.Active -> "active"
          Shows.Inactive -> "inactive"

      -- Artwork & Branding Section
      section "ARTWORK & BRANDING" $ do
        imageField "logo_file" $ do
          label "Logo Image"
          maxSize 10
          aspectRatio (4, 3)
          currentFile logoUrl

      -- Hosts Section (staff only)
      when isStaff $ do
        section "HOSTS" $ do
          plain $ renderHostsMultiSelect eligibleHosts currentHostIds

      -- Schedule Section (staff only)
      when isStaff $ do
        section "SCHEDULE" $ do
          plain $ do
            Lucid.p_
              [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]]
              "Set the recurring schedule for this show."
            unless (null currentScheduleTemplates && null pendingScheduleTemplates) $
              renderSchedulePreview currentScheduleTemplates pendingScheduleTemplates startDate
            renderScheduleEditor (ScheduleEditorData schedulesJson startDate)

      cancelButton [i|/#{dashboardShowsGetUrl}|] "CANCEL"
      submitButton "UPDATE SHOW"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Shows.Model -> Lucid.Html ()
renderFormHeader userMeta showModel = do
  let showBackUrl = showGetUrl showModel.slug
  Lucid.section_ [class_ $ base [Tokens.bgMain, Tokens.fgPrimary, Tokens.p6, Tokens.mb8, Tokens.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] "EDIT SHOW"
        Lucid.div_ [class_ $ base [Tokens.fgMuted, Tokens.textSm]] $ do
          Lucid.strong_ "Show: "
          Lucid.toHtml showModel.title
          " • "
          Lucid.strong_ "Editor: "
          Lucid.toHtml userMeta.mDisplayName
      Lucid.div_ [Lucid.class_ "space-x-4"] $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{showBackUrl}|],
            hxGet_ [i|/#{showBackUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "VIEW SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
            hxGet_ [i|/#{dashboardShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base ["text-blue-300", "hover:text-blue-100", Tokens.textSm, "underline"]
          ]
          "ALL SHOWS"

--------------------------------------------------------------------------------
-- Schedule Preview Banner

-- | Read-only preview of the active and pending schedules, shown when a pending
-- future schedule exists. Gives staff a clear picture of what's airing now and
-- what's coming up while they edit the upcoming schedule.
renderSchedulePreview :: [ShowSchedule.ScheduleTemplate Result] -> [ShowSchedule.ScheduleTemplate Result] -> Text -> Lucid.Html ()
renderSchedulePreview activeTemplates pendingTemplates pendingStartDate =
  Lucid.div_ [class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgAlt, Tokens.p4, Tokens.mb4]] $ do
    -- Active schedule
    unless (null activeTemplates) $ do
      Lucid.p_ [class_ $ base [Tokens.fontBold, Tokens.textSm, Tokens.mb2]] "CURRENT SCHEDULE"
      mapM_ renderSlot activeTemplates

    -- Pending schedule with start date
    unless (null pendingTemplates) $ do
      when (not (null activeTemplates)) $
        Lucid.hr_ [class_ $ base [Tokens.borderMuted, "my-3"]]
      let header =
            if Text.null pendingStartDate
              then "UPCOMING SCHEDULE"
              else "UPCOMING SCHEDULE (starts " <> pendingStartDate <> ")"
      Lucid.p_ [class_ $ base [Tokens.fontBold, Tokens.textSm, Tokens.mb2]] (Lucid.toHtml header)
      mapM_ renderSlot pendingTemplates
  where
    renderSlot :: ShowSchedule.ScheduleTemplate Result -> Lucid.Html ()
    renderSlot t =
      Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb2]] $ do
        let dayName = maybe "—" (Text.pack . show) t.stDayOfWeek
            timeStr = formatTimeRange t.stStartTime t.stEndTime
            weeksStr = formatWeeks t.stWeeksOfMonth
        Lucid.toHtml $ dayName <> " " <> timeStr <> weeksStr

    formatTimeRange :: TimeOfDay -> TimeOfDay -> Text
    formatTimeRange start end =
      let fmt tod =
            let h = todHour tod
                m = todMin tod
                period = if h < 12 then "AM" else "PM" :: Text
                h12 = if h `mod` 12 == 0 then 12 else h `mod` 12
             in if m == 0
                  then Text.pack (show h12) <> period
                  else Text.pack (show h12) <> ":" <> Text.pack (padZero m) <> period
       in fmt start <> "–" <> fmt end

    padZero :: Int -> String
    padZero n = if n < 10 then "0" <> show n else show n

    formatWeeks :: Maybe [Int64] -> Text
    formatWeeks Nothing = ""
    formatWeeks (Just ws) = case ws of
      [1, 2, 3, 4, 5] -> ""
      [1, 3] -> " (1st & 3rd weeks)"
      [2, 4] -> " (2nd & 4th weeks)"
      [n] -> " (" <> ordinal n <> " week)"
      _ -> " (weeks " <> Text.intercalate ", " (map (Text.pack . show) ws) <> ")"

    ordinal :: Int64 -> Text
    ordinal 1 = "1st"
    ordinal 2 = "2nd"
    ordinal 3 = "3rd"
    ordinal 4 = "4th"
    ordinal n = Text.pack (show n) <> "th"

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

-- | Render a searchable multi-select for host assignment.
renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Set User.Id -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts currentHostIds = do
  let (currentHosts, otherHosts) = partitionHosts eligibleHosts currentHostIds
      sortedHosts = currentHosts <> otherHosts
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [class_ $ base [Tokens.textXs, Tokens.fgMuted, Tokens.mb2]] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [class_ $ base [Tokens.mb2]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          class_ $ base [Tokens.fullWidth, Tokens.p3, Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono"],
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [class_ $ base [Tokens.bgAlt, Tokens.border2, Tokens.borderMuted]] $ do
      -- Header
      Lucid.div_ [class_ $ base [Tokens.bgInverse, Tokens.fgInverse, "border-b", Tokens.borderMuted, Tokens.p3, Tokens.fontBold, Tokens.textSm]] $
        Lucid.toHtml ("AVAILABLE HOSTS (" <> show (length eligibleHosts) <> ")")

      -- Scrollable host list
      Lucid.div_ [Lucid.class_ "max-h-64 overflow-y-auto"] $
        mapM_ (renderHostOption currentHostIds) sortedHosts

-- | Partition hosts into current hosts and other hosts
partitionHosts :: [UserMetadata.UserWithMetadata] -> Set User.Id -> ([UserMetadata.UserWithMetadata], [UserMetadata.UserWithMetadata])
partitionHosts hosts currentIds =
  let isCurrent h = Set.member h.uwmUserId currentIds
   in (filter isCurrent hosts, filter (not . isCurrent) hosts)

-- | Render a single host option in the multi-select
renderHostOption :: Set User.Id -> UserMetadata.UserWithMetadata -> Lucid.Html ()
renderHostOption currentHostIds user =
  let userId = user.uwmUserId
      displayName = display user.uwmDisplayName
      email = display user.uwmEmail
      roleText = display user.uwmUserRole
      userIdText = display userId
      isCurrentHost = Set.member userId currentHostIds
      filterCondition =
        [i|search === '' || '#{displayName}'.toLowerCase().includes(search.toLowerCase()) || '#{email}'.toLowerCase().includes(search.toLowerCase())|]
   in Lucid.div_
        [ class_ $ base ["border-b", Tokens.borderMuted, Tokens.p3, Tokens.hoverBg, "cursor-pointer"],
          xShow_ filterCondition,
          xBindClass_ [i|{ '#{Tokens.infoBg}': $refs.host_#{userIdText}?.checked }|]
        ]
        $ do
          Lucid.div_ [class_ $ base ["flex", "items-center"]] $ do
            Lucid.input_ $
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
                <> [Lucid.checked_ | isCurrentHost]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], class_ $ base ["flex-1", "cursor-pointer"]] $ do
              Lucid.div_ [class_ $ base [Tokens.fontBold]] $ Lucid.toHtml displayName
              Lucid.div_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] $
                Lucid.toHtml $
                  email <> " • " <> roleText <> if isCurrentHost then " • CURRENT HOST" else ""

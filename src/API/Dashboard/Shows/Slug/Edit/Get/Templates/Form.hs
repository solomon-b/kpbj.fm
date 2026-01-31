{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.Slug.Edit.Get.Templates.Form
  ( template,
    schedulesToJson,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Text.Encoding qualified as Text
import Data.Time (DayOfWeek (..))
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
template :: StorageBackend -> Shows.Model -> UserMetadata.Model -> Bool -> Text -> [UserMetadata.UserWithMetadata] -> Set User.Id -> Text -> Lucid.Html ()
template backend showModel userMeta isStaff schedulesJson eligibleHosts currentHostIds existingTags = do
  renderFormHeader userMeta showModel
  renderForm config form
  when isStaff $ renderScheduleManagementScript schedulesJson
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
          plain renderScheduleSection

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
        Lucid.div_ [class_ $ base ["text-gray-300 dark:text-gray-500", Tokens.textSm]] $ do
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
-- Searchable Multi-Select for Hosts

-- | Render a searchable multi-select for host assignment.
renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Set User.Id -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts currentHostIds = do
  let (currentHosts, otherHosts) = partitionHosts eligibleHosts currentHostIds
      sortedHosts = currentHosts <> otherHosts
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 dark:text-gray-400 mb-2"] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [Lucid.class_ "mb-2"] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          Lucid.class_ "w-full p-3 border-2 border-gray-400 dark:border-gray-500 font-mono",
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [Lucid.class_ "bg-gray-100 dark:bg-gray-700 border-2 border-gray-300 dark:border-gray-600"] $ do
      -- Header
      Lucid.div_ [Lucid.class_ "bg-gray-200 dark:bg-gray-600 border-b border-gray-400 dark:border-gray-500 p-3 font-bold text-sm"] $
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
        [ Lucid.class_ "border-b border-gray-300 dark:border-gray-600 p-3 hover:bg-gray-200 dark:hover:bg-gray-600 cursor-pointer",
          xShow_ filterCondition,
          xBindClass_ "{ 'bg-blue-50': $refs.host_#{userIdText}?.checked }"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center"] $ do
            Lucid.input_ $
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
                <> [Lucid.checked_ | isCurrentHost]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], Lucid.class_ "flex-1 cursor-pointer"] $ do
              Lucid.div_ [Lucid.class_ "font-bold"] $ Lucid.toHtml displayName
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600 dark:text-gray-400"] $
                Lucid.toHtml $
                  email <> " • " <> roleText <> if isCurrentHost then " • CURRENT HOST" else ""

--------------------------------------------------------------------------------
-- Schedule Section (staff/admin only)

renderScheduleSection :: Lucid.Html ()
renderScheduleSection = do
  Lucid.p_
    [Lucid.class_ "text-sm text-gray-600 dark:text-gray-400 mb-4"]
    "Manage recurring time slots when this show will air. Changes will take effect immediately."

  Lucid.div_ [Lucid.id_ "schedule-container"] $ do
    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 dark:border-gray-500 p-8 text-center text-gray-600 dark:text-gray-400", Lucid.id_ "schedule-add-btn-container"] $ do
      Lucid.button_
        [ Lucid.type_ "button",
          Lucid.id_ "add-schedule-btn",
          Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
        ]
        "+ ADD TIME SLOT"
      Lucid.div_ [Lucid.class_ "mt-2 text-sm"] "Click to add a recurring schedule"

  -- Hidden field for JSON data
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedules_json",
      Lucid.id_ "schedules-json",
      Lucid.value_ "[]"
    ]

--------------------------------------------------------------------------------
-- Schedule Management JavaScript

-- | Convert DayOfWeek to lowercase text for JavaScript
dayOfWeekToText :: DayOfWeek -> Text
dayOfWeekToText Sunday = "sunday"
dayOfWeekToText Monday = "monday"
dayOfWeekToText Tuesday = "tuesday"
dayOfWeekToText Wednesday = "wednesday"
dayOfWeekToText Thursday = "thursday"
dayOfWeekToText Friday = "friday"
dayOfWeekToText Saturday = "saturday"

-- | Convert schedule templates from database to JSON for the form
schedulesToJson :: [ShowSchedule.ScheduleTemplate Result] -> Text
schedulesToJson schedules =
  let scheduleData =
        [ Aeson.object
            [ "dayOfWeek" Aeson..= maybe ("" :: Text) dayOfWeekToText sched.stDayOfWeek,
              "weeksOfMonth" Aeson..= maybe ([] :: [Int]) (map fromIntegral) sched.stWeeksOfMonth,
              "startTime" Aeson..= Text.take 5 (Text.pack $ show sched.stStartTime),
              "endTime" Aeson..= Text.take 5 (Text.pack $ show sched.stEndTime)
            ]
          | sched <- schedules
        ]
   in Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode scheduleData

renderScheduleManagementScript :: Text -> Lucid.Html ()
renderScheduleManagementScript schedulesJson =
  Lucid.script_
    [i|
// Schedule management module (IIFE to avoid global pollution)
(function() {
  // Existing schedules from database
  const existingSchedules = #{schedulesJson};

  // Schedule slot HTML template
  const createScheduleElement = (data = null) => {
    const div = document.createElement('div');
    div.className = 'border-2 border-gray-300 dark:border-gray-600 p-4 bg-gray-50 mb-4 schedule-slot';
    div.innerHTML = `
      <div class='flex justify-between items-center mb-4'>
        <span class='font-bold text-sm'>TIME SLOT</span>
        <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700'
                data-action='remove-schedule'>REMOVE</button>
      </div>

      <div class='grid grid-cols-1 md:grid-cols-2 gap-4 mb-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Day of Week *</label>
          <select class='w-full p-2 border-2 border-gray-400 dark:border-gray-500 font-mono schedule-day' required>
            <option value=''>-- Select Day --</option>
            <option value='sunday'>Sunday</option>
            <option value='monday'>Monday</option>
            <option value='tuesday'>Tuesday</option>
            <option value='wednesday'>Wednesday</option>
            <option value='thursday'>Thursday</option>
            <option value='friday'>Friday</option>
            <option value='saturday'>Saturday</option>
          </select>
        </div>

        <div>
          <label class='block font-bold text-sm mb-1'>Weeks of Month *</label>
          <div class='flex gap-3 flex-wrap'>
            <label class='flex items-center'><input type='checkbox' class='mr-1 schedule-week' value='1' checked> 1st</label>
            <label class='flex items-center'><input type='checkbox' class='mr-1 schedule-week' value='2' checked> 2nd</label>
            <label class='flex items-center'><input type='checkbox' class='mr-1 schedule-week' value='3' checked> 3rd</label>
            <label class='flex items-center'><input type='checkbox' class='mr-1 schedule-week' value='4' checked> 4th</label>
            <label class='flex items-center'><input type='checkbox' class='mr-1 schedule-week' value='5' checked> 5th</label>
          </div>
          <p class='text-xs text-gray-500 dark:text-gray-400 mt-1'>Select which weeks of each month (all = every week)</p>
        </div>
      </div>

      <div class='grid grid-cols-2 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Start Time *</label>
          <input type='time' class='w-full p-2 border-2 border-gray-400 dark:border-gray-500 font-mono schedule-start' required>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>End Time *</label>
          <input type='time' class='w-full p-2 border-2 border-gray-400 dark:border-gray-500 font-mono schedule-end' required>
        </div>
      </div>
    `;

    // Pre-populate with data if provided
    if (data) {
      const daySelect = div.querySelector('.schedule-day');
      if (daySelect && data.dayOfWeek) {
        daySelect.value = data.dayOfWeek;
      }

      // Set weeks of month
      const weekCheckboxes = div.querySelectorAll('.schedule-week');
      weekCheckboxes.forEach(cb => {
        const weekNum = parseInt(cb.value, 10);
        if (data.weeksOfMonth && data.weeksOfMonth.length > 0) {
          cb.checked = data.weeksOfMonth.includes(weekNum);
        }
      });

      const startInput = div.querySelector('.schedule-start');
      if (startInput && data.startTime) {
        startInput.value = data.startTime;
      }

      const endInput = div.querySelector('.schedule-end');
      if (endInput && data.endTime) {
        endInput.value = data.endTime;
      }
    }

    return div;
  };

  // Extract schedule data from DOM element
  const extractScheduleData = (div) => {
    const weekCheckboxes = div.querySelectorAll('.schedule-week:checked');
    const weeksOfMonth = Array.from(weekCheckboxes).map(cb => parseInt(cb.value, 10));

    return {
      dayOfWeek: div.querySelector('.schedule-day')?.value || '',
      weeksOfMonth: weeksOfMonth,
      startTime: div.querySelector('.schedule-start')?.value || '',
      endTime: div.querySelector('.schedule-end')?.value || ''
    };
  };

  // Update hidden JSON field with current schedules
  const updateSchedulesJson = () => {
    const scheduleDivs = document.querySelectorAll('\#schedule-container .schedule-slot');
    const schedules = Array.from(scheduleDivs)
      .map(extractScheduleData)
      .filter(s => s.dayOfWeek && s.weeksOfMonth.length > 0 && s.startTime && s.endTime);

    const jsonField = document.getElementById('schedules-json');
    if (jsonField) {
      jsonField.value = JSON.stringify(schedules);
    }
  };

  // Add new schedule slot
  const addSchedule = (data = null) => {
    const container = document.getElementById('schedule-container');
    const addButton = document.getElementById('schedule-add-btn-container');
    if (container && addButton) {
      container.insertBefore(createScheduleElement(data), addButton);
      updateSchedulesJson();
    }
  };

  // Remove schedule slot
  const removeSchedule = (button) => {
    button.closest('.schedule-slot')?.remove();
    updateSchedulesJson();
  };

  // Initialize with existing schedules
  existingSchedules.forEach(schedule => addSchedule(schedule));

  // Event listeners
  document.getElementById('add-schedule-btn')?.addEventListener('click', () => addSchedule());

  const container = document.getElementById('schedule-container');
  if (container) {
    container.addEventListener('input', updateSchedulesJson);
    container.addEventListener('change', updateSchedulesJson);
    container.addEventListener('click', (e) => {
      if (e.target.dataset.action === 'remove-schedule') {
        removeSchedule(e.target);
      }
    });
  }

  // Initialize JSON field with existing data
  updateSchedulesJson();
})();
|]

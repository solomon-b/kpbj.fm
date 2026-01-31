{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as T
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine
import Lucid.Form.Builder
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ apiLinks.dashboard.admin.shows.list Nothing Nothing Nothing

dashboardShowsNewPostUrl :: Links.URI
dashboardShowsNewPostUrl = Links.linkURI apiLinks.dashboard.admin.shows.newPost

--------------------------------------------------------------------------------

-- | New show form template using V2 FormBuilder
template :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
template eligibleHosts = do
  renderFormHeader
  renderForm config form
  renderScheduleManagementScript
  where
    postUrl :: Text
    postUrl = [i|/#{dashboardShowsNewPostUrl}|]

    backUrl :: Text
    backUrl = [i|/#{dashboardShowsGetUrl}|]

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
          required
          minLength 3
          maxLength 200

        textareaField "description" 6 $ do
          label "Description"
          placeholder "Describe your show. What kind of music do you play? What's your show's vibe?"
          maxLength 5000

        textField "tags" $ do
          label "Tags"
          placeholder "e.g. Techno, Ambient, Experimental, Hip-Hop"
          hint "Comma-separated tags for categorization and filtering"
          maxLength 500

      -- Schedule & Settings Section
      section "SCHEDULE & SETTINGS" $ do
        selectField "status" $ do
          label "Show Status"
          hint "Active shows appear on the shows page"
          required
          addOptionSelected "active" "Active"
          addOption "inactive" "Inactive"

      -- Artwork & Branding Section
      section "ARTWORK & BRANDING" $ do
        imageField "logo_file" $ do
          label "Logo Image"
          maxSize 10
          aspectRatio (4, 3)

      -- Hosts Section
      section "HOSTS" $ do
        plain $ renderHostsMultiSelect eligibleHosts

      -- Schedule Section
      section "SCHEDULE" $ do
        plain renderScheduleSection

      cancelButton backUrl "CANCEL"
      submitButton "CREATE SHOW"

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Lucid.Html ()
renderFormHeader =
  Lucid.section_ [class_ $ base [T.bgInverse, T.fgInverse, T.p6, T.mb8, T.fullWidth]] $ do
    Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between"]] $ do
      Lucid.div_ $ do
        Lucid.h1_ [class_ $ base [T.text2xl, T.fontBold, T.mb2]] "CREATE NEW SHOW"
        Lucid.div_
          [class_ $ base [T.fgMuted, T.textSm]]
          "Add a new show to the station"
      Lucid.div_ $ do
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
            hxGet_ [i|/#{dashboardShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            class_ $ base [T.infoText, "hover:opacity-80", T.textSm, "underline"]
          ]
          "← BACK TO SHOWS"

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts = do
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [class_ $ base [T.textXs, T.fgMuted, T.mb2]] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [class_ $ base [T.mb2]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          class_ $ base [T.fullWidth, T.p3, T.border2, T.borderMuted, T.bgMain, T.fgPrimary, "font-mono"],
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [class_ $ base [T.bgAlt, T.border2, T.borderMuted]] $ do
      -- Header
      Lucid.div_ [class_ $ base [T.bgInverse, T.fgInverse, "border-b", T.borderMuted, T.p3, T.fontBold, T.textSm]] $
        Lucid.toHtml ("AVAILABLE HOSTS (" <> show (length eligibleHosts) <> ")")

      -- Scrollable host list
      Lucid.div_ [Lucid.class_ "max-h-64 overflow-y-auto"] $
        mapM_ renderHostOption eligibleHosts

renderHostOption :: UserMetadata.UserWithMetadata -> Lucid.Html ()
renderHostOption user =
  let userId = user.uwmUserId
      displayName = display user.uwmDisplayName
      email = display user.uwmEmail
      roleText = display user.uwmUserRole
      userIdText = display userId
      -- Alpine.js filter condition - check if search matches name or email
      filterCondition =
        [i|search === '' || '#{displayName}'.toLowerCase().includes(search.toLowerCase()) || '#{email}'.toLowerCase().includes(search.toLowerCase())|]
   in Lucid.div_
        [ class_ $ base ["border-b", T.borderMuted, T.p3, T.hoverBg, "cursor-pointer"],
          xShow_ filterCondition,
          xBindClass_ [i|{ '#{T.infoBg}': $refs.host_#{userIdText}?.checked }|]
        ]
        $ do
          Lucid.div_ [class_ $ base ["flex", "items-center"]] $ do
            Lucid.input_
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], class_ $ base ["flex-1", "cursor-pointer"]] $ do
              Lucid.div_ [class_ $ base [T.fontBold]] $ Lucid.toHtml displayName
              Lucid.div_ [class_ $ base [T.textSm, T.fgMuted]] $
                Lucid.toHtml (email <> " • " <> roleText)

--------------------------------------------------------------------------------
-- Schedule Section (dynamic add/remove time slots)

renderScheduleSection :: Lucid.Html ()
renderScheduleSection = do
  Lucid.p_
    [class_ $ base [T.textSm, T.fgMuted, T.mb4]]
    "Add recurring time slots when this show will air. Leave empty if no regular schedule."

  Lucid.div_ [Lucid.id_ "schedule-container"] $ do
    -- Schedule entries will be inserted here by JavaScript
    Lucid.div_ [class_ $ base [T.border2, "border-dashed", T.borderMuted, T.p8, "text-center", T.fgMuted], Lucid.id_ "schedule-add-btn-container"] $ do
      Lucid.button_
        [ Lucid.type_ "button",
          Lucid.id_ "add-schedule-btn",
          class_ $ base [T.successBg, T.successText, T.border2, T.successBorder, T.px6, "py-3", T.fontBold, "hover:opacity-80"]
        ]
        "+ ADD TIME SLOT"
      Lucid.div_ [class_ $ base ["mt-2", T.textSm]] "Click to add a recurring schedule"

  -- Hidden field for JSON data
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedules_json",
      Lucid.id_ "schedules-json",
      Lucid.value_ "[]"
    ]

--------------------------------------------------------------------------------
-- Schedule Management JavaScript

renderScheduleManagementScript :: Lucid.Html ()
renderScheduleManagementScript =
  Lucid.script_
    [i|
// Schedule management module (IIFE to avoid global pollution)
(function() {
  // Schedule slot HTML template
  const createScheduleElement = () => {
    const div = document.createElement('div');
    div.className = 'border-2 border-[var(--theme-border-muted)] p-4 bg-[var(--theme-bg-alt)] mb-4 schedule-slot';
    div.innerHTML = `
      <div class='flex justify-between items-center mb-4'>
        <span class='font-bold text-sm'>TIME SLOT</span>
        <button type='button' class='bg-[var(--theme-error)] text-[var(--theme-bg)] px-3 py-1 text-xs font-bold hover:opacity-80'
                data-action='remove-schedule'>REMOVE</button>
      </div>

      <div class='grid grid-cols-1 md:grid-cols-2 gap-4 mb-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Day of Week *</label>
          <select class='w-full p-2 border-2 border-[var(--theme-border-muted)] bg-[var(--theme-bg)] text-[var(--theme-fg)] font-mono schedule-day' required>
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
          <p class='text-xs text-[var(--theme-fg-muted)] mt-1'>Select which weeks of each month (all = every week)</p>
        </div>
      </div>

      <div class='grid grid-cols-2 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Start Time *</label>
          <input type='time' class='w-full p-2 border-2 border-[var(--theme-border-muted)] bg-[var(--theme-bg)] text-[var(--theme-fg)] font-mono schedule-start' required>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>End Time *</label>
          <input type='time' class='w-full p-2 border-2 border-[var(--theme-border-muted)] bg-[var(--theme-bg)] text-[var(--theme-fg)] font-mono schedule-end' required>
        </div>
      </div>
    `;
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
  const addSchedule = () => {
    const container = document.getElementById('schedule-container');
    const addButton = document.getElementById('schedule-add-btn-container');
    if (container && addButton) {
      container.insertBefore(createScheduleElement(), addButton);
      updateSchedulesJson();
    }
  };

  // Remove schedule slot
  const removeSchedule = (button) => {
    button.closest('.schedule-slot')?.remove();
    updateSchedulesJson();
  };

  // Initialize
  document.getElementById('add-schedule-btn')?.addEventListener('click', addSchedule);

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
})();
|]

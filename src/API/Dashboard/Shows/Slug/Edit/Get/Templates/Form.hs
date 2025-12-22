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
import Component.Form.Builder
import Component.ImageFilePicker qualified as ImageFilePicker
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
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xRef_, xShow_)
import Rel8 (Result)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ apiLinks.dashboard.admin.shows.list Nothing Nothing Nothing

showGetUrl :: Slug -> Links.URI
showGetUrl slug = Links.linkURI $ apiLinks.shows.detail slug Nothing

mediaGetUrl :: Links.URI
mediaGetUrl = Links.linkURI apiLinks.mediaGet

--------------------------------------------------------------------------------

-- | Show edit template using FormBuilder
--
-- The schedulesJson parameter should be a JSON array of schedule slot objects.
-- Use 'schedulesToJson' to convert from database models.
--
-- The eligibleHosts parameter is all users who can be assigned as hosts.
-- The currentHostIds parameter is the set of user IDs currently hosting this show.
-- The existingTags parameter is a comma-separated string of current tags.
template :: Shows.Model -> UserMetadata.Model -> Bool -> Text -> [UserMetadata.UserWithMetadata] -> Set User.Id -> Text -> Lucid.Html ()
template showModel userMeta isStaff schedulesJson eligibleHosts currentHostIds existingTags = do
  let showSlug = showModel.slug
      additionalContent =
        if isStaff
          then [renderSubmitActions showSlug, renderScheduleManagementScript schedulesJson]
          else [renderSubmitActions showSlug]
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/dashboard/shows/#{display showSlug}/edit|],
        fbMethod = "post",
        fbHeader = Just (renderFormHeader userMeta showModel),
        fbFields = showEditFormFields showModel isStaff eligibleHosts currentHostIds existingTags,
        fbAdditionalContent = additionalContent,
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: UserMetadata.Model -> Shows.Model -> Lucid.Html ()
renderFormHeader userMeta showModel = do
  let showBackUrl = showGetUrl showModel.slug
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "EDIT SHOW"
        Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] $ do
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
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "VIEW SHOW"
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
            hxGet_ [i|/#{dashboardShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "ALL SHOWS"

--------------------------------------------------------------------------------
-- Form Fields Definition

showEditFormFields :: Shows.Model -> Bool -> [UserMetadata.UserWithMetadata] -> Set User.Id -> Text -> [FormField]
showEditFormFields showModel isStaff eligibleHosts currentHostIds existingTags =
  [ -- Basic Information Section
    SectionField
      { sfTitle = "BASIC INFORMATION",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Show Title",
                vfInitialValue = Just showModel.title,
                vfPlaceholder = Just "e.g. Industrial Depths",
                vfHint = Nothing,
                vfValidation =
                  ValidationRules
                    { vrMinLength = Just 3,
                      vrMaxLength = Just 200,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextareaField
              { vtName = "description",
                vtLabel = "Description",
                vtInitialValue = Just showModel.description,
                vtRows = 6,
                vtPlaceholder = Just "Describe your show. What kind of music do you play? What's your show's vibe?",
                vtHint = Nothing,
                vtValidation =
                  ValidationRules
                    { vrMinLength = Just 10,
                      vrMaxLength = Just 5000,
                      vrPattern = Nothing,
                      vrRequired = True,
                      vrCustomValidation = Nothing
                    }
              },
            ValidatedTextField
              { vfName = "tags",
                vfLabel = "Tags",
                vfInitialValue = if Text.null existingTags then Nothing else Just existingTags,
                vfPlaceholder = Just "e.g. Techno, Ambient, Experimental, Hip-Hop",
                vfHint = Just "Comma-separated tags for categorization and filtering",
                vfValidation =
                  ValidationRules
                    { vrMinLength = Nothing,
                      vrMaxLength = Just 500,
                      vrPattern = Nothing,
                      vrRequired = False,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      },
    -- Schedule & Settings Section (conditional on isStaff)
    ConditionalField
      { cfCondition = isStaff,
        cfTrueFields =
          [ SectionField
              { sfTitle = "SCHEDULE & SETTINGS",
                sfFields =
                  [ ValidatedSelectField
                      { vsName = "status",
                        vsLabel = "Show Status",
                        vsOptions =
                          [ SelectOption "active" "Active" (showModel.status == Shows.Active) Nothing,
                            SelectOption "inactive" "Inactive" (showModel.status == Shows.Inactive) Nothing
                          ],
                        vsHint = Just "Active shows appear on the shows page",
                        vsValidation = emptyValidation {vrRequired = True}
                      }
                  ]
              }
          ],
        cfFalseFields =
          [ -- Hidden fields to preserve existing values for non-staff
            HiddenField
              { hfName = "status",
                hfValue = case showModel.status of
                  Shows.Active -> "active"
                  Shows.Inactive -> "inactive"
              }
          ]
      },
    -- Artwork & Branding Section
    SectionField
      { sfTitle = "ARTWORK & BRANDING",
        sfFields =
          [ PlainField
              { pfHtml = logoImageField showModel
              },
            PlainField
              { pfHtml = bannerImageField showModel
              }
          ]
      },
    -- Hosts Section (staff/admin only)
    ConditionalField
      { cfCondition = isStaff,
        cfTrueFields =
          [ SectionField
              { sfTitle = "HOSTS",
                sfFields =
                  [ PlainField
                      { pfHtml = renderHostsMultiSelect eligibleHosts currentHostIds
                      }
                  ]
              }
          ],
        cfFalseFields = []
      },
    -- Schedule Section (staff/admin only)
    ConditionalField
      { cfCondition = isStaff,
        cfTrueFields =
          [ SectionField
              { sfTitle = "SCHEDULE",
                sfFields =
                  [ PlainField
                      { pfHtml = renderScheduleSection
                      }
                  ]
              }
          ],
        cfFalseFields = []
      }
  ]

--------------------------------------------------------------------------------
-- Image Fields

-- | Render logo image field with integrated preview.
logoImageField :: Shows.Model -> Lucid.Html ()
logoImageField showModel =
  let imageUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) showModel.logoUrl
   in ImageFilePicker.render
        ImageFilePicker.Config
          { ImageFilePicker.fieldName = "logo_file",
            ImageFilePicker.label = "Logo Image",
            ImageFilePicker.existingImageUrl = imageUrl,
            ImageFilePicker.accept = "image/jpeg,image/png,image/webp,image/gif",
            ImageFilePicker.maxSizeMB = 10,
            ImageFilePicker.isRequired = False
          }

-- | Render banner image field with integrated preview.
bannerImageField :: Shows.Model -> Lucid.Html ()
bannerImageField showModel =
  let imageUrl = maybe "" (\path -> [i|/#{mediaGetUrl}/#{path}|]) showModel.bannerUrl
   in ImageFilePicker.render
        ImageFilePicker.Config
          { ImageFilePicker.fieldName = "banner_file",
            ImageFilePicker.label = "Banner Image",
            ImageFilePicker.existingImageUrl = imageUrl,
            ImageFilePicker.accept = "image/jpeg,image/png,image/webp,image/gif",
            ImageFilePicker.maxSizeMB = 10,
            ImageFilePicker.isRequired = False
          }

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Slug -> Lucid.Html ()
renderSubmitActions showSlug = do
  let showBackUrl = showGetUrl showSlug
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "UPDATE SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{showBackUrl}|],
          hxGet_ [i|/#{showBackUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline inline-block"
        ]
        "CANCEL"

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

-- | Render a searchable multi-select for host assignment.
--
-- Current hosts are displayed at the top of the list with their checkboxes pre-checked.
-- Other eligible hosts are shown below.
renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Set User.Id -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts currentHostIds = do
  -- Partition into current hosts and other hosts
  let (currentHosts, otherHosts) = partitionHosts eligibleHosts currentHostIds
      sortedHosts = currentHosts <> otherHosts
  Lucid.div_ [xData_ "{ search: '' }"] $ do
    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Assign Hosts"
    Lucid.p_ [Lucid.class_ "text-xs text-gray-600 mb-2"] "Select one or more hosts for this show. Regular users will be automatically promoted to Host role."

    -- Search input
    Lucid.div_ [Lucid.class_ "mb-2"] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "Search by name or email...",
          Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
          xModel_ "search"
        ]

    -- Results container
    Lucid.div_ [Lucid.class_ "bg-gray-100 border-2 border-gray-300"] $ do
      -- Header
      Lucid.div_ [Lucid.class_ "bg-gray-200 border-b border-gray-400 p-3 font-bold text-sm"] $
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
      -- Alpine.js filter condition - check if search matches name or email
      filterCondition =
        [i|search === '' || '#{displayName}'.toLowerCase().includes(search.toLowerCase()) || '#{email}'.toLowerCase().includes(search.toLowerCase())|]
   in Lucid.div_
        [ Lucid.class_ "border-b border-gray-300 p-3 hover:bg-gray-200 cursor-pointer",
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
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $
                Lucid.toHtml $
                  email <> " • " <> roleText <> if isCurrentHost then " • CURRENT HOST" else ""

--------------------------------------------------------------------------------
-- Schedule Section (staff/admin only)

renderScheduleSection :: Lucid.Html ()
renderScheduleSection = do
  Lucid.p_
    [Lucid.class_ "text-sm text-gray-600 mb-4"]
    "Manage recurring time slots when this show will air. Changes will take effect immediately."

  Lucid.div_ [Lucid.id_ "schedule-container"] $ do
    -- Schedule entries will be inserted here by JavaScript
    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-8 text-center text-gray-600", Lucid.id_ "schedule-add-btn-container"] $ do
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
    div.className = 'border-2 border-gray-300 p-4 bg-gray-50 mb-4 schedule-slot';
    div.innerHTML = `
      <div class='flex justify-between items-center mb-4'>
        <span class='font-bold text-sm'>TIME SLOT</span>
        <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700'
                data-action='remove-schedule'>REMOVE</button>
      </div>

      <div class='grid grid-cols-1 md:grid-cols-2 gap-4 mb-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Day of Week *</label>
          <select class='w-full p-2 border-2 border-gray-400 font-mono schedule-day' required>
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
          <p class='text-xs text-gray-500 mt-1'>Select which weeks of each month (all = every week)</p>
        </div>
      </div>

      <div class='grid grid-cols-2 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Start Time *</label>
          <input type='time' class='w-full p-2 border-2 border-gray-400 font-mono schedule-start' required>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>End Time *</label>
          <input type='time' class='w-full p-2 border-2 border-gray-400 font-mono schedule-end' required>
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
        // If weeksOfMonth is empty or contains all weeks, check all
        // Otherwise, only check the specified weeks
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

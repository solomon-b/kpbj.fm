{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Shows.New.Get.Templates.Page
  ( template,
  )
where

--------------------------------------------------------------------------------

import API.Links (dashboardShowsLinks)
import API.Types (DashboardShowsRoutes (..))
import Component.Form.Builder
import Component.ImageFilePicker qualified as ImageFilePicker
import Data.String.Interpolate (i)
import Data.Text.Display (display)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xBindClass_, xData_, xModel_, xRef_, xShow_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

dashboardShowsGetUrl :: Links.URI
dashboardShowsGetUrl = Links.linkURI $ dashboardShowsLinks.list Nothing Nothing Nothing

dashboardShowsNewPostUrl :: Links.URI
dashboardShowsNewPostUrl = Links.linkURI dashboardShowsLinks.newPost

--------------------------------------------------------------------------------

-- | New show form template using FormBuilder
template :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
template eligibleHosts =
  buildValidatedForm
    FormBuilder
      { fbAction = [i|/#{dashboardShowsNewPostUrl}|],
        fbMethod = "post",
        fbHeader = Just renderFormHeader,
        fbFields = showFormFields eligibleHosts,
        fbAdditionalContent = [renderSubmitActions, renderScheduleManagementScript],
        fbStyles = defaultFormStyles,
        fbHtmx = Nothing
      }

--------------------------------------------------------------------------------
-- Form Header (rendered OUTSIDE <form>)

renderFormHeader :: Lucid.Html ()
renderFormHeader =
  Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8 w-full"] $ do
    Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
      Lucid.div_ $ do
        Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "CREATE NEW SHOW"
        Lucid.div_
          [Lucid.class_ "text-gray-300 text-sm"]
          "Add a new show to the station"
      Lucid.div_ [Lucid.class_ "text-center"] $
        Lucid.a_
          [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
            hxGet_ [i|/#{dashboardShowsGetUrl}|],
            hxTarget_ "#main-content",
            hxPushUrl_ "true",
            Lucid.class_ "text-blue-300 hover:text-blue-100 text-sm underline"
          ]
          "← BACK TO SHOWS"

--------------------------------------------------------------------------------
-- Form Fields Definition

showFormFields :: [UserMetadata.UserWithMetadata] -> [FormField]
showFormFields eligibleHosts =
  [ -- Basic Information Section
    SectionField
      { sfTitle = "BASIC INFORMATION",
        sfFields =
          [ ValidatedTextField
              { vfName = "title",
                vfLabel = "Show Title",
                vfInitialValue = Nothing,
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
                vtInitialValue = Nothing,
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
              { vfName = "genre",
                vfLabel = "Genre",
                vfInitialValue = Nothing,
                vfPlaceholder = Just "e.g. Techno, Ambient, Experimental, Hip-Hop",
                vfHint = Just "Primary genre or style of music",
                vfValidation =
                  ValidationRules
                    { vrMinLength = Nothing,
                      vrMaxLength = Just 100,
                      vrPattern = Nothing,
                      vrRequired = False,
                      vrCustomValidation = Nothing
                    }
              }
          ]
      },
    -- Schedule & Settings Section
    SectionField
      { sfTitle = "SCHEDULE & SETTINGS",
        sfFields =
          [ ValidatedSelectField
              { vsName = "status",
                vsLabel = "Show Status",
                vsOptions =
                  [ SelectOption "active" "Active" True Nothing,
                    SelectOption "inactive" "Inactive" False Nothing
                  ],
                vsHint = Just "Active shows appear on the shows page",
                vsValidation = emptyValidation {vrRequired = True}
              }
          ]
      },
    -- Artwork & Branding Section
    SectionField
      { sfTitle = "ARTWORK & BRANDING",
        sfFields =
          [ PlainField
              { pfHtml = logoImageField
              },
            PlainField
              { pfHtml = bannerImageField
              }
          ]
      },
    -- Hosts Section (admin only - using PlainField for multi-select)
    SectionField
      { sfTitle = "HOSTS",
        sfFields =
          [ PlainField
              { pfHtml = renderHostsMultiSelect eligibleHosts
              }
          ]
      },
    -- Schedule Section (using PlainField for dynamic list management)
    SectionField
      { sfTitle = "SCHEDULE",
        sfFields =
          [ PlainField
              { pfHtml = renderScheduleSection
              }
          ]
      }
  ]

--------------------------------------------------------------------------------
-- Image Fields

-- | Render logo image field with integrated preview.
logoImageField :: Lucid.Html ()
logoImageField =
  ImageFilePicker.render
    ImageFilePicker.Config
      { ImageFilePicker.fieldName = "logo_file",
        ImageFilePicker.label = "Logo Image",
        ImageFilePicker.existingImageUrl = "",
        ImageFilePicker.accept = "image/jpeg,image/png,image/webp,image/gif",
        ImageFilePicker.maxSizeMB = 10,
        ImageFilePicker.isRequired = False
      }

-- | Render banner image field with integrated preview.
bannerImageField :: Lucid.Html ()
bannerImageField =
  ImageFilePicker.render
    ImageFilePicker.Config
      { ImageFilePicker.fieldName = "banner_file",
        ImageFilePicker.label = "Banner Image",
        ImageFilePicker.existingImageUrl = "",
        ImageFilePicker.accept = "image/jpeg,image/png,image/webp,image/gif",
        ImageFilePicker.maxSizeMB = 10,
        ImageFilePicker.isRequired = False
      }

--------------------------------------------------------------------------------
-- Searchable Multi-Select for Hosts

renderHostsMultiSelect :: [UserMetadata.UserWithMetadata] -> Lucid.Html ()
renderHostsMultiSelect eligibleHosts = do
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
        [ Lucid.class_ "border-b border-gray-300 p-3 hover:bg-gray-200 cursor-pointer",
          xShow_ filterCondition,
          xBindClass_ "{ 'bg-blue-50': $refs.host_#{userIdText}?.checked }"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center"] $ do
            Lucid.input_
              [ Lucid.type_ "checkbox",
                Lucid.name_ "hosts",
                Lucid.id_ [i|host_#{userIdText}|],
                Lucid.value_ userIdText,
                Lucid.class_ "mr-3",
                xRef_ [i|host_#{userIdText}|]
              ]
            Lucid.label_ [Lucid.for_ [i|host_#{userIdText}|], Lucid.class_ "flex-1 cursor-pointer"] $ do
              Lucid.div_ [Lucid.class_ "font-bold"] $ Lucid.toHtml displayName
              Lucid.div_ [Lucid.class_ "text-sm text-gray-600"] $
                Lucid.toHtml (email <> " • " <> roleText)

--------------------------------------------------------------------------------
-- Schedule Section (dynamic add/remove time slots)

renderScheduleSection :: Lucid.Html ()
renderScheduleSection = do
  Lucid.p_
    [Lucid.class_ "text-sm text-gray-600 mb-4"]
    "Add recurring time slots when this show will air. Leave empty if no regular schedule."

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

renderScheduleManagementScript :: Lucid.Html ()
renderScheduleManagementScript =
  Lucid.script_
    [i|
// Schedule management module (IIFE to avoid global pollution)
(function() {
  // Schedule slot HTML template
  const createScheduleElement = () => {
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

--------------------------------------------------------------------------------
-- Form Submit Actions (rendered inside <form>)

renderSubmitActions :: Lucid.Html ()
renderSubmitActions =
  Lucid.section_ [Lucid.class_ "bg-gray-50 border-2 border-gray-300 p-6"] $ do
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.button_
        [ Lucid.type_ "submit",
          Lucid.class_ "bg-gray-800 text-white px-8 py-3 font-bold hover:bg-gray-700 transition-colors"
        ]
        "CREATE SHOW"
      Lucid.a_
        [ Lucid.href_ [i|/#{dashboardShowsGetUrl}|],
          hxGet_ [i|/#{dashboardShowsGetUrl}|],
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          Lucid.class_ "bg-gray-400 text-white px-8 py-3 font-bold hover:bg-gray-500 transition-colors no-underline"
        ]
        "CANCEL"

{-# LANGUAGE QuasiQuotes #-}

-- | Schedule Editor Component
--
-- A reusable Alpine.js-powered schedule editor with:
-- - Frequency selection (weekly / twice a month / once a month)
-- - Week-of-month selection (for non-weekly frequencies)
-- - Multiple time slots with day, time, and duration pickers
-- - Time typeahead with shorthand matching (e.g. "8p", "10:30a")
-- - JSON serialization for form submission
--
-- Used in both the New Show and Edit Show forms.
module Component.ScheduleEditor
  ( -- * Configuration
    ScheduleEditorData (..),

    -- * Rendering
    renderScheduleEditor,

    -- * Conversion
    schedulesToEditorJson,
  )
where

--------------------------------------------------------------------------------

import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BSL
import Data.Int (Int64)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (TimeOfDay (..))
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Alpine
import OrphanInstances.DayOfWeek (dayOfWeekToPostgres)
import Rel8 (Result)

--------------------------------------------------------------------------------

-- | Data needed to initialize the schedule editor.
data ScheduleEditorData = ScheduleEditorData
  { -- | JSON array of existing schedule slots, or "[]" for a new show.
    sedExistingJson :: Text,
    -- | "YYYY-MM-DD" for edit pre-population, "" for new show.
    sedStartDate :: Text
  }

--------------------------------------------------------------------------------

-- | Convert active DB schedule templates to Alpine.js editor JSON.
--
-- Reverse-maps the DB representation back to the UI frequency model:
--
-- - @weeks_of_month = Nothing@ → frequency \"weekly\"
-- - @weeks_of_month = Just [1,3]@ → frequency \"twice\"
-- - @weeks_of_month = Just [2,4]@ → frequency \"twice\"
-- - @weeks_of_month = Just [n]@ (single element) → frequency \"once\"
-- - Other multi-element lists → frequency \"twice\" (best guess)
--
-- Duration is computed as minutes between start and end times.
schedulesToEditorJson :: [ShowSchedule.ScheduleTemplate Result] -> Text
schedulesToEditorJson [] = "[]"
schedulesToEditorJson templates =
  Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode editorSlots
  where
    editorSlots = map toEditorSlot templates

    toEditorSlot sched =
      let dayText = maybe ("" :: Text) dayOfWeekToPostgres sched.stDayOfWeek
          timeText = Text.take 5 (Text.pack $ show sched.stStartTime)
          durationMins = computeDuration sched.stStartTime sched.stEndTime
          freq = weeksToFrequency sched.stWeeksOfMonth
          weeksVal = sched.stWeeksOfMonth
       in Aeson.object
            [ "frequency" Aeson..= freq,
              "weeks" Aeson..= weeksVal,
              "slots"
                Aeson..= [ Aeson.object
                             [ "day" Aeson..= dayText,
                               "time" Aeson..= timeText,
                               "duration" Aeson..= durationMins
                             ]
                         ]
            ]

    -- Compute duration in minutes between two TimeOfDay values,
    -- handling overnight shows (end < start).
    computeDuration :: TimeOfDay -> TimeOfDay -> Int
    computeDuration start end =
      let startMins = todHour start * 60 + todMin start
          endMins = todHour end * 60 + todMin end
          rawMins = endMins - startMins
       in if rawMins <= 0 then rawMins + (24 * 60) else rawMins

    weeksToFrequency :: Maybe [Int64] -> Text
    weeksToFrequency Nothing = "weekly"
    weeksToFrequency (Just ws) = case ws of
      [] -> "weekly"
      [_] -> "once"
      [1, 3] -> "twice"
      [2, 4] -> "twice"
      _ -> "twice"

--------------------------------------------------------------------------------

-- | Render the full Alpine.js schedule editor component.
--
-- The component manages:
--
-- 1. Frequency selection buttons (WEEKLY / TWICE A MONTH / ONCE A MONTH)
-- 2. Week-of-month buttons (conditional on frequency)
-- 3. Start date picker (shown once frequency is selected)
-- 4. Time slot rows (day dropdown, time picker with typeahead, duration buttons)
-- 5. A hidden @schedules_json@ input for form submission
-- 6. A hidden @schedule_start_date@ input for form submission
renderScheduleEditor :: ScheduleEditorData -> Lucid.Html ()
renderScheduleEditor ScheduleEditorData {..} =
  Lucid.div_
    [ xData_ (alpineState sedExistingJson sedStartDate)
    ]
    $ do
      renderFrequencySelector
      renderWeekSelector
      renderStartDate
      renderSlots
      renderAddButton
      renderHiddenInput
      renderStartDateHidden

--------------------------------------------------------------------------------
-- Alpine.js State

alpineState :: Text -> Text -> Text
alpineState existingJson startDateText =
  [i|{
  frequency: null,
  weeks: null,
  startDate: '#{startDateText}',
  slots: [],
  activeTimePicker: null,
  timeFilter: '',
  allTimes: (function() {
    var times = [];
    for (var h = 0; h < 24; h++) {
      for (var m = 0; m < 60; m += 30) {
        var hour24 = h.toString().padStart(2, '0');
        var min = m.toString().padStart(2, '0');
        var value = hour24 + ':' + min;
        var period = h < 12 ? 'AM' : 'PM';
        var hour12 = h % 12 === 0 ? 12 : h % 12;
        var label = hour12 + ':' + min + ' ' + period;
        times.push({ value: value, label: label });
      }
    }
    return times;
  })(),

  init() {
    var existing = #{existingJson};
    if (existing && existing.length > 0) {
      var first = existing[0];
      this.frequency = first.frequency || null;
      this.weeks = first.weeks || null;
      this.slots = [];
      for (var i = 0; i < existing.length; i++) {
        var item = existing[i];
        if (item.slots && item.slots.length > 0) {
          for (var j = 0; j < item.slots.length; j++) {
            this.slots.push({
              day: item.slots[j].day || '',
              time: item.slots[j].time || '',
              duration: item.slots[j].duration || null
            });
          }
        }
      }
    }
  },

  setFrequency(f) {
    this.frequency = f;
    if (f === 'weekly') {
      this.weeks = null;
    } else if (f === 'twice') {
      this.weeks = [1, 3];
    } else if (f === 'once') {
      this.weeks = [1];
    }
    if (this.slots.length === 0) {
      this.slots.push({ day: '', time: '', duration: null });
    }
  },

  setWeeks(w) {
    this.weeks = w;
  },

  addSlot() {
    this.slots.push({ day: '', time: '', duration: null });
  },

  removeSlot(index) {
    this.slots.splice(index, 1);
  },

  setDuration(index, dur) {
    this.slots[index].duration = dur;
  },

  openTimePicker(index) {
    this.activeTimePicker = index;
    this.timeFilter = this.slots[index].time || '';
  },

  closeTimePicker() {
    this.activeTimePicker = null;
    this.timeFilter = '';
  },

  selectTime(index, time) {
    this.slots[index].time = time;
    this.closeTimePicker();
  },

  filteredTimes() {
    var filter = this.timeFilter.trim().toLowerCase();
    if (!filter) return this.allTimes;
    return this.allTimes.filter(t =>
      t.label.toLowerCase().includes(filter) ||
      t.value.toLowerCase().includes(filter) ||
      this.matchShorthand(filter, t)
    );
  },

  matchShorthand(input, time) {
    // Patterns: "8p", "8pm", "8:30p", "8:30pm", "20:00", "14:30"
    var patterns = [
      new RegExp('^(\\d{1,2})(a|am|p|pm)$', 'i'),
      new RegExp('^(\\d{1,2}):(\\d{2})(a|am|p|pm)$', 'i'),
      new RegExp('^(\\d{1,2}):(\\d{2})$')
    ];
    var p1 = patterns[0].exec(input);
    if (p1) {
      var h = parseInt(p1[1], 10);
      var isPM = p1[2].toLowerCase().startsWith('p');
      if (isPM && h !== 12) h += 12;
      if (!isPM && h === 12) h = 0;
      var expected = h.toString().padStart(2, '0') + ':00';
      return time.value === expected;
    }
    var p2 = patterns[1].exec(input);
    if (p2) {
      var h2 = parseInt(p2[1], 10);
      var min2 = p2[2];
      var isPM2 = p2[3].toLowerCase().startsWith('p');
      if (isPM2 && h2 !== 12) h2 += 12;
      if (!isPM2 && h2 === 12) h2 = 0;
      var expected2 = h2.toString().padStart(2, '0') + ':' + min2;
      return time.value === expected2;
    }
    var p3 = patterns[2].exec(input);
    if (p3) {
      return time.value.startsWith(input);
    }
    return false;
  },

  formatTime(value) {
    if (!value) return '';
    var parts = value.split(':');
    if (parts.length < 2) return value;
    var h = parseInt(parts[0], 10);
    var m = parts[1];
    var period = h < 12 ? 'AM' : 'PM';
    var h12 = h % 12 === 0 ? 12 : h % 12;
    return h12 + ':' + m + ' ' + period;
  },

  serializeForSubmit() {
    if (!this.frequency || this.slots.length === 0) return '[]';
    var weeks = this.frequency === 'weekly' ? [1, 2, 3, 4, 5] : (this.weeks || []);
    var valid = this.slots
      .filter(function(s) { return s.day && s.time && s.duration; })
      .map(function(s) {
        return {
          dayOfWeek: s.day,
          weeksOfMonth: weeks,
          startTime: s.time,
          duration: s.duration
        };
      });
    return JSON.stringify(valid);
  }
}|]

--------------------------------------------------------------------------------
-- Frequency Selector

renderFrequencySelector :: Lucid.Html ()
renderFrequencySelector =
  Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
    Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2]] "HOW OFTEN DOES THIS SHOW AIR?"
    Lucid.div_ [class_ $ base ["flex", "gap-2", "flex-wrap"]] $ do
      freqButton "weekly" "WEEKLY"
      freqButton "twice" "TWICE A MONTH"
      freqButton "once" "ONCE A MONTH"

freqButton :: Text -> Text -> Lucid.Html ()
freqButton freqVal label =
  Lucid.button_
    [ Lucid.type_ "button",
      xOnClick_ [i|setFrequency('#{freqVal}')|],
      class_ $ base [Tokens.border2, Tokens.fontBold, Tokens.textSm, "hover:opacity-80", Tokens.px4, Tokens.py2],
      xBindClass_
        [i|frequency === '#{freqVal}'
          ? '#{Tokens.infoBg} #{Tokens.infoText} #{Tokens.infoBorder}'
          : '#{Tokens.bgAlt} #{Tokens.fgPrimary} #{Tokens.borderMuted}'|]
    ]
    (Lucid.toHtml label)

--------------------------------------------------------------------------------
-- Week Selector

renderWeekSelector :: Lucid.Html ()
renderWeekSelector =
  Lucid.div_
    [ class_ $ base [Tokens.mb4],
      xShow_ "frequency === 'twice' || frequency === 'once'"
    ]
    $ do
      Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2]] "WHICH WEEKS?"

      -- Twice-a-month options
      Lucid.div_
        [ class_ $ base ["flex", "gap-2"],
          xShow_ "frequency === 'twice'"
        ]
        $ do
          weeksButton "[1,3]" "1ST & 3RD"
          weeksButton "[2,4]" "2ND & 4TH"

      -- Once-a-month options
      Lucid.div_
        [ class_ $ base ["flex", "gap-2", "flex-wrap"],
          xShow_ "frequency === 'once'"
        ]
        $ do
          onceWeekButton "[1]" "1ST"
          onceWeekButton "[2]" "2ND"
          onceWeekButton "[3]" "3RD"
          onceWeekButton "[4]" "4TH"

weeksButton :: Text -> Text -> Lucid.Html ()
weeksButton jsArray label =
  Lucid.button_
    [ Lucid.type_ "button",
      xOnClick_ [i|setWeeks(#{jsArray})|],
      class_ $ base [Tokens.border2, Tokens.fontBold, Tokens.textSm, "hover:opacity-80", Tokens.px4, Tokens.py2],
      xBindClass_
        [i|JSON.stringify(weeks) === JSON.stringify(#{jsArray})
          ? '#{Tokens.infoBg} #{Tokens.infoText} #{Tokens.infoBorder}'
          : '#{Tokens.bgAlt} #{Tokens.fgPrimary} #{Tokens.borderMuted}'|]
    ]
    (Lucid.toHtml label)

onceWeekButton :: Text -> Text -> Lucid.Html ()
onceWeekButton = weeksButton

--------------------------------------------------------------------------------
-- Time Slots

renderSlots :: Lucid.Html ()
renderSlots =
  Lucid.div_ [xShow_ "frequency !== null"] $
    Lucid.template_
      [xFor_ "(slot, index) in slots", xKey_ "index"]
      renderSlotRow

renderSlotRow :: Lucid.Html ()
renderSlotRow =
  Lucid.div_
    [ class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgAlt, Tokens.p4, Tokens.mb2]
    ]
    $ do
      -- Row label
      Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mb2]] $ do
        Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold]] "TIME SLOT"
        -- Remove button (only if more than one slot)
        Lucid.template_ [xIf_ "slots.length > 1"] $
          Lucid.button_
            [ Lucid.type_ "button",
              xOnClick_ "removeSlot(index)",
              class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.errorText, "hover:opacity-80"]
            ]
            "REMOVE"

      Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-3", Tokens.gap4]] $ do
        -- Day dropdown
        renderDayDropdown

        -- Time picker
        renderTimePicker

        -- Duration buttons
        renderDurationButtons

--------------------------------------------------------------------------------
-- Day Dropdown

renderDayDropdown :: Lucid.Html ()
renderDayDropdown =
  Lucid.div_ $ do
    Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] "DAY"
    Lucid.select_
      [ Lucid.name_ "",
        xModel_ "slot.day",
        class_ $ base ["w-full", Tokens.p3, Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", Tokens.textSm]
      ]
      $ do
        Lucid.option_ [Lucid.value_ ""] "-- Select Day --"
        mapM_
          (\(val, lbl) -> Lucid.option_ [Lucid.value_ val] lbl)
          [ ("sunday", "Sunday"),
            ("monday", "Monday"),
            ("tuesday", "Tuesday"),
            ("wednesday", "Wednesday"),
            ("thursday", "Thursday"),
            ("friday", "Friday"),
            ("saturday", "Saturday")
          ]

--------------------------------------------------------------------------------
-- Time Picker with Typeahead

renderTimePicker :: Lucid.Html ()
renderTimePicker =
  Lucid.div_ [class_ $ base ["relative"]] $ do
    Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] "TIME"

    -- Display input (opens picker)
    Lucid.input_
      [ Lucid.type_ "text",
        Lucid.placeholder_ "e.g. 8:00 PM",
        xOnClick_ "openTimePicker(index)",
        xOnInput_ "timeFilter = $event.target.value",
        xBindValue_ "activeTimePicker === index ? timeFilter : formatTime(slot.time)",
        xOnClickOutside_ "if (activeTimePicker === index) closeTimePicker()",
        class_ $ base ["w-full", Tokens.p3, Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", Tokens.textSm]
      ]

    -- Dropdown list
    Lucid.div_
      [ class_ $ base ["absolute", "z-10", "w-full", "max-h-48", "overflow-y-auto", Tokens.border2, Tokens.borderMuted, Tokens.bgMain],
        xShow_ "activeTimePicker === index"
      ]
      $ Lucid.template_
        [xFor_ "t in filteredTimes()", xKey_ "t.value"]
      $ Lucid.div_
        [ xOnClick_ "selectTime(index, t.value)",
          class_ $ base [Tokens.p3, Tokens.textSm, "cursor-pointer", Tokens.hoverBg, "font-mono"],
          xText_ "t.label"
        ]
        mempty

--------------------------------------------------------------------------------
-- Duration Buttons

renderDurationButtons :: Lucid.Html ()
renderDurationButtons =
  Lucid.div_ $ do
    Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] "DURATION"
    Lucid.div_ [class_ $ base ["flex", Tokens.gap2]] $ do
      durationButton 30 "30m"
      durationButton 60 "1hr"
      durationButton 120 "2hr"

durationButton :: Int -> Text -> Lucid.Html ()
durationButton dur label =
  Lucid.button_
    [ Lucid.type_ "button",
      xOnClick_ [i|setDuration(index, #{dur})|],
      class_ $ base [Tokens.border2, Tokens.fontBold, Tokens.textSm, "hover:opacity-80", Tokens.px4, Tokens.py2],
      xBindClass_
        [i|slot.duration === #{dur}
          ? '#{Tokens.infoBg} #{Tokens.infoText} #{Tokens.infoBorder}'
          : '#{Tokens.bgAlt} #{Tokens.fgPrimary} #{Tokens.borderMuted}'|]
    ]
    (Lucid.toHtml label)

--------------------------------------------------------------------------------
-- Start Date Picker

-- | Date input shown once a frequency is selected.
--
-- Lets staff choose when the new schedule takes effect. Defaults to whatever
-- @startDate@ is in the Alpine state (empty string for new shows, a pre-filled
-- date for existing shows).
renderStartDate :: Lucid.Html ()
renderStartDate =
  Lucid.div_ [xShow_ "frequency !== null", class_ $ base [Tokens.mb4]] $ do
    Lucid.label_ [class_ $ base [Tokens.fontBold, Tokens.textSm, "block", Tokens.mb2]] "When does this schedule start?"
    Lucid.input_
      [ Lucid.type_ "date",
        xModel_ "startDate",
        class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", "p-2"]
      ]

-- | Hidden input that carries @startDate@ to the server on form submission.
renderStartDateHidden :: Lucid.Html ()
renderStartDateHidden =
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedule_start_date",
      xBindValue_ "startDate"
    ]

--------------------------------------------------------------------------------
-- Add Slot Button

renderAddButton :: Lucid.Html ()
renderAddButton =
  Lucid.div_
    [ class_ $ base [Tokens.mb4],
      xShow_ "frequency !== null"
    ]
    $ Lucid.button_
      [ Lucid.type_ "button",
        xOnClick_ "addSlot()",
        class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgAlt, Tokens.fontBold, Tokens.textSm, "hover:opacity-80", Tokens.px4, Tokens.py2, "w-full"]
      ]
      "+ ADD ANOTHER DAY"

--------------------------------------------------------------------------------
-- Hidden Input

renderHiddenInput :: Lucid.Html ()
renderHiddenInput =
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedules_json",
      xBindValue_ "serializeForSubmit()"
    ]

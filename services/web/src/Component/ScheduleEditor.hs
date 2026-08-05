{-# LANGUAGE QuasiQuotes #-}

-- | Schedule Editor Component
--
-- A reusable Alpine.js-powered schedule editor with:
-- - Frequency selection (weekly / twice a month / once a month)
-- - Week-of-month selection (for non-weekly frequencies)
-- - One time slot with day, time, and duration pickers, plus an optional replay
-- - Time typeahead with shorthand matching (e.g. "8p", "10:30a")
-- - JSON serialization for form submission
--
-- Used in both the New Show and Edit Show forms.
--
-- A show holds one slot, which @one_active_slot_per_show@ enforces in the database.
-- The frequency and the weeks therefore belong to the slot, and @serializeForSubmit@
-- emits a one-element array so the three write paths keep a single JSON shape.
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
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Data.Time (TimeOfDay (..))
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Recurrence (frequencyLabel, recurrenceDay, recurrenceFromRow, weekNumbers)
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Lucid qualified
import Lucid.Alpine
import Lucid.Base qualified as LucidBase
import OrphanInstances.DayOfWeek (dayOfWeekToPostgres)
import Rel8 (Result)

--------------------------------------------------------------------------------

-- | Data needed to initialize the schedule editor.
data ScheduleEditorData = ScheduleEditorData
  { -- | The existing slot as a JSON object, or "null" for a new show.
    sedExistingJson :: Text,
    -- | "YYYY-MM-DD" for edit pre-population, "" for new show.
    sedStartDate :: Text,
    -- | "YYYY-MM-DD" lower bound for the date picker; "" means no bound.
    sedMinDate :: Text
  }

--------------------------------------------------------------------------------

-- | Convert a show's active schedule template to Alpine.js editor JSON.
--
-- Emits @"null"@ when the show holds no slot, which @init()@ reads as an empty form.
-- 'Domain.Types.Recurrence.frequencyLabel' decides which of the three frequency
-- buttons is selected, so the form agrees with every other surface that names the
-- same recurrence.
--
-- Duration is computed as minutes between start and end times.
schedulesToEditorJson :: Maybe (ShowSchedule.ScheduleTemplate Result) -> Text
schedulesToEditorJson Nothing = "null"
schedulesToEditorJson (Just sched) =
  Text.decodeUtf8 $ BSL.toStrict $ Aeson.encode editorSlot
  where
    recurrence = recurrenceFromRow sched.stDayOfWeek sched.stWeeksOfMonth

    editorSlot =
      Aeson.object
        [ "frequency" Aeson..= frequencyLabel recurrence,
          "weeks" Aeson..= weekNumbers recurrence,
          "day" Aeson..= dayOfWeekToPostgres (recurrenceDay recurrence),
          "time" Aeson..= Text.take 5 (Text.pack $ show sched.stStartTime),
          "duration" Aeson..= computeDuration sched.stStartTime sched.stEndTime,
          "replayTime" Aeson..= case sched.stReplayStartTime of
            Just rt -> Text.take 5 (Text.pack $ show rt)
            Nothing -> "" :: Text
        ]

    -- Compute duration in minutes between two TimeOfDay values,
    -- handling overnight shows (end < start).
    computeDuration :: TimeOfDay -> TimeOfDay -> Int
    computeDuration start end =
      let startMins = todHour start * 60 + todMin start
          endMins = todHour end * 60 + todMin end
          rawMins = endMins - startMins
       in if rawMins <= 0 then rawMins + (24 * 60) else rawMins

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
      renderStartDate sedMinDate
      renderSlot
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
  slot: { day: '', time: '', duration: null, replayTime: '' },
  timePickerOpen: false,
  replayPickerOpen: false,
  timeFilter: '',
  replayFilter: '',
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
    if (existing) {
      this.frequency = existing.frequency || null;
      this.weeks = existing.weeks || null;
      this.slot = {
        day: existing.day || '',
        time: existing.time || '',
        duration: existing.duration || null,
        replayTime: existing.replayTime || ''
      };
    }
  },

  setFrequency(f) {
    this.frequency = f;
    if (f === 'weekly') {
      this.weeks = [1, 2, 3, 4, 5];
    } else if (f === 'twice') {
      this.weeks = [1, 3];
    } else if (f === 'once') {
      this.weeks = [1];
    }
  },

  setWeeks(w) {
    this.weeks = w;
  },

  setDuration(dur) {
    this.slot.duration = dur;
  },

  openTimePicker() {
    this.timePickerOpen = true;
    this.timeFilter = this.formatTime(this.slot.time);
  },

  closeTimePicker(resolve) {
    if (resolve !== false && this.timePickerOpen) {
      var text = (this.timeFilter || '').trim();
      if (text) {
        var match = this.resolveFilter(text, this.filteredTimes());
        if (match) this.slot.time = match;
      }
    }
    this.timePickerOpen = false;
    this.timeFilter = '';
  },

  selectTime(time) {
    this.slot.time = time;
    this.closeTimePicker(false);
  },

  openReplayPicker() {
    this.replayPickerOpen = true;
    this.replayFilter = this.formatTime(this.slot.replayTime);
  },

  closeReplayPicker(resolve) {
    if (resolve !== false && this.replayPickerOpen) {
      var text = (this.replayFilter || '').trim();
      if (text) {
        var match = this.resolveFilter(text, this.filteredReplayTimes());
        if (match) this.slot.replayTime = match;
      }
    }
    this.replayPickerOpen = false;
    this.replayFilter = '';
  },

  selectReplayTime(time) {
    this.slot.replayTime = time;
    this.closeReplayPicker(false);
  },

  resolveFilter(text, filtered) {
    if (!text) return null;
    if (filtered.length === 1) return filtered[0].value;
    var lower = text.toLowerCase();
    var exact = this.allTimes.find(function(t) {
      return t.label.toLowerCase() === lower || t.value === text;
    });
    if (exact) return exact.value;
    var shorthand = this.allTimes.find(function(t) {
      return this.matchShorthand(lower, t);
    }.bind(this));
    if (shorthand) return shorthand.value;
    return null;
  },

  clearReplayTime() {
    this.slot.replayTime = '';
  },

  slotEndTime() {
    if (!this.slot.time || !this.slot.duration) return null;
    var parts = this.slot.time.split(':');
    var h = parseInt(parts[0], 10);
    var m = parseInt(parts[1], 10);
    var totalMins = h * 60 + m + this.slot.duration;
    return totalMins % (24 * 60);
  },

  filteredReplayTimes() {
    var filter = this.replayFilter.trim().toLowerCase();
    var endMins = this.slotEndTime();
    var available = this.allTimes;
    if (endMins !== null) {
      available = available.filter(function(t) {
        var parts = t.value.split(':');
        var tMins = parseInt(parts[0], 10) * 60 + parseInt(parts[1], 10);
        return tMins >= endMins;
      });
    }
    if (!filter) return available;
    var self = this;
    return available.filter(function(t) {
      return t.label.toLowerCase().includes(filter) ||
        t.value.toLowerCase().includes(filter) ||
        self.matchShorthand(filter, t);
    });
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
    var s = this.slot;
    if (!this.frequency || !s.day || !s.time || !s.duration) return '[]';
    var obj = {
      dayOfWeek: s.day,
      weeksOfMonth: this.weeks || [1, 2, 3, 4, 5],
      startTime: s.time,
      duration: s.duration
    };
    if (s.replayTime) { obj.replayTime = s.replayTime; }
    return JSON.stringify([obj]);
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

renderSlot :: Lucid.Html ()
renderSlot =
  Lucid.div_
    [ xShow_ "frequency !== null",
      class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgAlt, Tokens.p4, Tokens.mb2]
    ]
    $ do
      Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mb2]] $
        Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fontBold]] "TIME SLOT"

      Lucid.div_ [class_ $ base ["grid", "grid-cols-1", "md:grid-cols-3", Tokens.gap4]] $ do
        -- Day dropdown
        renderDayDropdown

        -- Time picker
        renderTimePicker

        -- Duration buttons
        renderDurationButtons

      -- Replay time picker (shown when time and duration are set)
      renderReplayTimePicker

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
  Lucid.div_
    [ class_ $ base ["relative"],
      xOnClickOutside_ "if (timePickerOpen) closeTimePicker()"
    ]
    $ do
      Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.mb2, "block"]] "TIME"

      -- Display input (opens picker)
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.placeholder_ "e.g. 8:00 PM",
          xOnClick_ "openTimePicker()",
          xOnInput_ "timeFilter = $event.target.value",
          xBindValue_ "timePickerOpen ? timeFilter : formatTime(slot.time)",
          xOn_ "keydown.enter.prevent" "closeTimePicker()",
          xOn_ "keydown.escape" "closeTimePicker(false)",
          class_ $ base ["w-full", Tokens.p3, Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", Tokens.textSm]
        ]

      -- Dropdown list
      Lucid.div_
        [ class_ $ base ["absolute", "z-10", "w-full", "max-h-48", "overflow-y-auto", Tokens.border2, Tokens.borderMuted, Tokens.bgMain],
          xShow_ "timePickerOpen"
        ]
        $ do
          Lucid.template_
            [xFor_ "t in filteredTimes()", xKey_ "t.value"]
            $ Lucid.div_
              [ xOnClick_ "selectTime(t.value)",
                class_ $ base [Tokens.p3, Tokens.textSm, "cursor-pointer", Tokens.hoverBg, "font-mono"],
                xText_ "t.label"
              ]
              mempty
          Lucid.div_
            [ class_ $ base [Tokens.p3, Tokens.textSm, Tokens.errorText, "font-mono"],
              xShow_ "timeFilter.trim() && filteredTimes().length === 0"
            ]
            "No matching times \x2014 try \"8:00 PM\" or \"8p\""

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
      xOnClick_ [i|setDuration(#{dur})|],
      class_ $ base [Tokens.border2, Tokens.fontBold, Tokens.textSm, "hover:opacity-80", Tokens.px4, Tokens.py2],
      xBindClass_
        [i|slot.duration === #{dur}
          ? '#{Tokens.infoBg} #{Tokens.infoText} #{Tokens.infoBorder}'
          : '#{Tokens.bgAlt} #{Tokens.fgPrimary} #{Tokens.borderMuted}'|]
    ]
    (Lucid.toHtml label)

--------------------------------------------------------------------------------
-- Replay Time Picker

renderReplayTimePicker :: Lucid.Html ()
renderReplayTimePicker =
  Lucid.div_
    [ class_ $ base [Tokens.mt4],
      xShow_ "slot.time && slot.duration"
    ]
    $ do
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2, Tokens.mb2]] $ do
        Lucid.label_ [class_ $ base [Tokens.textSm, Tokens.fontBold]] "REPLAY TIME"
        Lucid.span_ [class_ $ base [Tokens.textSm, Tokens.fgMuted]] "(optional)"
      Lucid.div_ [class_ $ base ["flex", "items-center", Tokens.gap2]] $ do
        -- Replay time input with typeahead
        Lucid.div_
          [ class_ $ base ["relative", "flex-1"],
            xOnClickOutside_ "if (replayPickerOpen) closeReplayPicker()"
          ]
          $ do
            Lucid.input_
              [ Lucid.type_ "text",
                Lucid.placeholder_ "e.g. 8:00 PM",
                xOnClick_ "openReplayPicker()",
                xOnInput_ "replayFilter = $event.target.value",
                xBindValue_ "replayPickerOpen ? replayFilter : formatTime(slot.replayTime)",
                xOn_ "keydown.enter.prevent" "closeReplayPicker()",
                xOn_ "keydown.escape" "closeReplayPicker(false)",
                class_ $ base ["w-full", Tokens.p3, Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", Tokens.textSm]
              ]
            -- Dropdown list
            Lucid.div_
              [ class_ $ base ["absolute", "z-10", "w-full", "max-h-48", "overflow-y-auto", Tokens.border2, Tokens.borderMuted, Tokens.bgMain],
                xShow_ "replayPickerOpen"
              ]
              $ do
                Lucid.template_
                  [xFor_ "t in filteredReplayTimes()", xKey_ "t.value"]
                  $ Lucid.div_
                    [ xOnClick_ "selectReplayTime(t.value)",
                      class_ $ base [Tokens.p3, Tokens.textSm, "cursor-pointer", Tokens.hoverBg, "font-mono"],
                      xText_ "t.label"
                    ]
                    mempty
                Lucid.div_
                  [ class_ $ base [Tokens.p3, Tokens.textSm, Tokens.errorText, "font-mono"],
                    xShow_ "replayFilter.trim() && filteredReplayTimes().length === 0"
                  ]
                  "No matching times \x2014 try \"8:00 PM\" or \"8p\""
        -- Clear button
        Lucid.template_ [xIf_ "slot.replayTime"] $
          Lucid.button_
            [ Lucid.type_ "button",
              xOnClick_ "clearReplayTime()",
              class_ $ base [Tokens.textSm, Tokens.fontBold, Tokens.errorText, "hover:opacity-80", Tokens.px4, Tokens.py2]
            ]
            "CLEAR"

--------------------------------------------------------------------------------
-- Start Date Picker

-- | Date input shown once a frequency is selected.
--
-- Lets staff choose when the new schedule takes effect. Defaults to whatever
-- @startDate@ is in the Alpine state (empty string for new shows, a pre-filled
-- date for existing shows). When @minDate@ is non-empty it becomes the input's
-- @min@ attribute, greying out earlier dates in the native picker.
renderStartDate :: Text -> Lucid.Html ()
renderStartDate minDate =
  Lucid.div_ [xShow_ "frequency !== null", class_ $ base [Tokens.mb4]] $ do
    Lucid.label_ [class_ $ base [Tokens.fontBold, Tokens.textSm, "block", Tokens.mb2]] "When does this schedule start?"
    Lucid.input_ $
      [ Lucid.type_ "date",
        xModel_ "startDate",
        class_ $ base [Tokens.border2, Tokens.borderMuted, Tokens.bgMain, Tokens.fgPrimary, "font-mono", "p-2"]
      ]
        <> [LucidBase.makeAttributes "min" minDate | not (Text.null minDate)]

-- | Hidden input that carries @startDate@ to the server on form submission.
renderStartDateHidden :: Lucid.Html ()
renderStartDateHidden =
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedule_start_date",
      xBindValue_ "startDate"
    ]

--------------------------------------------------------------------------------
-- Hidden Input

renderHiddenInput :: Lucid.Html ()
renderHiddenInput =
  Lucid.input_
    [ Lucid.type_ "hidden",
      Lucid.name_ "schedules_json",
      xBindValue_ "serializeForSubmit()"
    ]

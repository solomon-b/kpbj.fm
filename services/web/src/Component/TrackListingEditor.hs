{-# LANGUAGE QuasiQuotes #-}

-- | Track Listing Editor Component
--
-- A reusable table-based track editor with:
-- - Click-to-edit inline editing for title and artist
-- - Drag-and-drop reordering using native HTML5 APIs
-- - A text view for bulk edits, one @Artist - Title@ line for each track
-- - JSON serialization for form submission
--
-- Used in both Episode New and Episode Edit forms.
module Component.TrackListingEditor
  ( -- * Configuration
    Config (..),
    TrackData (..),

    -- * Rendering
    render,

    -- * Conversion
    fromEpisodeTrack,
  )
where

--------------------------------------------------------------------------------

import Component.Button ((&))
import Component.Button qualified as Button
import Component.Input qualified as Input
import Data.Aeson (FromJSON, ToJSON, (.=))
import Data.Aeson qualified as Aeson
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Encoding qualified as TLE
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.EpisodeTrack qualified as EpisodeTrack
import GHC.Generics (Generic)
import Lucid qualified
import Lucid.Alpine

--------------------------------------------------------------------------------

-- | Configuration for the track listing editor.
data Config = Config
  { -- | Unique identifier for Alpine.js state isolation
    editorId :: Text,
    -- | Initial track data (empty list for new episodes)
    initialTracks :: [TrackData],
    -- | Name of the hidden field containing JSON (e.g., "tracks_json")
    jsonFieldName :: Text
  }

-- | Track data structure matching the server-side TrackInfo type.
--
-- The field names use the @ti@ prefix to match JSON parsing on the server.
data TrackData = TrackData
  { tdTitle :: Text,
    tdArtist :: Text
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Convert a database EpisodeTrack model to TrackData for the editor.
fromEpisodeTrack :: EpisodeTrack.Model -> TrackData
fromEpisodeTrack track =
  TrackData
    { tdTitle = track.title,
      tdArtist = track.artist
    }

--------------------------------------------------------------------------------

-- | Render the track listing editor with Alpine.js state management.
--
-- The component outputs:
--
-- - A compact table with drag handles, title, artist columns, and delete buttons
-- - A + icon in the last row to add new tracks
-- - A hidden input field containing JSON array of tracks
--
-- The JSON format uses @tiTitle@ and @tiArtist@ field names to match
-- the server-side @TrackInfo@ type.
render :: Config -> Lucid.Html ()
render Config {..} = do
  -- Encode initial tracks as JSON for Alpine.js state initialization
  let initialTracksJson :: Text
      initialTracksJson =
        TL.toStrict $
          TLE.decodeUtf8 $
            Aeson.encode $
              map (\t -> Aeson.object ["tiTitle" .= tdTitle t, "tiArtist" .= tdArtist t]) initialTracks

  -- Main container with Alpine.js state
  Lucid.div_
    [ Lucid.id_ editorId,
      xData_ (alpineState initialTracksJson)
    ]
    $ do
      -- Track table (hidden while the text view is open)
      Lucid.div_ [xShow_ "!bulkOpen"] $ do
        renderTrackTable
        renderBulkButton

      -- Text view of the same list
      renderBulkEditor

      -- Hidden JSON field for form submission. An open text view wins, so a
      -- save without APPLY keeps the text.
      Lucid.input_
        [ Lucid.type_ "hidden",
          Lucid.name_ jsonFieldName,
          xBindValue_ "JSON.stringify(bulkOpen ? parseTracks(bulkText) : tracks)"
        ]

--------------------------------------------------------------------------------
-- Alpine.js State

-- | Generate the Alpine.js state object as a JavaScript string.
alpineState :: Text -> Text
alpineState initialTracksJson =
  [i|{
  tracks: #{initialTracksJson},
  editingRow: null,
  editingField: null,
  editValue: '',
  isTabbing: false,
  draggedIndex: null,
  dragOverIndex: null,
  bulkOpen: false,
  bulkText: '',

  formatTracks() {
    return this.tracks
      .map(t => t.tiArtist ? t.tiArtist + ' - ' + t.tiTitle : t.tiTitle)
      .filter(line => line !== '')
      .join('\\n');
  },

  parseTracks(text) {
    return text.split(/\\r?\\n/)
      .map(line => {
        const m = line.match(/^(.*?)\\s[-–—]\\s(.*)$/);
        return m
          ? { tiArtist: m[1].trim(), tiTitle: m[2].trim() }
          : { tiArtist: '', tiTitle: line.trim() };
      })
      .filter(t => t.tiTitle !== '' || t.tiArtist !== '');
  },

  openBulk() {
    this.saveEdit();
    this.bulkText = this.formatTracks();
    this.bulkOpen = true;
  },

  applyBulk() {
    this.tracks = this.parseTracks(this.bulkText);
    this.cancelBulk();
  },

  cancelBulk() {
    this.bulkOpen = false;
    this.bulkText = '';
  },

  addTrack() {
    this.tracks.push({ tiTitle: '', tiArtist: '' });
    this.editingRow = this.tracks.length - 1;
    this.editingField = 'title';
    this.editValue = '';
  },

  removeTrack(index) {
    this.tracks.splice(index, 1);
  },

  startEdit(index, field) {
    if (this.draggedIndex !== null) return;
    this.editingRow = index;
    this.editingField = field;
    this.editValue = field === 'title' ? this.tracks[index].tiTitle : this.tracks[index].tiArtist;
  },

  saveEdit() {
    if (this.editingRow === null) return;
    const value = this.editValue.trim();
    if (this.editingField === 'title') {
      this.tracks[this.editingRow].tiTitle = value;
    } else {
      this.tracks[this.editingRow].tiArtist = value;
    }
    this.editingRow = null;
    this.editingField = null;
    this.editValue = '';
  },

  cancelEdit() {
    this.editingRow = null;
    this.editingField = null;
    this.editValue = '';
  },

  handleTab(event) {
    event.preventDefault();
    if (this.editingRow === null) return;
    this.isTabbing = true;
    const row = this.editingRow;
    const field = this.editingField;
    this.saveEdit();
    if (event.shiftKey) {
      if (field === 'artist') {
        this.startEdit(row, 'title');
      } else if (row > 0) {
        this.startEdit(row - 1, 'artist');
      }
    } else {
      if (field === 'title') {
        this.startEdit(row, 'artist');
      } else if (row < this.tracks.length - 1) {
        this.startEdit(row + 1, 'title');
      }
    }
    this.$nextTick(() => { this.isTabbing = false; });
  },

  handleDragStart(event, index) {
    this.draggedIndex = index;
    event.dataTransfer.effectAllowed = 'move';
    event.target.closest('tr').classList.add('opacity-50');
  },

  handleDragOver(event, index) {
    if (this.draggedIndex === null || this.draggedIndex === index) return;
    event.dataTransfer.dropEffect = 'move';
    this.dragOverIndex = index;
  },

  handleDragLeave(event) {
    if (!event.currentTarget.contains(event.relatedTarget)) {
      this.dragOverIndex = null;
    }
  },

  handleDrop(event, index) {
    if (this.draggedIndex === null || this.draggedIndex === index) {
      this.dragOverIndex = null;
      return;
    }
    const track = this.tracks.splice(this.draggedIndex, 1)[0];
    this.tracks.splice(index, 0, track);
    this.draggedIndex = null;
    this.dragOverIndex = null;
  },

  handleDragEnd(event) {
    event.target.closest('tr')?.classList.remove('opacity-50');
    this.draggedIndex = null;
    this.dragOverIndex = null;
  }
}|]

--------------------------------------------------------------------------------
-- Table Rendering

-- | Render the track table with header and body.
renderTrackTable :: Lucid.Html ()
renderTrackTable =
  Lucid.table_ [class_ $ base ["w-full", "border-collapse", Tokens.border2, Tokens.borderMuted, Tokens.textSm]] $ do
    renderTableHeader
    renderTableBody

-- | Render the table header row.
renderTableHeader :: Lucid.Html ()
renderTableHeader =
  Lucid.thead_ [class_ $ base [Tokens.bgInverse, Tokens.fgInverse]] $
    Lucid.tr_ $ do
      -- Drag handle column
      Lucid.th_ [class_ $ base ["p-2", "w-8"]] mempty
      -- Track number column
      Lucid.th_ [class_ $ base ["p-2", "w-10", "text-center"]] "#"
      -- Title column
      Lucid.th_ [class_ $ base ["p-2", "text-left"]] "Title"
      -- Artist column
      Lucid.th_ [class_ $ base ["p-2", "text-left"]] "Artist"
      -- Delete button column
      Lucid.th_ [class_ $ base ["p-2", "w-10"]] mempty

-- | Render the table body with x-for loop over tracks.
renderTableBody :: Lucid.Html ()
renderTableBody =
  Lucid.tbody_ $ do
    -- Track rows
    Lucid.template_
      [xFor_ "(track, index) in tracks", xKey_ "index"]
      renderTrackRow
    -- Add row (always present)
    renderAddRow

-- | Render a single track row with all cells.
renderTrackRow :: Lucid.Html ()
renderTrackRow =
  Lucid.tr_
    [ Lucid.draggable_ "true",
      class_ $ base [Tokens.bgMain],
      xBindClass_ [i|{ '#{Tokens.bgAlt} #{Tokens.border2} #{Tokens.infoBorder}': dragOverIndex === index }|],
      xOn_ "dragstart" "handleDragStart($event, index)",
      xOnDragover_ "handleDragOver($event, index)",
      xOnDragleave_ "handleDragLeave($event)",
      xOnDrop_ "handleDrop($event, index)",
      xOn_ "dragend" "handleDragEnd($event)"
    ]
    $ do
      -- Drag handle cell
      renderDragHandleCell

      -- Track number cell (auto-calculated)
      renderTrackNumberCell

      -- Title cell (editable)
      renderEditableCell "title" "tiTitle"

      -- Artist cell (editable)
      renderEditableCell "artist" "tiArtist"

      -- Delete button cell
      renderDeleteCell

-- | Render the drag handle cell.
renderDragHandleCell :: Lucid.Html ()
renderDragHandleCell =
  Lucid.td_ [class_ $ base [Tokens.p3, "cursor-move", Tokens.fgMuted, "text-center"]] $
    Lucid.span_ [class_ $ base ["text-lg", "select-none"]] $
      Lucid.toHtmlRaw ("&#x2630;" :: Text) -- hamburger icon

-- | Render the track number cell.
renderTrackNumberCell :: Lucid.Html ()
renderTrackNumberCell =
  Lucid.td_
    [ class_ $ base [Tokens.p3, "text-center", "font-mono", Tokens.fgMuted],
      xText_ "index + 1"
    ]
    mempty

-- | Render an editable cell (title or artist).
--
-- The cell displays text that can be clicked to edit inline.
-- When editing, an input field replaces the text.
renderEditableCell :: Text -> Text -> Lucid.Html ()
renderEditableCell fieldName trackProp =
  Lucid.td_ [class_ $ base [Tokens.p3]] $ do
    -- Editing input (shown when this cell is being edited)
    Lucid.template_ [xIf_ [i|editingRow === index && editingField === '#{fieldName}'|]] $
      Lucid.input_
        [ Lucid.type_ "text",
          xModel_ "editValue",
          xOn_ "blur" "if (!isTabbing) saveEdit()",
          xOn_ "keydown.enter" "saveEdit()",
          xOn_ "keydown.escape" "cancelEdit()",
          xOn_ "keydown.tab" "handleTab($event)",
          xInit_ "$el.focus(); $el.select()",
          class_ $ base ["w-full", "p-1", Tokens.border2, Tokens.infoBorder, Tokens.bgMain, Tokens.fgPrimary, "font-mono", Tokens.textSm, "outline-none"]
        ]

    -- Display text (shown when not editing this cell)
    Lucid.template_ [xIf_ [i|!(editingRow === index && editingField === '#{fieldName}')|]] $
      Lucid.span_
        [ xOnClick_ [i|startEdit(index, '#{fieldName}')|],
          class_ $ base ["cursor-pointer", Tokens.hoverBg, "px-2", "py-1", "-mx-2", "-my-1", "block", "min-h-[1.5rem]"],
          xBindClass_ [i|{ '#{Tokens.fgMuted} italic': !track.#{trackProp} }|],
          xText_ [i|track.#{trackProp} || '(click to add)'|]
        ]
        mempty

-- | Render the delete button cell.
renderDeleteCell :: Lucid.Html ()
renderDeleteCell =
  Lucid.td_ [class_ $ base [Tokens.p3, "text-center"]] $
    Button.render
      (Button.defaults & Button.danger & Button.small & Button.with [xOnClick_ "removeTrack(index)"])
      "×"

--------------------------------------------------------------------------------
-- Add Row

-- | Render the add track row with a + icon.
renderAddRow :: Lucid.Html ()
renderAddRow =
  Lucid.tr_
    [ xOnClick_ "addTrack()",
      class_ $ base [Tokens.bgAlt, Tokens.hoverBg, "cursor-pointer"]
    ]
    $ do
      -- Empty drag handle cell
      Lucid.td_ [class_ $ base ["p-2"]] mempty
      -- + icon in track number column
      Lucid.td_ [class_ $ base ["p-2", "text-center", Tokens.successText, Tokens.fontBold, "text-lg"]] "+"
      -- Title cell with placeholder text
      Lucid.td_ [class_ $ base ["p-2", Tokens.fgMuted, "italic"]] "(add track)"
      -- Empty artist cell
      Lucid.td_ [class_ $ base ["p-2"]] mempty
      -- Empty delete cell
      Lucid.td_ [class_ $ base ["p-2"]] mempty

--------------------------------------------------------------------------------
-- Bulk Edit

-- | Render the button that opens the text view.
renderBulkButton :: Lucid.Html ()
renderBulkButton =
  Lucid.div_ [class_ $ base [Tokens.mt2]] $
    Button.render
      (Button.defaults & Button.secondary & Button.small & Button.with [xOnClick_ "openBulk()"])
      "BULK EDIT"

-- | Render the text view: one @Artist - Title@ line for each track.
--
-- The textarea has no @name@, so the form does not post it. The hidden JSON
-- field carries the parsed text.
renderBulkEditor :: Lucid.Html ()
renderBulkEditor =
  Lucid.div_ [xShow_ "bulkOpen", xCloak_] $ do
    Lucid.p_
      [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb2]]
      "One track per line: Artist - Title. APPLY replaces the full list."
    Input.textarea
      [ xModel_ "bulkText",
        Lucid.rows_ "12",
        Lucid.placeholder_ "Artist - Title"
      ]
      mempty
    Lucid.div_ [class_ $ base ["flex", Tokens.gap3, Tokens.mt2]] $ do
      Button.render
        (Button.defaults & Button.primary & Button.small & Button.with [xOnClick_ "applyBulk()"])
        "APPLY"
      Button.render
        (Button.defaults & Button.secondary & Button.small & Button.with [xOnClick_ "cancelBulk()"])
        "CANCEL"

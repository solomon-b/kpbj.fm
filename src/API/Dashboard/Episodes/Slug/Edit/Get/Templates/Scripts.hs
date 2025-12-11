{-# LANGUAGE QuasiQuotes #-}

module API.Dashboard.Episodes.Slug.Edit.Get.Templates.Scripts
  ( scripts,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Lucid qualified

--------------------------------------------------------------------------------

-- | JavaScript for episode edit form track management
scripts :: Lucid.Html ()
scripts = do
  Lucid.script_
    [i|
    // Track management for episode edit form
    (function() {
      let trackCount = document.querySelectorAll('.track-item').length;

      // Function to create a new empty track editor
      function createTrackEditor(index) {
        const trackDiv = document.createElement('div');
        trackDiv.className = 'border-2 border-gray-300 p-4 track-item';
        trackDiv.innerHTML = `
          <div class="flex justify-between items-center mb-3">
            <h3 class="font-bold">Track \#$${index + 1}</h3>
            <button type="button" class="text-red-600 font-bold hover:text-red-800 remove-track-btn">X REMOVE</button>
          </div>

          <div class="grid grid-cols-2 gap-4">
            <div>
              <label class="block font-bold mb-1 text-sm">Track \#</label>
              <input
                type="number"
                name="tracks[$${index}][track_number]"
                value="${index + 1}"
                min="1"
                class="w-full p-2 border border-gray-400 font-mono text-sm"
              />
            </div>

            <div class="col-span-2">
              <label class="block font-bold mb-1 text-sm">Title *</label>
              <input
                type="text"
                name="tracks[$${index}][title]"
                required
                minlength="1"
                maxlength="200"
                class="w-full p-2 border border-gray-400 font-mono text-sm"
              />
            </div>

            <div class="col-span-2">
              <label class="block font-bold mb-1 text-sm">Artist *</label>
              <input
                type="text"
                name="tracks[$${index}][artist]"
                required
                minlength="1"
                maxlength="200"
                class="w-full p-2 border border-gray-400 font-mono text-sm"
              />
            </div>
          </div>
        `;
        return trackDiv;
      }

      // Add track button handler
      const addTrackBtn = document.getElementById('add-track-btn');
      if (addTrackBtn) {
        addTrackBtn.addEventListener('click', function() {
          const container = document.getElementById('tracks-container');
          const newTrack = createTrackEditor(trackCount);
          container.appendChild(newTrack);
          trackCount++;

          // Attach remove handler to the new track
          attachRemoveHandler(newTrack.querySelector('.remove-track-btn'));
        });
      }

      // Remove track handler
      function attachRemoveHandler(btn) {
        btn.addEventListener('click', function() {
          if (confirm('Remove this track?')) {
            btn.closest('.track-item').remove();
            renumberTracks();
          }
        });
      }

      // Attach remove handlers to existing tracks
      document.querySelectorAll('.remove-track-btn').forEach(attachRemoveHandler);

      // Renumber tracks after removal
      function renumberTracks() {
        const tracks = document.querySelectorAll('.track-item');
        tracks.forEach((track, index) => {
          const heading = track.querySelector('h3');
          if (heading) {
            heading.textContent = 'Track \#' + (index + 1);
          }
        });
        trackCount = tracks.length;
      }
    })();
  |]

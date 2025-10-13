{-# LANGUAGE QuasiQuotes #-}

module API.Episodes.Upload.Get.Templates.Scripts
  ( renderEpisodeUploadScripts,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Lucid qualified

--------------------------------------------------------------------------------

-- | Render all JavaScript for episode upload form (validation + track management)
renderEpisodeUploadScripts :: Lucid.Html ()
renderEpisodeUploadScripts = do
  renderValidationScript
  renderTrackManagementScript

-- | Render Alpine.js validation script for episode upload form
renderValidationScript :: Lucid.Html ()
renderValidationScript = do
  Lucid.script_
    [i|
// Alpine.js validation handler for episode upload form
function episodeUploadValidator() {
  // Validation rule definitions
  const validators = {
    title: (value) => {
      const trimmed = value.trim();
      if (!trimmed) return 'Episode title is required';
      if (trimmed.length < 3) return 'Title must be at least 3 characters';
      if (trimmed.length > 200) return 'Title must be less than 200 characters';
      return null;
    },

    description: (value) => {
      const trimmed = value.trim();
      if (!trimmed) return 'Episode description is required';
      if (trimmed.length < 10) return 'Description must be at least 10 characters';
      return null;
    },

    audio_file: (fileInput) => {
      const file = fileInput?.files[0];
      if (!file) return 'Audio file is required';

      const validTypes = ['audio/mpeg', 'audio/mp3', 'audio/wav', 'audio/flac', 'audio/aac', 'audio/ogg', 'audio/x-m4a'];
      const validExtensions = /\\.(mp3|wav|flac|aac|ogg|m4a)$/i;

      if (!validTypes.includes(file.type) && !validExtensions.test(file.name)) {
        return 'Audio file must be MP3, WAV, FLAC, AAC, OGG, or M4A format';
      }

      const maxSize = 524288000; // 500MB
      if (file.size > maxSize) {
        return `Audio file must be less than 500MB (current: ${(file.size / 1024 / 1024).toFixed(2)}MB)`;
      }

      return null;
    },

    artwork_file: (fileInput) => {
      const file = fileInput?.files[0];
      if (!file) return null; // Artwork is optional

      const validTypes = ['image/jpeg', 'image/jpg', 'image/png'];
      const validExtensions = /\\.(jpg|jpeg|png)$/i;

      if (!validTypes.includes(file.type) && !validExtensions.test(file.name)) {
        return 'Artwork must be JPG or PNG format';
      }

      const maxSize = 5242880; // 5MB
      if (file.size > maxSize) {
        return `Artwork must be less than 5MB (current: ${(file.size / 1024 / 1024).toFixed(2)}MB)`;
      }

      return null;
    }
  };

  return {
    fields: {
      episode_type: { value: 'pre-recorded', isValid: true, error: '' },
      title: { value: '', isValid: false, error: '' },
      description: { value: '', isValid: false, error: '' },
      audio_file: { fileName: '', fileSize: 0, isValid: false, error: '' },
      artwork_file: { fileName: '', fileSize: 0, isValid: true, error: '' }
    },
    isSubmitting: false,
    attemptedSubmit: false,
    showErrorSummary: false,
    tracksValid: true,
    tracksError: '',

    // Generic field validator
    validateField(fieldName, getValue) {
      const field = this.fields[fieldName];
      const error = validators[fieldName](getValue());
      field.isValid = error === null;
      field.error = error || '';
      return field.isValid;
    },

    validateTitle() {
      return this.validateField('title', () => this.fields.title.value);
    },

    validateDescription() {
      return this.validateField('description', () => this.fields.description.value);
    },

    validateAudioFile() {
      const field = this.fields.audio_file;
      const fileInput = document.getElementById('main-file');
      const file = fileInput?.files[0];

      const error = validators.audio_file(fileInput);
      field.isValid = error === null;
      field.error = error || '';
      field.fileName = file?.name || '';
      field.fileSize = file?.size || 0;

      return field.isValid;
    },

    validateArtworkFile() {
      const field = this.fields.artwork_file;
      const fileInput = document.getElementById('episode-image');
      const file = fileInput?.files[0];

      const error = validators.artwork_file(fileInput);
      field.isValid = error === null;
      field.error = error || '';
      field.fileName = file?.name || '';
      field.fileSize = file?.size || 0;

      return field.isValid;
    },

    validateTracks() {
      const tracksJson = document.getElementById('tracks-json');
      if (!tracksJson) {
        this.tracksValid = true;
        this.tracksError = '';
        return true;
      }

      let tracks;
      try {
        tracks = JSON.parse(tracksJson.value || '[]');
      } catch {
        this.tracksValid = false;
        this.tracksError = 'Invalid track data format';
        return false;
      }

      if (tracks.length === 0) {
        this.tracksValid = true;
        this.tracksError = '';
        return true;
      }

      // Validate each track
      const trackError = tracks.find((track, i) => {
        if (!track.tiTitle?.trim()) return `Track ${i + 1}: Title is required`;
        if (!track.tiArtist?.trim()) return `Track ${i + 1}: Artist is required`;
        if (track.tiDuration?.trim() && !/^\\d{1,2}:\\d{2}$/.test(track.tiDuration.trim())) {
          return `Track ${i + 1}: Duration must be in MM:SS format (e.g., 4:23)`;
        }
        return null;
      });

      this.tracksValid = !trackError;
      this.tracksError = trackError || '';
      return this.tracksValid;
    },

    validateAllFields() {
      return [
        this.validateTitle(),
        this.validateDescription(),
        this.validateAudioFile(),
        this.validateArtworkFile(),
        this.validateTracks()
      ].every(Boolean);
    },

    getFirstError() {
      const fieldOrder = ['title', 'description', 'audio_file', 'artwork_file'];
      const invalidField = fieldOrder.find(name => !this.fields[name].isValid);
      return invalidField ? this.fields[invalidField].error : (this.tracksValid ? '' : this.tracksError);
    },

    handleSubmit(event) {
      this.attemptedSubmit = true;

      if (!this.validateAllFields()) {
        event.preventDefault();
        this.showErrorSummary = true;

        setTimeout(() => {
          document.querySelector('[data-error="true"]')?.scrollIntoView({
            behavior: 'smooth',
            block: 'center'
          });
        }, 100);

        return false;
      }

      this.isSubmitting = true;
      this.showErrorSummary = false;
    },

    handleAudioFileChange() {
      if (this.attemptedSubmit) {
        this.validateAudioFile();
      } else {
        const file = document.getElementById('main-file')?.files[0];
        if (file) {
          this.fields.audio_file.fileName = file.name;
          this.fields.audio_file.fileSize = file.size;
        }
      }
    },

    handleArtworkFileChange() {
      if (this.attemptedSubmit) {
        this.validateArtworkFile();
      } else {
        const file = document.getElementById('episode-image')?.files[0];
        if (file) {
          this.fields.artwork_file.fileName = file.name;
          this.fields.artwork_file.fileSize = file.size;
        }
      }
    },

    formatFileSize(bytes) {
      if (bytes === 0) return '0 Bytes';
      const sizes = ['Bytes', 'KB', 'MB', 'GB'];
      const i = Math.floor(Math.log(bytes) / Math.log(1024));
      return `${Math.round(bytes / Math.pow(1024, i) * 100) / 100} ${sizes[i]}`;
    }
  };
}
|]

-- | Render JavaScript for track management
renderTrackManagementScript :: Lucid.Html ()
renderTrackManagementScript = do
  Lucid.script_
    [i|
// Track management module (IIFE to avoid global pollution)
(function() {
  // Track HTML template
  const createTrackElement = () => {
    const div = document.createElement('div');
    div.className = 'border border-gray-300 p-4 bg-gray-50 mb-4';
    div.innerHTML = `
      <div class='grid grid-cols-1 md:grid-cols-3 gap-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Track Title</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-title' placeholder='Track title'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Artist</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-artist' placeholder='Artist name'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Album/Year</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-album' placeholder='Album (Year)'>
        </div>
      </div>
      <div class='grid grid-cols-2 md:grid-cols-4 gap-4 mt-4'>
        <div>
          <label class='block font-bold text-sm mb-1'>Duration</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-duration' placeholder='4:23'>
        </div>
        <div>
          <label class='block font-bold text-sm mb-1'>Label</label>
          <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-label' placeholder='Record label'>
        </div>
        <div class='flex items-end'>
          <label class='flex items-center text-sm'>
            <input type='checkbox' class='mr-2 track-exclusive'> Exclusive Premiere
          </label>
        </div>
        <div class='flex items-end justify-end'>
          <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700' data-action='remove-track'>
            REMOVE
          </button>
        </div>
      </div>
    `;
    return div;
  };

  // Extract track data from DOM element
  const extractTrackData = (div) => ({
    tiTitle: div.querySelector('.track-title')?.value || '',
    tiArtist: div.querySelector('.track-artist')?.value || '',
    tiAlbum: div.querySelector('.track-album')?.value || null,
    tiYear: null,
    tiDuration: div.querySelector('.track-duration')?.value || null,
    tiLabel: div.querySelector('.track-label')?.value || null,
    tiIsExclusive: div.querySelector('.track-exclusive')?.checked || false
  });

  // Update hidden JSON field with current tracks
  const updateTracksJson = () => {
    const trackDivs = document.querySelectorAll('\#tracklist-container .border:not(.border-dashed)');
    const tracks = Array.from(trackDivs).map(extractTrackData);

    const jsonField = document.getElementById('tracks-json');
    if (jsonField) {
      jsonField.value = JSON.stringify(tracks);
    }

    // Trigger Alpine validation if form submission was attempted
    const form = document.querySelector('form[x-data]');
    const alpineData = form?.__x?.$data;
    if (alpineData?.attemptedSubmit) {
      alpineData.validateTracks();
    }
  };

  // Add new track
  const addTrack = () => {
    const container = document.getElementById('tracklist-container');
    const addButton = container?.querySelector('.border-dashed');
    if (container && addButton) {
      container.insertBefore(createTrackElement(), addButton);
      updateTracksJson();
    }
  };

  // Remove track
  const removeTrack = (button) => {
    button.closest('.border')?.remove();
    updateTracksJson();
  };

  // Extract and set audio duration
  const extractAudioDuration = (file) => {
    const isAudio = file.type.startsWith('audio/') || /\\.(mp3|wav|flac|aac|ogg|m4a)$/i.test(file.name);
    if (!isAudio) return;

    const audio = new Audio();
    audio.preload = 'metadata';

    audio.onloadedmetadata = () => {
      const durationField = document.getElementById('duration-seconds');
      if (durationField) {
        durationField.value = Math.round(audio.duration);
      }
      URL.revokeObjectURL(audio.src);
    };

    audio.onerror = () => URL.revokeObjectURL(audio.src);
    audio.src = URL.createObjectURL(file);
  };

  // Initialize on DOM ready
  document.addEventListener('DOMContentLoaded', () => {
    // Add track button
    document.getElementById('add-track-btn')?.addEventListener('click', addTrack);

    // Track list updates (use event delegation)
    const container = document.getElementById('tracklist-container');
    if (container) {
      container.addEventListener('input', updateTracksJson);
      container.addEventListener('change', updateTracksJson);
      container.addEventListener('click', (e) => {
        if (e.target.dataset.action === 'remove-track') {
          removeTrack(e.target);
        }
      });
    }

    // Audio file duration extraction
    const audioInput = document.getElementById('main-file');
    audioInput?.addEventListener('change', (e) => {
      const file = e.target.files[0];
      if (file) extractAudioDuration(file);
    });
  });
})();
|]

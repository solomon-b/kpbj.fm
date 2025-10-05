{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Episodes.Upload.Get where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (episodeUploadGetLink, episodeUploadPostLink)
import App.Common (getUserInfo)
import Component.Frame (loadFrame, loadFrameWithUser)
import Control.Monad.Catch (MonadCatch)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.IO.Unlift (MonadUnliftIO)
import Control.Monad.Reader (MonadReader)
import Data.Has (Has)
import Data.String.Interpolate (i)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Effects.Database.Class (MonadDB)
import Effects.Database.Execute (execQuerySpan)
import Effects.Database.Tables.Show qualified as Show
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import Hasql.Pool qualified as HSQL.Pool
import Log qualified
import Lucid qualified
import OpenTelemetry.Trace (Tracer)
import Servant ((:>))
import Servant qualified
import Servant.Links qualified as Links
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- URL helpers
episodeUploadGetUrl :: Links.URI
episodeUploadGetUrl = Links.linkURI episodeUploadGetLink

episodeUploadPostUrl :: Links.URI
episodeUploadPostUrl = Links.linkURI episodeUploadPostLink

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "GET /episodes/upload"
    ( "episodes"
        :> "upload"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Get '[HTML] (Lucid.Html ())
    )

--------------------------------------------------------------------------------

-- | Render a show option for the dropdown
renderShowOption :: Show.ShowModel -> Bool -> Lucid.Html ()
renderShowOption s isSelected = do
  Lucid.option_
    [ Lucid.value_ (Text.pack $ Prelude.show $ s.id),
      if isSelected then Lucid.selected_ "selected" else mempty
    ]
    $ Lucid.toHtml s.title

-- | Render the episode upload form
episodeUploadForm :: [Show.ShowModel] -> Lucid.Html ()
episodeUploadForm userShows = do
  Lucid.main_ [Lucid.class_ "flex-grow px-4 py-8 max-w-4xl mx-auto w-full"] $ do
    -- Form Header
    Lucid.section_ [Lucid.class_ "bg-gray-800 text-white p-6 mb-8"] $ do
      Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
        Lucid.div_ $ do
          Lucid.h1_ [Lucid.class_ "text-2xl font-bold mb-2"] "UPLOAD EPISODE"
          Lucid.div_ [Lucid.class_ "text-gray-300 text-sm"] "Create a new episode for one of your shows"
        Lucid.div_ [Lucid.class_ "text-center"] $ do
          Lucid.div_ [Lucid.class_ "w-16 h-16 bg-gray-300 mx-auto mb-2 flex items-center justify-center border-2 border-gray-600"] $ do
            Lucid.span_ [Lucid.class_ "text-2xl"] "🎵"

    -- Episode Upload Form
    Lucid.form_
      [ Lucid.method_ "post",
        Lucid.action_ [i|/#{episodeUploadPostUrl}|],
        Lucid.enctype_ "multipart/form-data",
        Lucid.class_ "space-y-8"
      ]
      $ do
        -- Show Selection
        Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "SELECT SHOW"
          if null userShows
            then do
              Lucid.div_ [Lucid.class_ "bg-yellow-100 border border-yellow-400 p-4 mb-4"] $ do
                Lucid.p_ [Lucid.class_ "text-yellow-800"] "You are not currently assigned as a host to any shows. Contact station management to get show host access."
              Lucid.div_ [Lucid.class_ "flex justify-end"] $ do
                Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"] "Back to Dashboard"
            else do
              Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-2 gap-6"] $ do
                Lucid.div_ $ do
                  Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Show *"
                  Lucid.select_ [Lucid.name_ "show_id", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white", Lucid.required_ "required"] $ do
                    Lucid.option_ [Lucid.value_ ""] "-- Select Show --"
                    mapM_ (`renderShowOption` False) userShows

                Lucid.div_ $ do
                  Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Type *"
                  Lucid.select_ [Lucid.name_ "episode_type", Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono bg-white", Lucid.required_ "required"] $ do
                    Lucid.option_ [Lucid.value_ "pre-recorded", Lucid.selected_ "selected"] "Pre-recorded"
                    Lucid.option_ [Lucid.value_ "live"] "Live Show"
                    Lucid.option_ [Lucid.value_ "hybrid"] "Hybrid (Live + Pre-recorded segments)"

              -- Episode Details
              Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mt-8"] $ do
                Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "EPISODE DETAILS"

                Lucid.div_ [Lucid.class_ "space-y-6"] $ do
                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Title *"
                    Lucid.input_
                      [ Lucid.type_ "text",
                        Lucid.name_ "title",
                        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                        Lucid.placeholder_ "e.g. Industrial Depths #088",
                        Lucid.required_ "required"
                      ]

                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Description *"
                    Lucid.textarea_
                      [ Lucid.name_ "description",
                        Lucid.rows_ "4",
                        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                        Lucid.placeholder_ "Describe what listeners can expect from this episode...",
                        Lucid.required_ "required"
                      ]
                      mempty

                  Lucid.div_ [Lucid.class_ "grid grid-cols-1 md:grid-cols-3 gap-6"] $ do
                    Lucid.div_ $ do
                      Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Number"
                      Lucid.input_
                        [ Lucid.type_ "number",
                          Lucid.name_ "episode_number",
                          Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                          Lucid.placeholder_ "88",
                          Lucid.min_ "1"
                        ]

                    Lucid.div_ $ do
                      Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Season"
                      Lucid.input_
                        [ Lucid.type_ "number",
                          Lucid.name_ "season_number",
                          Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                          Lucid.placeholder_ "1",
                          Lucid.value_ "1",
                          Lucid.min_ "1"
                        ]

                    Lucid.div_ $ do
                      Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Scheduled Date"
                      Lucid.input_
                        [ Lucid.type_ "date",
                          Lucid.name_ "scheduled_date",
                          Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono"
                        ]

                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Tags"
                    Lucid.input_
                      [ Lucid.type_ "text",
                        Lucid.name_ "tags",
                        Lucid.class_ "w-full p-3 border-2 border-gray-400 font-mono",
                        Lucid.placeholder_ "industrial, ambient, glitch, experimental (comma separated)"
                      ]

              -- Track Listing Section
              Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "TRACKLIST"
                Lucid.p_ [Lucid.class_ "text-sm text-gray-600 mb-4"] "Add tracks in the order they will be played during the episode."

                Lucid.div_ [Lucid.id_ "tracklist-container"] $ do
                  -- Initial empty state
                  Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-8 text-center text-gray-600"] $ do
                    Lucid.button_
                      [ Lucid.type_ "button",
                        Lucid.id_ "add-track-btn",
                        Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
                      ]
                      "+ ADD TRACK"
                    Lucid.div_ [Lucid.class_ "mt-2 text-sm"] "Click to add your first track"

                -- Hidden JSON field for track data
                Lucid.input_ [Lucid.type_ "hidden", Lucid.name_ "tracks_json", Lucid.id_ "tracks-json", Lucid.value_ "[]"]

              -- Audio Upload Section
              Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
                Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "AUDIO FILES"

                Lucid.div_ [Lucid.class_ "space-y-6"] $ do
                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Main Episode File *"
                    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-6 text-center"] $ do
                      Lucid.input_
                        [ Lucid.type_ "file",
                          Lucid.name_ "audio_file",
                          Lucid.accept_ "audio/*",
                          Lucid.class_ "hidden",
                          Lucid.id_ "main-file",
                          Lucid.required_ "required"
                        ]
                      Lucid.label_ [Lucid.for_ "main-file", Lucid.class_ "cursor-pointer"] $ do
                        Lucid.div_ [Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700 inline-block"] $ do
                          "📁 CHOOSE AUDIO FILE"
                        Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "MP3, WAV, FLAC accepted • Max 500MB"

                  Lucid.div_ $ do
                    Lucid.label_ [Lucid.class_ "block font-bold mb-2"] "Episode Image (Optional)"
                    Lucid.div_ [Lucid.class_ "border-2 border-dashed border-gray-400 p-6 text-center"] $ do
                      Lucid.input_
                        [ Lucid.type_ "file",
                          Lucid.name_ "artwork_file",
                          Lucid.accept_ "image/*",
                          Lucid.class_ "hidden",
                          Lucid.id_ "episode-image"
                        ]
                      Lucid.label_ [Lucid.for_ "episode-image", Lucid.class_ "cursor-pointer"] $ do
                        Lucid.div_ [Lucid.class_ "bg-purple-600 text-white px-6 py-3 font-bold hover:bg-purple-700 inline-block"] $ do
                          "🖼️ CHOOSE IMAGE"
                        Lucid.div_ [Lucid.class_ "mt-2 text-sm text-gray-600"] "JPG, PNG accepted • Max 5MB • Recommended: 800x800px"

              -- Form Actions
              Lucid.section_ [Lucid.class_ "bg-gray-100 border-2 border-gray-400 p-6"] $ do
                Lucid.div_ [Lucid.class_ "flex justify-end items-center"] $ do
                  Lucid.div_ [Lucid.class_ "flex gap-4"] $ do
                    Lucid.a_
                      [ Lucid.href_ "/host/dashboard",
                        Lucid.class_ "bg-gray-600 text-white px-6 py-3 font-bold hover:bg-gray-700"
                      ]
                      "CANCEL"
                    Lucid.button_
                      [ Lucid.type_ "submit",
                        Lucid.name_ "action",
                        Lucid.value_ "draft",
                        Lucid.class_ "bg-green-600 text-white px-6 py-3 font-bold hover:bg-green-700"
                      ]
                      "SAVE DRAFT"
                    Lucid.button_
                      [ Lucid.type_ "submit",
                        Lucid.name_ "action",
                        Lucid.value_ "publish",
                        Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"
                      ]
                      "PUBLISH EPISODE"

    -- JavaScript for dynamic track management
    renderTrackManagementScript

-- | Render JavaScript for track management
renderTrackManagementScript :: Lucid.Html ()
renderTrackManagementScript = do
  Lucid.script_ $
    Text.unlines
      [ "let trackCount = 0;",
        "let tracks = [];",
        "",
        "function addTrack() {",
        "  trackCount++;",
        "  const trackDiv = document.createElement('div');",
        "  trackDiv.className = 'border border-gray-300 p-4 bg-gray-50 mb-4';",
        "  trackDiv.innerHTML = `",
        "    <div class='grid grid-cols-1 md:grid-cols-3 gap-4'>",
        "      <div>",
        "        <label class='block font-bold text-sm mb-1'>Track Title</label>",
        "        <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-title' placeholder='Track title'>",
        "      </div>",
        "      <div>",
        "        <label class='block font-bold text-sm mb-1'>Artist</label>",
        "        <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-artist' placeholder='Artist name'>",
        "      </div>",
        "      <div>",
        "        <label class='block font-bold text-sm mb-1'>Album/Year</label>",
        "        <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-album' placeholder='Album (Year)'>",
        "      </div>",
        "    </div>",
        "    <div class='grid grid-cols-2 md:grid-cols-4 gap-4 mt-4'>",
        "      <div>",
        "        <label class='block font-bold text-sm mb-1'>Duration</label>",
        "        <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-duration' placeholder='4:23'>",
        "      </div>",
        "      <div>",
        "        <label class='block font-bold text-sm mb-1'>Label</label>",
        "        <input type='text' class='w-full p-2 border border-gray-400 text-sm font-mono track-label' placeholder='Record label'>",
        "      </div>",
        "      <div class='flex items-end'>",
        "        <label class='flex items-center text-sm'>",
        "          <input type='checkbox' class='mr-2 track-exclusive'> Exclusive Premiere",
        "        </label>",
        "      </div>",
        "      <div class='flex items-end justify-end'>",
        "        <button type='button' class='bg-red-600 text-white px-3 py-1 text-xs font-bold hover:bg-red-700' onclick='removeTrack(this)'>",
        "          REMOVE",
        "        </button>",
        "      </div>",
        "    </div>",
        "  `;",
        "  ",
        "  const container = document.getElementById('tracklist-container');",
        "  const addButton = container.querySelector('.border-dashed');",
        "  container.insertBefore(trackDiv, addButton);",
        "  updateTracksJson();",
        "}",
        "",
        "function removeTrack(button) {",
        "  button.closest('.border').remove();",
        "  trackCount--;",
        "  updateTracksJson();",
        "}",
        "",
        "function updateTracksJson() {",
        "  const trackDivs = document.querySelectorAll('#tracklist-container .border:not(.border-dashed)');",
        "  tracks = [];",
        "  ",
        "  trackDivs.forEach((div, index) => {",
        "    const track = {",
        "      tiTitle: div.querySelector('.track-title')?.value || '',",
        "      tiArtist: div.querySelector('.track-artist')?.value || '',",
        "      tiAlbum: div.querySelector('.track-album')?.value || null,",
        "      tiYear: null, // Could parse from album field",
        "      tiDuration: div.querySelector('.track-duration')?.value || null,",
        "      tiLabel: div.querySelector('.track-label')?.value || null,",
        "      tiIsExclusive: div.querySelector('.track-exclusive')?.checked || false",
        "    };",
        "    tracks.push(track);",
        "  });",
        "  ",
        "  document.getElementById('tracks-json').value = JSON.stringify(tracks);",
        "}",
        "",
        "// Add event listener for the add track button",
        "document.addEventListener('DOMContentLoaded', function() {",
        "  document.getElementById('add-track-btn').addEventListener('click', addTrack);",
        "  ",
        "  // Update tracks JSON whenever form inputs change",
        "  document.addEventListener('input', function(e) {",
        "    if (e.target.closest('#tracklist-container')) {",
        "      updateTracksJson();",
        "    }",
        "  });",
        "  ",
        "  document.addEventListener('change', function(e) {",
        "    if (e.target.closest('#tracklist-container')) {",
        "      updateTracksJson();",
        "    }",
        "  });",
        "});"
      ]

--------------------------------------------------------------------------------

handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env
  ) =>
  Tracer ->
  Maybe Cookie ->
  m (Lucid.Html ())
handler _tracer cookie = do
  getUserInfo cookie $ \case
    Nothing -> do
      -- Redirect to login
      loadFrame $ do
        Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
          Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
          Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to upload episodes."
          Lucid.a_ [Lucid.href_ "/user/login", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Login"
    Just (user, userMetadata) -> do
      showsResult <- execQuerySpan (Show.getShowsForUser user.mId)
      case showsResult of
        Left _err -> do
          Log.logInfo "Failed to fetch user's shows" ()
          loadFrame $ do
            Lucid.div_ [Lucid.class_ "bg-white border-2 border-red-600 p-8 text-center"] $ do
              Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 text-red-600"] "Error"
              Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "Failed to load your shows."
              Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Back to Dashboard"
        Right userShows -> do
          loadFrameWithUser userMetadata (episodeUploadForm userShows)

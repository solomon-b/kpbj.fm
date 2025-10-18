{-# LANGUAGE QuasiQuotes #-}

module Component.Frame where

--------------------------------------------------------------------------------

import {-# SOURCE #-} API (aboutGetLink, archiveGetLink, blogGetLink, donateGetLink, eventsGetLink, hostDashboardGetLink, rootGetLink, userLoginGetLink, userLogoutGetLink, userRegisterGetLink)
import Control.Monad.Catch (MonadThrow)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Log qualified
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_, xData_, xModel_, xOnClick_, xOnInput_, xRef_, xText_)
import Servant.Links qualified as Link

--------------------------------------------------------------------------------

rootGetUrl :: Link.URI
rootGetUrl = Link.linkURI rootGetLink

aboutGetUrl :: Link.URI
aboutGetUrl = Link.linkURI aboutGetLink

archiveGetUrl :: Link.URI
archiveGetUrl = Link.linkURI $ archiveGetLink Nothing Nothing Nothing Nothing

donateGetUrl :: Link.URI
donateGetUrl = Link.linkURI donateGetLink

blogGetUrl :: Link.URI
blogGetUrl = Link.linkURI $ blogGetLink Nothing Nothing

eventsGetUrl :: Link.URI
eventsGetUrl = Link.linkURI $ eventsGetLink Nothing Nothing

userLoginGetUrl :: Link.URI
userLoginGetUrl = Link.linkURI $ userLoginGetLink Nothing Nothing

userRegisterGetUrl :: Link.URI
userRegisterGetUrl = Link.linkURI $ userRegisterGetLink Nothing Nothing Nothing

userLogoutGetUrl :: Link.URI
userLogoutGetUrl = Link.linkURI userLogoutGetLink

hostDashboardGetUrl :: Link.URI
hostDashboardGetUrl = Link.linkURI $ hostDashboardGetLink Nothing

--------------------------------------------------------------------------------

newtype UserInfo = UserInfo {userDisplayName :: DisplayName}

musicPlayer :: Lucid.Html ()
musicPlayer =
  Lucid.div_
    [ Lucid.class_ "px-6 text-center",
      xData_ playerData
    ]
    $ do
      Lucid.audio_
        [ xRef_ "audio",
          Lucid.preload_ "none"
        ]
        mempty
      Lucid.div_ [Lucid.class_ "inline-flex items-center gap-4 text-sm font-mono"] $ do
        Lucid.a_
          [ Lucid.href_ "#",
            Lucid.class_ "hover:text-gray-600 cursor-pointer",
            xOnClick_ "toggle()",
            xText_ "isPlaying ? '[ PAUSE ]' : '[ PLAY ]'"
          ]
          "[ PLAY ]"
        Lucid.span_ [Lucid.class_ "text-gray-500"] "|"
        Lucid.div_ [xText_ "currentShow || 'NOW PLAYING: KPBJ 95.9 FM'"] "NOW PLAYING: KPBJ 95.9 FM"
        Lucid.span_ [Lucid.class_ "text-gray-500"] "|"
        Lucid.div_ [Lucid.class_ "flex items-center gap-2"] $ do
          Lucid.span_ "VOL:"
          Lucid.input_
            [ Lucid.type_ "range",
              Lucid.min_ "0",
              Lucid.max_ "100",
              xModel_ "volume",
              xOnInput_ "setVolume($event.target.value)",
              Lucid.class_ "w-20 h-1"
            ]
  where
    playerData =
      [i|{
      playerId: 'navbar-player',
      isPlaying: false,
      volume: 80,
      streamUrl: 'https://kpbj.hasnoskills.com:8000/radio.mp3',
      metadataUrl: 'https://kpbj.hasnoskills.com:8000/status-json.xsl',

      // Metadata
      currentShow: '',
      currentTrack: '',
      currentArtist: '',
      errorMessage: '',
      metadataInterval: null,

      init() {
        this.setVolume(this.volume);
        this.fetchMetadata(); // Initial fetch
      },

      toggle() {
        this.isPlaying ? this.pause() : this.play();
      },

      play() {
        console.log('Navbar player: Starting...');

        // Pause all other players
        pauseOtherPlayers(this.playerId);

        const audio = this.$refs.audio;

        // Load source if needed
        if (!audio.src || audio.src === '') {
          audio.src = this.streamUrl;
        }

        audio.volume = this.volume / 100;

        audio.play()
          .then(() => {
            console.log('Navbar player: Playing');
            this.isPlaying = true;
            this.startMetadataPolling();
          })
          .catch((error) => {
            console.error('Navbar player failed:', error);
            this.errorMessage = 'Failed to start stream: ' + error.message;
            setTimeout(() => this.errorMessage = '', 5000);
          });
      },

      pause() {
        console.log('Navbar player: Pausing');
        const audio = this.$refs.audio;
        audio.pause();
        this.isPlaying = false;
      },

      setVolume(value) {
        this.volume = value;
        const audio = this.$refs.audio;
        if (audio) {
          audio.volume = value / 100;
        }
      },

      async fetchMetadata() {
        try {
          // Try multiple approaches to get stream metadata
          const response = await fetch(this.streamUrl, {
            method: 'HEAD',
            headers: {
              'Icy-MetaData': '1'
            }
          });

          const icyTitle = response.headers.get('icy-name') || response.headers.get('icy-description');
          const icyGenre = response.headers.get('icy-genre');

          console.log(icyTitle);

          if (icyTitle) {
            this.currentShow = icyTitle;
            this.errorMessage = '';
            console.log('Fetched metadata:', icyTitle);
          } else {
            // Fallback: parse common metadata formats
            this.parseStreamTitle('KPBJ 95.9 FM - Community Radio');
          }
        } catch (error) {
          console.log('Metadata fetch failed, using default');
          this.currentShow = 'KPBJ 95.9 FM';
          this.currentTrack = 'Live Community Radio';
        }
      },

      parseStreamTitle(title) {
        if (!title) return;

        // Common patterns: "Artist - Track", "Show: Artist - Track", etc.
        if (title.includes(' - ')) {
          const parts = title.split(' - ');
          if (parts.length >= 2) {
            this.currentArtist = parts[0].trim();
            this.currentTrack = parts.slice(1).join(' - ').trim();
          }
        } else if (title.includes(':')) {
          const parts = title.split(':');
          this.currentShow = parts[0].trim();
          if (parts[1]) {
            this.parseStreamTitle(parts[1].trim());
          }
        } else {
          this.currentTrack = title.trim();
        }
      },

      startMetadataPolling() {
        // Poll for metadata every 30 seconds
        this.metadataInterval = setInterval(() => {
          this.fetchMetadata();
        }, 30000);
      },

      stopMetadataPolling() {
        if (this.metadataInterval) {
          clearInterval(this.metadataInterval);
          this.metadataInterval = null;
        }
      }
    }|]

-- | JavaScript function to coordinate multiple independent audio players
playerScript :: Text
playerScript =
  [i|
    // Global state to track which player is currently active
    window.currentActivePlayer = null;

    // Pause all other players when a new one starts
    function pauseOtherPlayers(currentPlayerId) {
      // Get all Alpine components on the page that have a pause method
      document.querySelectorAll('[x-data]').forEach(el => {
        const data = Alpine.$data(el);
        if (data && data.playerId !== currentPlayerId && data.isPlaying && typeof data.pause === 'function') {
          data.pause();
        }
      });
    }
  |]

authWidget :: Maybe UserMetadata.Model -> Lucid.Html ()
authWidget mUser =
  Lucid.div_ [Lucid.class_ "flex gap-4 items-center text-sm text-gray-600"] $ do
    case mUser of
      Nothing -> do
        Lucid.a_ [Lucid.href_ [i|/#{userLoginGetUrl}|], hxGet_ [i|/#{userLoginGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Login"
        Lucid.a_ [Lucid.href_ [i|/#{userRegisterGetUrl}|], hxGet_ [i|/#{userRegisterGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "hover:text-gray-800"] "Sign Up"
      Just user -> do
        Lucid.span_ [Lucid.class_ "text-gray-400"] "•"
        Lucid.span_ [Lucid.class_ "text-gray-800 font-bold"] ("Welcome, " <> Lucid.toHtml user.mDisplayName)
        Lucid.a_ [Lucid.href_ [i|/#{hostDashboardGetUrl}|], hxGet_ [i|/#{hostDashboardGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "text-blue-600 hover:text-blue-800 font-bold"] "Dashboard"
        Lucid.a_ [Lucid.href_ [i|/#{userLogoutGetUrl}|], Lucid.class_ "hover:text-gray-800", hxGet_ [i|/#{userLogoutGetUrl}|]] "Logout"

logo :: Lucid.Html ()
logo =
  Lucid.a_ [Lucid.href_ [i|/#{rootGetUrl}|], hxGet_ [i|/#{rootGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "text-lg font-bold text-center whitespace-pre leading-none block hover:text-gray-600"] $ do
    Lucid.pre_ [Lucid.style_ "margin: 0;"] $ do
      "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
      "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
      "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
      "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
      "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"

navigation :: Lucid.Html ()
navigation =
  Lucid.nav_ [Lucid.class_ "flex gap-8 items-center flex-wrap"] $ do
    Lucid.a_ [Lucid.id_ "nav-donate", Lucid.href_ [i|/#{donateGetUrl}|], hxGet_ [i|/#{donateGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "Donate"
    -- Lucid.a_ [Lucid.id_ "nav-list", Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Listen"
    Lucid.a_ [Lucid.id_ "nav-shows", Lucid.href_ "/shows", hxGet_ "/shows", hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "Shows"
    Lucid.a_ [Lucid.id_ "nav-archive", Lucid.href_ [i|/#{archiveGetUrl}|], hxGet_ [i|/#{archiveGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "Archive"
    Lucid.a_ [Lucid.id_ "nav-blog", Lucid.href_ [i|/#{blogGetUrl}|], hxGet_ [i|/#{blogGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "Blog"
    Lucid.a_ [Lucid.id_ "nav-events", Lucid.href_ [i|/#{eventsGetUrl}|], hxGet_ [i|/#{eventsGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "Events"
    -- Lucid.a_ [Lucid.id_ "nav-store", Lucid.href_ "/", Lucid.class_ "font-bold uppercase hover:underline"] "Store"
    Lucid.a_ [Lucid.id_ "nav-about", Lucid.href_ [i|/#{aboutGetUrl}|], hxGet_ [i|/#{aboutGetUrl}|], hxTarget_ "#main-content", hxPushUrl_ "true", Lucid.class_ "font-bold uppercase hover:underline"] "About"
    Lucid.a_ [Lucid.id_ "nav-contact", Lucid.href_ "mailto:contact@kpbj.fm", Lucid.class_ "font-bold uppercase hover:underline"] "Contact"

template :: Maybe UserMetadata.Model -> Lucid.Html () -> Lucid.Html ()
template mUser main =
  Lucid.doctypehtml_ $ do
    Lucid.head_ $ do
      Lucid.title_ "KPBJ 95.9FM"
      Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      Lucid.script_ [Lucid.src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      Lucid.script_ [Lucid.src_ "//unpkg.com/alpinejs", Lucid.defer_ "true"] (mempty @Text)
      Lucid.script_ [] ("tailwind.config = { theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }" :: Text)
      Lucid.script_ [] playerScript
    Lucid.body_ [Lucid.class_ "font-mono text-gray-800 min-h-screen flex flex-col"] $ do
      -- Persistent header with navigation
      Lucid.header_ [Lucid.class_ "bg-white p-4"] $ do
        Lucid.div_ [Lucid.class_ "max-w-6xl mx-auto flex flex-col items-center gap-4"] $ do
          logo
          musicPlayer
          navigation
          authWidget mUser

      Lucid.main_ [Lucid.class_ "flex-grow px-4 py-8 max-w-6xl mx-auto w-full flex flex-col items-center", Lucid.id_ "main-content"] main
      Lucid.footer_ [Lucid.class_ "px-4 py-8 mt-auto text-center"] $ do
        Lucid.p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

-- footer_ [class_ "bg-gray-800 text-white px-4 py-8 mt-auto"] $ do
--   div_ [class_ "max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8"] $ do
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Listen"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Live Stream"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Show Schedule"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Episode Archive"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Mobile Apps"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Community"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Get A Show"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Volunteer"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Events"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Blog"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Support"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Donate"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Merch Store"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Become a Sponsor"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Membership"
--     div_ $ do
--       h4_ [class_ "font-bold uppercase mb-4"] "Connect"
--       ul_ [class_ "space-y-2"] $ do
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Contact Us"
--         li_ $ a_ [href_ "mailto:contact@kpbj.fm", class_ "text-gray-300 hover:text-white hover:underline"] "Email"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Instagram"
--         li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Newsletter"
--   div_ [class_ "text-center mt-8 pt-4 border-t border-gray-600 text-gray-400"] $ do
--     p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

--------------------------------------------------------------------------------

loadFrame :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadFrame = pure . template Nothing

loadFrameWithUser :: (Log.MonadLog m, MonadThrow m) => UserMetadata.Model -> Lucid.Html () -> m (Lucid.Html ())
loadFrameWithUser user = pure . template (Just user)

-- | Load content-only for HTMX responses
loadContentOnly :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadContentOnly = pure

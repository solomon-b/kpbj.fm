module Component.Frame where

--------------------------------------------------------------------------------

import Control.Monad.Catch (MonadThrow)
import Data.Text (Text)
import Domain.Types.DisplayName (DisplayName)
import Log qualified
import Lucid (a_, body_, button_, class_, defer_, div_, doctypehtml_, footer_, h3_, h4_, head_, header_, href_, input_, li_, link_, main_, nav_, p_, pre_, rel_, script_, span_, src_, style_, title_, type_, ul_)
import Lucid qualified
import Lucid.Extras (hxGet_)

--------------------------------------------------------------------------------

newtype UserInfo = UserInfo {userDisplayName :: DisplayName}

musicPlayer :: Lucid.Html ()
musicPlayer =
  div_ [class_ "bg-gray-800 text-white p-4 sticky top-0 z-40 border-b-2 border-black"] $ do
    div_ [class_ "max-w-6xl mx-auto flex items-center gap-4 md:flex-row flex-col"] $ do
      div_ [class_ "flex items-center gap-2"] $ do
        button_ [class_ "bg-white text-gray-800 px-4 py-2 font-bold cursor-pointer hover:bg-gray-200"] "▶ LIVE"
      div_ [class_ "flex-grow text-center"] $ do
        h3_ [class_ "mb-1 font-bold"] "Now Playing: The Midnight Frequency"
        p_ [class_ "text-sm text-gray-300"] "Host: DJ Nyx • 95.9 FM Shadow Hills • Live Stream"
      div_ [class_ "flex items-center gap-2"] $ do
        span_ [class_ "text-sm"] "Vol:"
        input_ [type_ "range", Lucid.min_ "0", Lucid.max_ "100", Lucid.value_ "80", class_ "w-20"]

template :: Maybe UserInfo -> Lucid.Html () -> Lucid.Html ()
template mUser main =
  doctypehtml_ $ do
    head_ $ do
      title_ "KPBJ 95.9FM"
      link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.7.1/css/all.min.css"]
      script_ [src_ "https://cdn.tailwindcss.com"] (mempty @Text)
      script_ [src_ "https://unpkg.com/htmx.org@2.0.0"] (mempty @Text)
      script_ [src_ "https://cdn.jsdelivr.net/npm/flowbite@2.5.2/dist/flowbite.min.js"] (mempty @Text)
      script_ [src_ "//unpkg.com/alpinejs", defer_ "true"] (mempty @Text)
      script_ [] ("tailwind.config = { theme: { extend: { fontFamily: { mono: ['Courier New', 'monospace'] } } } }" :: Text)
    body_ [class_ "font-mono bg-gray-50 text-gray-800 min-h-screen flex flex-col"] $ do
      header_ [class_ "bg-white border-b-2 border-gray-800 p-4"] $ do
        div_ [class_ "max-w-6xl mx-auto flex flex-col items-center gap-4"] $ do
          a_ [href_ "/", class_ "text-lg font-bold text-center whitespace-pre leading-none block hover:text-gray-600"] $ do
            pre_ [style_ "margin: 0;"] $ do
              "▄ •▄  ▄▄▄·▄▄▄▄·  ▐▄▄▄    ·▄▄▄• ▌ ▄ ·.\n"
              "█▌▄▌▪▐█ ▄█▐█ ▀█▪  ·██    ▐▄▄··██ ▐███▪\n"
              "▐▀▀▄· ██▀·▐█▀▀█▄▪▄ ██    ██▪ ▐█ ▌▐▌▐█·\n"
              "▐█.█▌▐█▪·•██▄▪▐█▐▌▐█▌    ██▌.██ ██▌▐█▌\n"
              "·▀  ▀.▀   ·▀▀▀▀  ▀▀▀•    ▀▀▀ ▀▀  █▪▀▀▀"
          nav_ [class_ "flex gap-8 items-center flex-wrap"] $ do
            a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Donate"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Listen"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Shows"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Archive"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Blog"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Events"
            -- a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "Store"
            a_ [href_ "/", class_ "font-bold uppercase hover:underline"] "About"
            a_ [href_ "mailto:contact@kpbj.fm", class_ "font-bold uppercase hover:underline"] "Contact"
          div_ [class_ "flex gap-4 items-center text-sm text-gray-600"] $ do
            a_ [href_ "/", class_ "hover:text-gray-800"] "🔍 Search"
            case mUser of
              Nothing -> do
                a_ [href_ "/user/login", class_ "hover:text-gray-800"] "Login"
                a_ [href_ "/user/register", class_ "hover:text-gray-800"] "Sign Up"
              Just user -> do
                span_ [class_ "text-gray-400"] "•"
                span_ [class_ "text-gray-800 font-bold"] ("Welcome, " <> Lucid.toHtml (userDisplayName user))
                a_ [href_ "/", class_ "text-blue-600 hover:text-blue-800 font-bold"] "Dashboard"
                a_ [href_ "/user/logout", class_ "hover:text-gray-800", hxGet_ "/user/logout"] "Logout"
      -- musicPlayer
      main_ [class_ "flex-grow px-4 py-8 max-w-6xl mx-auto w-full"] main
      footer_ [class_ "bg-gray-800 text-white px-4 py-8 mt-auto"] $ do
        div_ [class_ "max-w-6xl mx-auto grid grid-cols-1 md:grid-cols-2 lg:grid-cols-4 gap-8"] $ do
          div_ $ do
            h4_ [class_ "font-bold uppercase mb-4"] "Listen"
            ul_ [class_ "space-y-2"] $ do
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Live Stream"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Show Schedule"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Episode Archive"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Mobile Apps"
          div_ $ do
            h4_ [class_ "font-bold uppercase mb-4"] "Community"
            ul_ [class_ "space-y-2"] $ do
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Get A Show"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Volunteer"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Events"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Blog"
          div_ $ do
            h4_ [class_ "font-bold uppercase mb-4"] "Support"
            ul_ [class_ "space-y-2"] $ do
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Donate"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Merch Store"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Become a Sponsor"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Membership"
          div_ $ do
            h4_ [class_ "font-bold uppercase mb-4"] "Connect"
            ul_ [class_ "space-y-2"] $ do
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Contact Us"
              li_ $ a_ [href_ "mailto:contact@kpbj.fm", class_ "text-gray-300 hover:text-white hover:underline"] "Email"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Instagram"
              li_ $ a_ [href_ "/", class_ "text-gray-300 hover:text-white hover:underline"] "Newsletter"
        div_ [class_ "text-center mt-8 pt-4 border-t border-gray-600 text-gray-400"] $ do
          p_ "© 2025 Sun Valley Arts and Culture, a 501(c)(3) non-profit organization"

--------------------------------------------------------------------------------

loadFrame :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
loadFrame = pure . template Nothing

loadFrameWithUser :: (Log.MonadLog m, MonadThrow m) => UserInfo -> Lucid.Html () -> m (Lucid.Html ())
loadFrameWithUser user = pure . template (Just user)

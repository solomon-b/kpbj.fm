module API.SplashPage where

--------------------------------------------------------------------------------

import Config (Environment (..))
import Lucid qualified
import Servant ((:<|>) (..), (:>))
import Servant qualified
import Servant.HTML.Lucid qualified as Lucid

--------------------------------------------------------------------------------
-- Route

data SplashPage = SplashPage

instance Lucid.ToHtml SplashPage where
  toHtml :: (Monad m) => SplashPage -> Lucid.HtmlT m ()
  toHtml SplashPage =
    Lucid.doctypehtml_ $ do
      Lucid.head_ $ do
        Lucid.title_ "kpbj.fm"
        Lucid.link_ [Lucid.rel_ "stylesheet", Lucid.type_ "text/css", Lucid.href_ "static/styles.css"]
      Lucid.body_ $ do
        Lucid.header_ $ do
          Lucid.a_
            [Lucid.href_ "https://fccdata.org/?call=kpbj&facid=&city=&state=&ccode=1&country=US"]
            (Lucid.img_ [Lucid.src_ "static/range.png"])
          Lucid.aside_ "Coming Soon"
        Lucid.div_ $ do
          Lucid.form_ [Lucid.method_ "POST", Lucid.action_ "mailing-list/signup"] $ do
            Lucid.input_ [Lucid.type_ "text", Lucid.name_ "emailAddress", Lucid.placeholder_ "email address"]
            Lucid.button_ "submit"

  toHtmlRaw :: (Monad m) => SplashPage -> Lucid.HtmlT m ()
  toHtmlRaw = Lucid.toHtml

type SplashPageAPI =
  Servant.Get '[Lucid.HTML] SplashPage :<|> "static" :> Servant.Raw

--------------------------------------------------------------------------------
-- Handler

splashPageHandler :: (Monad m) => Environment -> Servant.ServerT SplashPageAPI m
splashPageHandler = \case
  Production -> pure SplashPage :<|> Servant.serveDirectoryWebApp "/backend/static"
  Development -> pure SplashPage :<|> Servant.serveDirectoryWebApp "./backend/static"

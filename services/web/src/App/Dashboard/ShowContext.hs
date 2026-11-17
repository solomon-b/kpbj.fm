-- | Types and pure rules for the current show of the dashboard.
--
-- The current show is a preference of the user. It is never an access decision.
module App.Dashboard.ShowContext
  ( -- * Show Context
    ShowContext (..),

    -- * Show Section
    ShowSection (..),

    -- * Current Show Cookie
    currentShowCookieName,
    lookupCurrentShowSlug,

    -- * Choosing a Show
    pickCurrentShow,
  )
where

--------------------------------------------------------------------------------

import Data.List (find)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Domain.Types.Cookie (Cookie (..))
import Domain.Types.Slug (Slug (..))
import Effects.Database.Tables.Shows qualified as Shows
import Network.HTTP.Types.URI (urlDecode)
import Servant (FromHttpApiData (..), ToHttpApiData (..))
import Web.Cookie (parseCookies)

--------------------------------------------------------------------------------

-- | The relation of a dashboard page to a show.
data ShowContext = NoShow | ForShow Shows.Model

-- | A section of the dashboard that has one page for each show.
data ShowSection = SectionEpisodes | SectionPromos | SectionSettings
  deriving stock (Eq, Show, Enum, Bounded)

instance ToHttpApiData ShowSection where
  toUrlPiece :: ShowSection -> Text
  toUrlPiece = \case
    SectionEpisodes -> "episodes"
    SectionPromos -> "promos"
    SectionSettings -> "settings"

instance FromHttpApiData ShowSection where
  parseUrlPiece :: Text -> Either Text ShowSection
  parseUrlPiece = \case
    "episodes" -> Right SectionEpisodes
    "promos" -> Right SectionPromos
    "settings" -> Right SectionSettings
    other -> Left $ "Unknown dashboard section: " <> other

--------------------------------------------------------------------------------

-- | The name of the cookie that holds the slug of the current show.
currentShowCookieName :: Text
currentShowCookieName = "dashboard_show"

-- | Read the slug of the current show from the raw @Cookie@ header.
--
-- The value is percent-decoded. An empty or invalid value gives 'Nothing'.
lookupCurrentShowSlug :: Cookie -> Maybe Slug
lookupCurrentShowSlug (Cookie header) = do
  rawValue <- lookup (Text.encodeUtf8 currentShowCookieName) cookies
  decoded <- either (const Nothing) Just $ Text.decodeUtf8' (urlDecode False rawValue)
  if Text.null decoded then Nothing else Just (Slug decoded)
  where
    cookies = parseCookies (Text.encodeUtf8 header)

--------------------------------------------------------------------------------

-- | Choose the current show from the shows of a user.
--
-- Use the show that the slug names. Otherwise use the first show.
pickCurrentShow :: Maybe Slug -> [Shows.Model] -> Maybe Shows.Model
pickCurrentShow mSlug userShows = case (mSlug >>= findBySlug, userShows) of
  (Just named, _) -> Just named
  (Nothing, first : _) -> Just first
  (Nothing, []) -> Nothing
  where
    findBySlug slug = find (\s -> s.slug == slug) userShows

module API.Shows.Slug.Feed.Get.TemplatesSpec where

--------------------------------------------------------------------------------

import API.Shows.Slug.Feed.Get.Templates (template)
import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime (..), fromGregorian)
import Domain.Types.Slug (Slug (..))
import Domain.Types.StorageBackend (LocalStorageConfig (..), StorageBackend (..))
import Effects.Database.Tables.Episodes qualified as Episodes
import Effects.Database.Tables.ShowSchedule qualified as ShowSchedule
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldContain, shouldNotContain)
import Text.RSS (RSS)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "API.Shows.Slug.Feed.Get.Templates" $ do
    it "makes local media URLs absolute with the site base URL" $ do
      let output = render sampleShow [sampleEpisode]
      output `shouldContain` "url='http://localhost:4000/media/audio/episodes/2026/10/24/night-moves_2026-10-24_0b7c.mp3'"
      output `shouldContain` "<url>http://localhost:4000/media/images/logos/night-moves.png</url>"

    it "links the channel to the show page and itself to the show feed" $ do
      let output = render sampleShow [sampleEpisode]
      output `shouldContain` "<link>http://localhost:4000/shows/night-moves</link>"
      output `shouldContain` "href='http://localhost:4000/shows/night-moves/feed.xml'"

    it "links each item to the public episode page" $ do
      let output = render sampleShow [sampleEpisode]
      output `shouldContain` "<link>http://localhost:4000/shows/night-moves/episodes/12</link>"
      output `shouldContain` "<title>Episode 12</title>"

    it "gives a default description to a show with none" $ do
      let output = render sampleShow {Shows.description = Nothing} []
      output `shouldContain` "<description>Episodes of Night Moves on KPBJ 95.9FM.</description>"

    it "leaves out an episode with no audio" $ do
      let output = render sampleShow [sampleEpisode {Episodes.audioFilePath = Nothing}]
      output `shouldNotContain` "<item>"

--------------------------------------------------------------------------------
-- Fixtures

siteBaseUrl :: Text.Text
siteBaseUrl = "http://localhost:4000"

localBackend :: StorageBackend
localBackend = LocalStorage (LocalStorageConfig "/tmp/kpbj")

octoberTwentyFourth :: UTCTime
octoberTwentyFourth = UTCTime (fromGregorian 2026 10 24) (19 * 3600)

sampleShow :: Shows.Model
sampleShow =
  Shows.Show
    { Shows.id = Shows.Id 7,
      Shows.title = "Night Moves",
      Shows.slug = Slug "night-moves",
      Shows.description = Just "Late-night soul and funk.",
      Shows.logoUrl = Just "images/logos/night-moves.png",
      Shows.status = Shows.Active,
      Shows.createdAt = octoberTwentyFourth,
      Shows.updatedAt = octoberTwentyFourth,
      Shows.deletedAt = Nothing
    }

sampleEpisode :: Episodes.Model
sampleEpisode =
  Episodes.Episode
    { Episodes.id = Episodes.Id 41,
      Episodes.showId = Shows.Id 7,
      Episodes.description = Just "Two hours of late-night soul.",
      Episodes.episodeNumber = Episodes.EpisodeNumber 12,
      Episodes.audioFilePath = Just "audio/episodes/2026/10/24/night-moves_2026-10-24_0b7c.mp3",
      Episodes.audioFileSize = Just 57671680,
      Episodes.audioMimeType = Just "audio/mpeg",
      Episodes.durationSeconds = Just 7200,
      Episodes.artworkUrl = Nothing,
      Episodes.scheduleTemplateId = Just (ShowSchedule.TemplateId 3),
      Episodes.airDate = Just (fromGregorian 2026 10 24),
      Episodes.publishedAt = Just octoberTwentyFourth,
      Episodes.deletedAt = Nothing,
      Episodes.createdBy = User.Id 1,
      Episodes.createdAt = octoberTwentyFourth,
      Episodes.updatedAt = octoberTwentyFourth
    }

-- | Render the show feed the way Servant sends it, as a 'String'.
render :: Shows.Model -> [Episodes.Model] -> String
render showModel episodes =
  Text.unpack (Text.Encoding.decodeUtf8 (LBS.toStrict body))
  where
    body = Servant.mimeRender (Proxy @RSS) (template siteBaseUrl localBackend showModel episodes)

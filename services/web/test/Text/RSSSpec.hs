module Text.RSSSpec where

--------------------------------------------------------------------------------

import Data.ByteString.Lazy qualified as LBS
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Time (UTCTime (..), fromGregorian)
import Servant qualified
import Test.Hspec (Spec, describe, it, shouldBe, shouldContain, shouldNotContain)
import Text.RSS (Channel (..), Enclosure (..), Item (..), RSS, formatRfc822, renderFeed)

--------------------------------------------------------------------------------

spec :: Spec
spec =
  describe "Text.RSS" $ do
    describe "formatRfc822" $ do
      it "gives the RFC 822 form in UTC" $
        formatRfc822 octoberTwentyFourth `shouldBe` "Sat, 24 Oct 2026 19:30:00 +0000"

    describe "renderFeed" $ do
      it "declares the XML encoding, RSS 2.0, and the atom namespace" $ do
        let output = render sampleChannel []
        output `shouldContain` "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        output `shouldContain` "<rss version='2.0' xmlns:atom='http://www.w3.org/2005/Atom'>"

      it "gives the channel fields and the self link" $ do
        let output = render sampleChannel []
        output `shouldContain` "<title>KPBJ 95.9FM</title>"
        output `shouldContain` "<link>https://kpbj.fm/</link>"
        output `shouldContain` "<language>en-us</language>"
        output `shouldContain` "<lastBuildDate>Sat, 24 Oct 2026 19:30:00 +0000</lastBuildDate>"
        output `shouldContain` "<atom:link href='https://kpbj.fm/feed.xml' rel='self' type='application/rss+xml'/>"

      it "leaves out the image when the channel has none" $
        render sampleChannel [] `shouldNotContain` "<image>"

      it "gives the image with the channel title and link" $ do
        let channel = sampleChannel {channelImageUrl = Just "https://cdn.example/logo.png"}
        render channel [] `shouldContain` "<image><url>https://cdn.example/logo.png</url><title>KPBJ 95.9FM</title><link>https://kpbj.fm/</link></image>"

      it "gives each item its guid, date, and enclosure" $ do
        let output = render sampleChannel [sampleItem]
        output `shouldContain` "<guid isPermaLink='true'>https://kpbj.fm/shows/night-moves/episodes/12</guid>"
        output `shouldContain` "<pubDate>Sat, 24 Oct 2026 19:30:00 +0000</pubDate>"
        output `shouldContain` "<enclosure url='https://cdn.example/audio.mp3' length='57671680' type='audio/mpeg'/>"

      it "escapes markup in text" $ do
        let item = sampleItem {itemTitle = "Rock & Roll <Live>", itemDescription = Just "<b>bold</b> & more"}
            output = render sampleChannel [item]
        output `shouldContain` "<title>Rock &amp; Roll &lt;Live&gt;</title>"
        output `shouldContain` "&lt;b&gt;bold&lt;/b&gt; &amp; more"
        output `shouldNotContain` "<b>"

      it "escapes ampersands in URLs" $ do
        let enclosure = sampleEnclosure {enclosureUrl = "https://cdn.example/a.mp3?x=1&y=2"}
            output = render sampleChannel [sampleItem {itemEnclosure = enclosure}]
        output `shouldContain` "x=1&amp;y=2"

      it "removes characters that XML does not allow" $ do
        let item = sampleItem {itemDescription = Just "Page one\fpage two\v"}
            output = render sampleChannel [item]
        output `shouldContain` "Page onepage two"
        output `shouldNotContain` "\f"

--------------------------------------------------------------------------------
-- Fixtures

octoberTwentyFourth :: UTCTime
octoberTwentyFourth = UTCTime (fromGregorian 2026 10 24) (19 * 3600 + 30 * 60)

sampleChannel :: Channel
sampleChannel =
  Channel
    { channelTitle = "KPBJ 95.9FM",
      channelLink = "https://kpbj.fm/",
      channelDescription = "Episodes from every show on KPBJ 95.9FM.",
      channelSelfUrl = "https://kpbj.fm/feed.xml",
      channelImageUrl = Nothing,
      channelLastBuildDate = Just octoberTwentyFourth
    }

sampleEnclosure :: Enclosure
sampleEnclosure =
  Enclosure
    { enclosureUrl = "https://cdn.example/audio.mp3",
      enclosureLength = 57671680,
      enclosureType = "audio/mpeg"
    }

sampleItem :: Item
sampleItem =
  Item
    { itemTitle = "Night Moves - Episode 12",
      itemLink = "https://kpbj.fm/shows/night-moves/episodes/12",
      itemDescription = Just "Two hours of late-night soul.",
      itemPublishedAt = octoberTwentyFourth,
      itemEnclosure = sampleEnclosure
    }

-- | Render a feed the way Servant sends it, as a 'String' for the matchers.
render :: Channel -> [Item] -> String
render channel items =
  Text.unpack (decodeBody (Servant.mimeRender (Proxy @RSS) (renderFeed channel items)))
  where
    decodeBody :: LBS.ByteString -> Text
    decodeBody = Text.Encoding.decodeUtf8 . LBS.toStrict

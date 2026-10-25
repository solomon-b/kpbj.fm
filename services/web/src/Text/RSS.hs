-- | RSS 2.0 feeds: a Servant content type and a small XML renderer.
--
-- The renderer builds the document with @xmlhtml@, which escapes all text and
-- all attribute values. The feed holds plain RSS 2.0 plus one @atom:link@ that
-- gives the URL of the feed itself.
module Text.RSS
  ( -- * Content Type
    RSS,
    feedCacheControl,

    -- * Feed Data
    Channel (..),
    Item (..),
    Enclosure (..),

    -- * Rendering
    renderFeed,
    formatRfc822,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString.Builder qualified as Builder
import Data.Int (Int64)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Network.HTTP.Media qualified as Media
import Servant qualified
import Text.XmlHtml qualified as Xml

--------------------------------------------------------------------------------
-- Content Type

-- | Servant content type for RSS feeds.
--
-- A response has the type @application/rss+xml; charset=utf-8@. The type also
-- accepts @application/xml@ and @text/xml@. Some feed readers ask for only one
-- of those, and without them Servant sends a 406 to those readers.
data RSS

instance Servant.Accept RSS where
  contentTypes _ =
    ("application" Media.// "rss+xml" Media./: ("charset", "utf-8"))
      :| [ "application" Media.// "xml" Media./: ("charset", "utf-8"),
           "text" Media.// "xml" Media./: ("charset", "utf-8")
         ]

instance Servant.MimeRender RSS Xml.Document where
  mimeRender _ document = Builder.toLazyByteString (Xml.render document)

-- | The @Cache-Control@ value of a feed response.
--
-- Feed readers poll often. A cache time of 15 minutes lets a proxy or a reader
-- keep the response, and a new episode still shows up quickly.
feedCacheControl :: Text
feedCacheControl = "public, max-age=900"

--------------------------------------------------------------------------------
-- Feed Data

-- | The @channel@ element of a feed. All URLs are absolute.
data Channel = Channel
  { channelTitle :: Text,
    -- | The web page that the feed belongs to.
    channelLink :: Text,
    channelDescription :: Text,
    -- | The URL of the feed itself, for the @atom:link@ element.
    channelSelfUrl :: Text,
    -- | An image for the channel. When it is set, the feed gets an @image@
    -- element.
    channelImageUrl :: Maybe Text,
    -- | The publish date of the newest item. An empty feed has none.
    channelLastBuildDate :: Maybe UTCTime
  }
  deriving stock (Show, Eq)

-- | One @item@ of a feed. All URLs are absolute.
data Item = Item
  { itemTitle :: Text,
    -- | The public page of the item. The feed also uses it as the @guid@.
    itemLink :: Text,
    itemDescription :: Maybe Text,
    itemPublishedAt :: UTCTime,
    itemEnclosure :: Enclosure
  }
  deriving stock (Show, Eq)

-- | The media file of an item.
data Enclosure = Enclosure
  { enclosureUrl :: Text,
    -- | The size of the file in bytes.
    enclosureLength :: Int64,
    -- | The MIME type of the file, for example @audio/mpeg@.
    enclosureType :: Text
  }
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Rendering

-- | Render a channel and its items as an RSS 2.0 document.
renderFeed :: Channel -> [Item] -> Xml.Document
renderFeed channel items =
  Xml.XmlDocument Xml.UTF8 Nothing [rssElement]
  where
    rssElement =
      element
        "rss"
        [ ("version", "2.0"),
          ("xmlns:atom", "http://www.w3.org/2005/Atom")
        ]
        [channelElement]

    channelElement =
      element "channel" [] $
        [ textElement "title" channel.channelTitle,
          textElement "link" channel.channelLink,
          textElement "description" channel.channelDescription,
          textElement "language" "en-us",
          element
            "atom:link"
            [ ("href", channel.channelSelfUrl),
              ("rel", "self"),
              ("type", "application/rss+xml")
            ]
            []
        ]
          <> lastBuildDateElements
          <> imageElements
          <> map renderItem items

    lastBuildDateElements =
      case channel.channelLastBuildDate of
        Nothing -> []
        Just lastBuildDate -> [textElement "lastBuildDate" (formatRfc822 lastBuildDate)]

    -- RSS 2.0 says the title and the link of the image must be the same as
    -- those of the channel.
    imageElements =
      case channel.channelImageUrl of
        Nothing -> []
        Just imageUrl ->
          [ element
              "image"
              []
              [ textElement "url" imageUrl,
                textElement "title" channel.channelTitle,
                textElement "link" channel.channelLink
              ]
          ]

-- | Render one @item@ element.
renderItem :: Item -> Xml.Node
renderItem item =
  element "item" [] $
    [ textElement "title" item.itemTitle,
      textElement "link" item.itemLink,
      element "guid" [("isPermaLink", "true")] [textNode item.itemLink]
    ]
      <> descriptionElements
      <> [ textElement "pubDate" (formatRfc822 item.itemPublishedAt),
           element
             "enclosure"
             [ ("url", enclosure.enclosureUrl),
               ("length", Text.pack (show enclosure.enclosureLength)),
               ("type", enclosure.enclosureType)
             ]
             []
         ]
  where
    enclosure = item.itemEnclosure

    descriptionElements =
      case item.itemDescription of
        Nothing -> []
        Just description -> [textElement "description" description]

-- | Format a time in the RFC 822 form that RSS 2.0 uses for dates.
--
-- >>> formatRfc822 (UTCTime (fromGregorian 2026 10 24) 0)
-- "Sat, 24 Oct 2026 00:00:00 +0000"
formatRfc822 :: UTCTime -> Text
formatRfc822 time =
  Text.pack (formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S +0000" time)

--------------------------------------------------------------------------------
-- XML helpers

-- | An element with attributes and children. Attribute values go through
-- 'removeInvalidXmlChars'.
element :: Text -> [(Text, Text)] -> [Xml.Node] -> Xml.Node
element tag attributes children =
  Xml.Element
    { Xml.elementTag = tag,
      Xml.elementAttrs = [(name, removeInvalidXmlChars value) | (name, value) <- attributes],
      Xml.elementChildren = children
    }

-- | An element that holds only text.
textElement :: Text -> Text -> Xml.Node
textElement tag content = element tag [] [textNode content]

-- | A text node. @xmlhtml@ escapes it when it renders the document.
textNode :: Text -> Xml.Node
textNode content = Xml.TextNode (removeInvalidXmlChars content)

-- | Remove the characters that XML 1.0 does not allow anywhere in a document.
--
-- Escaping cannot make these characters valid. A host can paste one into a
-- description, for example a form feed from a word processor. One such
-- character makes a feed reader reject the full feed, so we remove it.
removeInvalidXmlChars :: Text -> Text
removeInvalidXmlChars = Text.filter isXmlChar
  where
    isXmlChar c =
      c == '\t'
        || c == '\n'
        || c == '\r'
        || (c >= '\x20' && c <= '\xD7FF')
        || (c >= '\xE000' && c <= '\xFFFD')
        || (c >= '\x10000' && c <= '\x10FFFF')

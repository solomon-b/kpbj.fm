{-# LANGUAGE QuasiQuotes #-}

module Component.Tags
  ( renderTags,
    TagLink (..),
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Extras (hxGet_, hxPushUrl_, hxTarget_)
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

-- | A tag with its display name and link URL.
data TagLink = TagLink
  { tagName :: Text,
    tagUrl :: Links.URI
  }

-- | Render tags as clickable pills with # prefix.
renderTags :: [TagLink] -> Lucid.Html ()
renderTags tags =
  Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2"] $ do
    mapM_ renderTag tags
  where
    renderTag :: TagLink -> Lucid.Html ()
    renderTag tag =
      let url = tagUrl tag
          name = tagName tag
       in Lucid.a_
            [ Lucid.href_ [i|/#{url}|],
              hxGet_ [i|/#{url}|],
              hxTarget_ "#main-content",
              hxPushUrl_ "true",
              Lucid.class_ tagStyle
            ]
            $ Lucid.toHtml
            $ "#" <> name

    tagStyle :: Text
    tagStyle = "bg-gray-200 text-gray-800 px-2 py-1 text-xs font-mono hover:bg-gray-300 cursor-pointer"

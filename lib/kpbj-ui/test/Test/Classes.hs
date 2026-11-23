-- | Reads the classes out of rendered HTML, for the safelist tests.
module Test.Classes
  ( classesOf,
    unlisted,
  )
where

--------------------------------------------------------------------------------

import Data.List (nub)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Lucid qualified

--------------------------------------------------------------------------------

-- | Every class that the HTML can show.
--
-- It reads each @class@ attribute, and each quoted string in an
-- @x-bind:class@ attribute.
classesOf :: Lucid.Html () -> [Text]
classesOf html = nub (static <> bound)
  where
    text = LText.toStrict (Lucid.renderText html)
    static = foldMap Text.words (attrValues "class" text)
    bound = foldMap (foldMap Text.words . quoted) (attrValues "x-bind:class" text)

-- | The classes of the HTML that are not in the given list.
unlisted :: [Text] -> Lucid.Html () -> [Text]
unlisted list = filter (`notElem` list) . classesOf

-- | The value of each attribute with the given name.
attrValues :: Text -> Text -> [Text]
attrValues name text =
  case Text.breakOn opening text of
    (_, "") -> []
    (_, rest) ->
      let (value, more) = Text.breakOn "\"" (Text.drop (Text.length opening) rest)
       in value : attrValues name more
  where
    opening = " " <> name <> "=\""

-- | The single-quoted strings of a JavaScript expression in an attribute.
quoted :: Text -> [Text]
quoted = odds . Text.splitOn "'" . Text.replace "&#39;" "'"
  where
    odds (_ : x : rest) = x : odds rest
    odds _ = []

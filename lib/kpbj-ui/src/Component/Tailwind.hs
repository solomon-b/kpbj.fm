{-# LANGUAGE QuasiQuotes #-}

-- | The Tailwind CDN config that every frame uses.
--
-- The config sets class-based dark mode, the monospace font stack, and the
-- safelist. In Firefox, the CDN does not write the rule for a class that
-- first appears in an HTMX swap. The safelist makes the CDN always write
-- those classes.
--
-- A component whose classes can first appear in an HTMX swap gives its own
-- @safelist@, and 'safelist' here adds it.
module Component.Tailwind
  ( safelist,
    configScript,
  )
where

--------------------------------------------------------------------------------

import Component.Button qualified as Button
import Component.Choice qualified as Choice
import Component.Field qualified as Field
import Component.Input qualified as Input
import Data.Aeson.Text (encodeToLazyText)
import Data.List (nub)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text.Lazy qualified as LText
import Lucid qualified

--------------------------------------------------------------------------------

-- | The classes that the Tailwind CDN always writes.
--
-- It holds every class of the buttons and the form controls, and the
-- classes of other elements that showed the problem. It also holds the
-- classes that place the remove button on an image thumbnail, because no
-- component gives them.
safelist :: [Text]
safelist =
  nub $
    Button.safelist
      <> Field.safelist
      <> Input.safelist
      <> Choice.safelist
      <> ["max-h-64", "absolute", "top-1", "right-1"]

-- | The @script@ element that sets @tailwind.config@.
configScript :: Lucid.Html ()
configScript =
  Lucid.script_ [] ([i|tailwind.config = { darkMode: 'class', safelist: #{safelistJson}, theme: { fontFamily: { sans: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'], mono: ['ui-monospace', 'SFMono-Regular', 'Menlo', 'Monaco', 'Consolas', 'Liberation Mono', 'Courier New', 'monospace'] } } }|] :: Text)
  where
    safelistJson :: Text
    safelistJson = LText.toStrict (encodeToLazyText safelist)

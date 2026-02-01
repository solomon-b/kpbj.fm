{-# LANGUAGE QuasiQuotes #-}

-- | Centralized theme system for KPBJ.
--
-- This module provides a terminal-color-scheme-like theming system where colors
-- are defined once and applied consistently across the entire site. Each theme
-- has both light and dark variants, and users can switch themes at runtime.
--
-- = Architecture
--
-- @
-- Theme Definition (Haskell data type)
--     |
--     v
-- CSS Custom Properties (generated per-page)
--     |
--     v
-- Semantic Tokens (Tailwind classes referencing CSS vars)
--     |
--     v
-- Templates (use tokens via Design.Tokens)
-- @
--
-- = Usage
--
-- In Frame.hs, inject the theme CSS in the @\<head\>@ section:
--
-- @
-- Lucid.style_ [] (themeCSS defaultTheme)
-- @
--
-- Then use the semantic tokens from this module (re-exported via Design.Tokens):
--
-- @
-- Lucid.div_ [Lucid.class_ Theme.bgMain] content
-- @
module Design.Theme
  ( -- * Types
    Theme (..),
    Palette (..),
    ColorValue,

    -- * Built-in Themes
    defaultTheme,
    solarizedTheme,
    gruvboxTheme,
    draculaTheme,
    nordTheme,

    -- * CSS Generation
    themeCSS,

    -- * Background Tokens
    bgMain,
    bgAlt,
    bgInverse,

    -- * Foreground Tokens
    fgPrimary,
    fgMuted,
    fgInverse,

    -- * Border Tokens
    borderDefault,
    borderMuted,

    -- * Accent Tokens
    accent,
    accentHover,
    accentFg,

    -- * Semantic Status Tokens
    success,
    error,
    warning,
    info,

    -- * Hover State Token
    hoverBg,
  )
where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Prelude hiding (error)

--------------------------------------------------------------------------------

-- | A hex color value (e.g., "#ffffff" or "#1f2937")
type ColorValue = Text

-- | A complete color palette for one mode (light or dark).
--
-- Contains 16 semantic color roles, similar to terminal color schemes.
-- Each field maps to a CSS custom property.
data Palette = Palette
  { -- Base colors (8)

    -- | Main page background
    pBg :: ColorValue,
    -- | Secondary background (cards, muted sections)
    pBgAlt :: ColorValue,
    -- | Primary text color
    pFg :: ColorValue,
    -- | Muted/secondary text color
    pFgMuted :: ColorValue,
    -- | Primary border color
    pBorder :: ColorValue,
    -- | Subtle/muted border color
    pBorderMuted :: ColorValue,
    -- | Primary accent color (buttons, links)
    pAccent :: ColorValue,
    -- | Text on accent backgrounds
    pAccentFg :: ColorValue,
    -- Semantic colors (4)

    -- | Success/green color
    pSuccess :: ColorValue,
    -- | Error/red color
    pError :: ColorValue,
    -- | Warning/yellow color
    pWarning :: ColorValue,
    -- | Info/blue color
    pInfo :: ColorValue,
    -- Inverse colors (2)

    -- | Inverse background (for dark-on-light or light-on-dark sections)
    pBgInverse :: ColorValue,
    -- | Inverse text color
    pFgInverse :: ColorValue,
    -- Hover colors (2)

    -- | Hover background color
    pHover :: ColorValue,
    -- | Accent hover state
    pAccentHover :: ColorValue
  }
  deriving (Show, Eq)

-- | A complete theme with light and dark variants.
data Theme = Theme
  { -- | Human-readable theme name
    themeName :: Text,
    -- | Light mode palette
    themeLight :: Palette,
    -- | Dark mode palette
    themeDark :: Palette
  }
  deriving (Show, Eq)

--------------------------------------------------------------------------------
-- Built-in Themes

-- | Solarized theme by Ethan Schoonover.
--
-- A precision color scheme with both light and dark modes designed for
-- readability and reduced eye strain. Uses carefully selected colors
-- with specific relationships to reduce contrast while maintaining clarity.
--
-- See: https://ethanschoonover.com/solarized/
solarizedTheme :: Theme
solarizedTheme =
  Theme
    { themeName = "solarized",
      themeLight =
        Palette
          { pBg = "#fdf6e3", -- base3 (lightest)
            pBgAlt = "#eee8d5", -- base2
            pFg = "#657b83", -- base00
            pFgMuted = "#93a1a1", -- base1
            pBorder = "#93a1a1", -- base1
            pBorderMuted = "#93a1a1", -- base1 (darker than bgAlt for visibility)
            pAccent = "#268bd2", -- blue
            pAccentFg = "#fdf6e3", -- base3
            pSuccess = "#859900", -- green
            pError = "#dc322f", -- red
            pWarning = "#b58900", -- yellow
            pInfo = "#268bd2", -- blue
            pBgInverse = "#002b36", -- base03 (darkest)
            pFgInverse = "#fdf6e3", -- base3
            pHover = "#eee8d5", -- base2
            pAccentHover = "#2aa198" -- cyan
          },
      themeDark =
        Palette
          { pBg = "#002b36", -- base03 (darkest)
            pBgAlt = "#073642", -- base02
            pFg = "#839496", -- base0
            pFgMuted = "#586e75", -- base01
            pBorder = "#586e75", -- base01
            pBorderMuted = "#586e75", -- base01 (lighter than bgAlt for visibility)
            pAccent = "#268bd2", -- blue
            pAccentFg = "#002b36", -- base03
            pSuccess = "#859900", -- green
            pError = "#dc322f", -- red
            pWarning = "#b58900", -- yellow
            pInfo = "#268bd2", -- blue
            pBgInverse = "#fdf6e3", -- base3 (lightest)
            pFgInverse = "#002b36", -- base03
            pHover = "#073642", -- base02
            pAccentHover = "#2aa198" -- cyan
          }
    }

-- | Gruvbox theme by Pavel Pertsev.
--
-- A retro groove color scheme with warm, earthy tones. Designed for
-- extended coding sessions with reduced eye strain while maintaining
-- good contrast and readability.
--
-- See: https://github.com/morhetz/gruvbox
gruvboxTheme :: Theme
gruvboxTheme =
  Theme
    { themeName = "gruvbox",
      themeLight =
        Palette
          { pBg = "#fbf1c7", -- light0
            pBgAlt = "#ebdbb2", -- light1
            pFg = "#3c3836", -- dark1
            pFgMuted = "#7c6f64", -- dark4
            pBorder = "#3c3836", -- dark1
            pBorderMuted = "#bdae93", -- light3
            pAccent = "#076678", -- blue (dark)
            pAccentFg = "#fbf1c7", -- light0
            pSuccess = "#79740e", -- green (dark)
            pError = "#9d0006", -- red (dark)
            pWarning = "#b57614", -- yellow (dark)
            pInfo = "#076678", -- blue (dark)
            pBgInverse = "#282828", -- dark0
            pFgInverse = "#fbf1c7", -- light0
            pHover = "#ebdbb2", -- light1
            pAccentHover = "#458588" -- blue (neutral)
          },
      themeDark =
        Palette
          { pBg = "#282828", -- dark0
            pBgAlt = "#3c3836", -- dark1
            pFg = "#ebdbb2", -- light1
            pFgMuted = "#a89984", -- light4
            pBorder = "#504945", -- dark2
            pBorderMuted = "#504945", -- dark2
            pAccent = "#83a598", -- blue
            pAccentFg = "#282828", -- dark0
            pSuccess = "#b8bb26", -- green
            pError = "#fb4934", -- red
            pWarning = "#fabd2f", -- yellow
            pInfo = "#83a598", -- blue
            pBgInverse = "#fbf1c7", -- light0
            pFgInverse = "#282828", -- dark0
            pHover = "#3c3836", -- dark1
            pAccentHover = "#8ec07c" -- aqua
          }
    }

-- | Dracula theme by Zeno Rocha.
--
-- A dark theme with vibrant colors inspired by Dracula. Features high
-- contrast with purple and pink accent colors on a dark background.
--
-- See: https://draculatheme.com/
draculaTheme :: Theme
draculaTheme =
  Theme
    { themeName = "dracula",
      themeLight =
        Palette
          { pBg = "#f8f8f2", -- foreground as bg for light mode
            pBgAlt = "#e6e6e6", -- slightly darker
            pFg = "#282a36", -- background as fg for light mode
            pFgMuted = "#6272a4", -- comment
            pBorder = "#282a36", -- background
            pBorderMuted = "#d6d6d6", -- muted border
            pAccent = "#bd93f9", -- purple
            pAccentFg = "#f8f8f2", -- foreground
            pSuccess = "#50fa7b", -- green
            pError = "#ff5555", -- red
            pWarning = "#f1fa8c", -- yellow
            pInfo = "#8be9fd", -- cyan
            pBgInverse = "#282a36", -- background
            pFgInverse = "#f8f8f2", -- foreground
            pHover = "#e6e6e6", -- slightly darker
            pAccentHover = "#ff79c6" -- pink
          },
      themeDark =
        Palette
          { pBg = "#282a36", -- background
            pBgAlt = "#44475a", -- current line
            pFg = "#f8f8f2", -- foreground
            pFgMuted = "#6272a4", -- comment
            pBorder = "#44475a", -- current line
            pBorderMuted = "#44475a", -- current line
            pAccent = "#bd93f9", -- purple
            pAccentFg = "#282a36", -- background
            pSuccess = "#50fa7b", -- green
            pError = "#ff5555", -- red
            pWarning = "#f1fa8c", -- yellow
            pInfo = "#8be9fd", -- cyan
            pBgInverse = "#f8f8f2", -- foreground
            pFgInverse = "#282a36", -- background
            pHover = "#44475a", -- current line
            pAccentHover = "#ff79c6" -- pink
          }
    }

-- | Nord theme by Arctic Ice Studio.
--
-- An arctic, north-bluish color palette inspired by the beauty of the
-- arctic. Designed for clean, uncluttered design patterns with a calm
-- and harmonious color scheme.
--
-- See: https://www.nordtheme.com/
nordTheme :: Theme
nordTheme =
  Theme
    { themeName = "nord",
      themeLight =
        Palette
          { pBg = "#eceff4", -- snow storm 2
            pBgAlt = "#e5e9f0", -- snow storm 1
            pFg = "#2e3440", -- polar night 0
            pFgMuted = "#4c566a", -- polar night 3
            pBorder = "#2e3440", -- polar night 0
            pBorderMuted = "#d8dee9", -- snow storm 0
            pAccent = "#5e81ac", -- frost 3
            pAccentFg = "#eceff4", -- snow storm 2
            pSuccess = "#a3be8c", -- aurora green
            pError = "#bf616a", -- aurora red
            pWarning = "#ebcb8b", -- aurora yellow
            pInfo = "#81a1c1", -- frost 2
            pBgInverse = "#2e3440", -- polar night 0
            pFgInverse = "#eceff4", -- snow storm 2
            pHover = "#e5e9f0", -- snow storm 1
            pAccentHover = "#81a1c1" -- frost 2
          },
      themeDark =
        Palette
          { pBg = "#2e3440", -- polar night 0
            pBgAlt = "#3b4252", -- polar night 1
            pFg = "#eceff4", -- snow storm 2
            pFgMuted = "#d8dee9", -- snow storm 0
            pBorder = "#4c566a", -- polar night 3
            pBorderMuted = "#4c566a", -- polar night 3
            pAccent = "#88c0d0", -- frost 1
            pAccentFg = "#2e3440", -- polar night 0
            pSuccess = "#a3be8c", -- aurora green
            pError = "#bf616a", -- aurora red
            pWarning = "#ebcb8b", -- aurora yellow
            pInfo = "#81a1c1", -- frost 2
            pBgInverse = "#eceff4", -- snow storm 2
            pFgInverse = "#2e3440", -- polar night 0
            pHover = "#3b4252", -- polar night 1
            pAccentHover = "#8fbcbb" -- frost 0
          }
    }

-- | The default KPBJ brutalist theme.
--
-- Features high contrast, bold borders, and a monochrome base palette
-- with semantic colors for status indicators.
defaultTheme :: Theme
defaultTheme =
  Theme
    { themeName = "default",
      themeLight =
        Palette
          { pBg = "#ffffff",
            pBgAlt = "#f3f4f6", -- gray-100
            pFg = "#1f2937", -- gray-800
            pFgMuted = "#6b7280", -- gray-500
            pBorder = "#1f2937", -- gray-800 (brutalist bold border)
            pBorderMuted = "#d1d5db", -- gray-300
            pAccent = "#1f2937", -- gray-800
            pAccentFg = "#ffffff",
            pSuccess = "#059669", -- green-600
            pError = "#dc2626", -- red-600
            pWarning = "#d97706", -- yellow-600
            pInfo = "#2563eb", -- blue-600
            pBgInverse = "#1f2937", -- gray-800
            pFgInverse = "#ffffff",
            pHover = "#f3f4f6", -- gray-100
            pAccentHover = "#374151" -- gray-700
          },
      themeDark =
        Palette
          { pBg = "#1f2937", -- gray-800
            pBgAlt = "#374151", -- gray-700
            pFg = "#f3f4f6", -- gray-100
            pFgMuted = "#9ca3af", -- gray-400
            pBorder = "#4b5563", -- gray-600
            pBorderMuted = "#4b5563", -- gray-600 (lighter than bgAlt for visibility)
            pAccent = "#f3f4f6", -- gray-100
            pAccentFg = "#1f2937", -- gray-800
            pSuccess = "#10b981", -- green-500
            pError = "#f87171", -- red-400
            pWarning = "#fbbf24", -- yellow-400
            pInfo = "#60a5fa", -- blue-400
            pBgInverse = "#f9fafb", -- gray-50
            pFgInverse = "#1f2937", -- gray-800
            pHover = "#374151", -- gray-700
            pAccentHover = "#e5e7eb" -- gray-200
          }
    }

--------------------------------------------------------------------------------
-- CSS Generation

-- | Generate CSS custom properties for a theme.
--
-- Outputs @:root { ... }@ for light mode and @.dark { ... }@ for dark mode.
-- This should be injected into the @\<head\>@ before Tailwind CDN.
themeCSS :: Theme -> Text
themeCSS Theme {..} =
  [i|
:root {
  --theme-bg: #{pBg themeLight};
  --theme-bg-alt: #{pBgAlt themeLight};
  --theme-fg: #{pFg themeLight};
  --theme-fg-muted: #{pFgMuted themeLight};
  --theme-border: #{pBorder themeLight};
  --theme-border-muted: #{pBorderMuted themeLight};
  --theme-accent: #{pAccent themeLight};
  --theme-accent-fg: #{pAccentFg themeLight};
  --theme-success: #{pSuccess themeLight};
  --theme-error: #{pError themeLight};
  --theme-warning: #{pWarning themeLight};
  --theme-info: #{pInfo themeLight};
  --theme-bg-inverse: #{pBgInverse themeLight};
  --theme-fg-inverse: #{pFgInverse themeLight};
  --theme-hover: #{pHover themeLight};
  --theme-accent-hover: #{pAccentHover themeLight};
}

.dark {
  --theme-bg: #{pBg themeDark};
  --theme-bg-alt: #{pBgAlt themeDark};
  --theme-fg: #{pFg themeDark};
  --theme-fg-muted: #{pFgMuted themeDark};
  --theme-border: #{pBorder themeDark};
  --theme-border-muted: #{pBorderMuted themeDark};
  --theme-accent: #{pAccent themeDark};
  --theme-accent-fg: #{pAccentFg themeDark};
  --theme-success: #{pSuccess themeDark};
  --theme-error: #{pError themeDark};
  --theme-warning: #{pWarning themeDark};
  --theme-info: #{pInfo themeDark};
  --theme-bg-inverse: #{pBgInverse themeDark};
  --theme-fg-inverse: #{pFgInverse themeDark};
  --theme-hover: #{pHover themeDark};
  --theme-accent-hover: #{pAccentHover themeDark};
}
|]

--------------------------------------------------------------------------------
-- Semantic Tokens (Tailwind classes referencing CSS variables)
--
-- These tokens use Tailwind's arbitrary value syntax to reference
-- the CSS custom properties defined by the theme.

-- | Main background color
bgMain :: Text
bgMain = "bg-[var(--theme-bg)]"

-- | Secondary/alternate background color (cards, muted sections)
bgAlt :: Text
bgAlt = "bg-[var(--theme-bg-alt)]"

-- | Inverse background (e.g., dark header on light page)
bgInverse :: Text
bgInverse = "bg-[var(--theme-bg-inverse)]"

-- | Primary text color
fgPrimary :: Text
fgPrimary = "text-[var(--theme-fg)]"

-- | Muted/secondary text color
fgMuted :: Text
fgMuted = "text-[var(--theme-fg-muted)]"

-- | Inverse text color (for inverse backgrounds)
fgInverse :: Text
fgInverse = "text-[var(--theme-fg-inverse)]"

-- | Default border color
borderDefault :: Text
borderDefault = "border-[var(--theme-border)]"

-- | Muted/subtle border color
borderMuted :: Text
borderMuted = "border-[var(--theme-border-muted)]"

-- | Primary accent background (for buttons, links)
accent :: Text
accent = "bg-[var(--theme-accent)]"

-- | Accent hover state
accentHover :: Text
accentHover = "hover:bg-[var(--theme-accent-hover)]"

-- | Text color for accent backgrounds
accentFg :: Text
accentFg = "text-[var(--theme-accent-fg)]"

-- | Success color (use for text, borders, or backgrounds)
success :: Text
success = "text-[var(--theme-success)]"

-- | Error color (use for text, borders, or backgrounds)
error :: Text
error = "text-[var(--theme-error)]"

-- | Warning color (use for text, borders, or backgrounds)
warning :: Text
warning = "text-[var(--theme-warning)]"

-- | Info color (use for text, borders, or backgrounds)
info :: Text
info = "text-[var(--theme-info)]"

-- | Hover background color
hoverBg :: Text
hoverBg = "hover:bg-[var(--theme-hover)]"

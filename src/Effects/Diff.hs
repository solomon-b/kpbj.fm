-- | Diff computation and rendering utilities for site page revision history.
--
-- Uses the @Diff@ library to compute line-by-line differences between
-- text content, with HTML rendering for visual display in the dashboard.
module Effects.Diff
  ( -- * Types
    DiffLine (..),

    -- * Computation
    computeLineDiff,

    -- * Rendering
    renderDiff,
  )
where

--------------------------------------------------------------------------------

import Data.Algorithm.Diff (Diff, PolyDiff (..), getGroupedDiff)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified

--------------------------------------------------------------------------------
-- Types

-- | A line in a diff output with its change type.
data DiffLine
  = -- | Line exists in both versions (unchanged)
    Context Text
  | -- | Line was added (only in new version)
    Added Text
  | -- | Line was removed (only in old version)
    Removed Text
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Computation

-- | Compute line-by-line diff between old and new content.
--
-- Returns a list of diff lines showing what changed between the two versions.
-- Lines are tagged as Context (unchanged), Added (new), or Removed (old).
computeLineDiff ::
  -- | Old content (e.g., revision content)
  Text ->
  -- | New content (e.g., current page content)
  Text ->
  [DiffLine]
computeLineDiff oldText newText =
  let oldLines = Text.lines oldText
      newLines = Text.lines newText
      diffs = getGroupedDiff oldLines newLines
   in concatMap convertDiff diffs
  where
    convertDiff :: Diff [Text] -> [DiffLine]
    convertDiff = \case
      First oldLines -> map Removed oldLines
      Second newLines -> map Added newLines
      Both sameLines _ -> map Context sameLines

--------------------------------------------------------------------------------
-- Rendering

-- | Render a diff as HTML with color-coded lines.
--
-- - Green background for added lines
-- - Red background for removed lines
-- - Gray for context lines
renderDiff :: [DiffLine] -> Lucid.Html ()
renderDiff diffLines =
  Lucid.div_ [class_ $ base ["font-mono", Tokens.textSm, "border", Tokens.borderMuted, "rounded", "overflow-hidden"]] $ do
    if null diffLines
      then Lucid.div_ [class_ $ base [Tokens.p4, Tokens.fgMuted, "text-center"]] "No changes"
      else mapM_ renderLine diffLines
  where
    renderLine :: DiffLine -> Lucid.Html ()
    renderLine = \case
      Context line ->
        Lucid.div_ [class_ $ base [Tokens.px3, "py-1", Tokens.bgAlt, "border-l-4", Tokens.borderMuted]] $ do
          Lucid.span_ [class_ $ base [Tokens.fgMuted, "mr-2", "select-none"]] " "
          Lucid.span_ $ Lucid.toHtml (displayLine line)
      Added line ->
        Lucid.div_ [class_ $ base [Tokens.px3, "py-1", Tokens.successBg, "border-l-4", Tokens.successBorder]] $ do
          Lucid.span_ [class_ $ base [Tokens.successText, "mr-2", "select-none", Tokens.fontBold]] "+"
          Lucid.span_ [class_ $ base [Tokens.successText]] $ Lucid.toHtml (displayLine line)
      Removed line ->
        Lucid.div_ [class_ $ base [Tokens.px3, "py-1", Tokens.errorBg, "border-l-4", Tokens.errorBorder]] $ do
          Lucid.span_ [class_ $ base [Tokens.errorText, "mr-2", "select-none", Tokens.fontBold]] "-"
          Lucid.span_ [class_ $ base [Tokens.errorText]] $ Lucid.toHtml (displayLine line)

    -- Display empty lines as a non-breaking space for visibility
    displayLine :: Text -> Text
    displayLine t
      | Text.null t = "\160" -- Non-breaking space
      | otherwise = t

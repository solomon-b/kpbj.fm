-- | A modal dialog that Alpine shows and hides.
--
-- A modal has three parts:
--
-- * The root covers the screen and centers the box. Its @x-show@ expression
--   controls when the modal shows.
-- * The backdrop darkens the page. A click on it runs the close expression.
-- * The box holds the content.
--
-- > import Component.Modal qualified as Modal
-- >
-- > Modal.modal Modal.Standard "open" "open = false" [xData_ "{ open: true }"] $ do
-- >   Lucid.h2_ "Password Reset"
-- >   ...
--
-- Each look copies a modal that the pages had before this module.
module Component.Modal
  ( -- * Variants
    Look (..),

    -- * Rendering
    modal,
    classes,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Data.List (nub)
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine (xOnClick_, xShow_)

--------------------------------------------------------------------------------
-- Variants

-- | The look of a modal.
data Look
  = -- | A narrow box with a 2px border and padding, on a backdrop at 70%
    -- opacity. The dashboard password reset uses it.
    Standard
  | -- | A wide column box with a 1px border and no padding, on a backdrop at
    -- 75% black. The form builder image cropper uses it. The content gives
    -- its own header, body, and footer.
    Cropper
  deriving stock (Eq, Show, Enum, Bounded)

--------------------------------------------------------------------------------
-- Classes

rootClasses :: Look -> [Text]
rootClasses = \case
  Standard -> centered <> [Tokens.p4]
  Cropper -> centered
  where
    centered = ["fixed", "inset-0", "z-50", "flex", "items-center", "justify-center"]

backdropClasses :: Look -> [Text]
backdropClasses = \case
  Standard -> ["absolute", "inset-0", "bg-black", "opacity-70"]
  Cropper -> ["absolute", "inset-0", "bg-black/75"]

boxClasses :: Look -> [Text]
boxClasses = \case
  Standard ->
    ["relative", Tokens.fullWidth, "max-w-md", Tokens.bgMain, Tokens.fgPrimary, Tokens.border2, Tokens.borderDefault, Tokens.p6]
  Cropper ->
    ["relative", Tokens.bgMain, "border", Tokens.borderDefault, "w-[90%]", "max-w-[600px]", "max-h-[90vh]", "overflow-hidden", "flex", "flex-col"]

-- | Every class of the root, the backdrop, and the box of a look.
classes :: Look -> [Text]
classes look = nub $ rootClasses look <> backdropClasses look <> boxClasses look

--------------------------------------------------------------------------------
-- Rendering

-- | A modal with this look.
modal ::
  Look ->
  -- | The Alpine expression that is true while the modal shows
  Text ->
  -- | The Alpine expression that closes the modal
  Text ->
  -- | More attributes for the root, such as @x-data@ or a @style@ that hides
  -- the modal before Alpine starts
  [Lucid.Attributes] ->
  -- | The content of the box
  Lucid.Html () ->
  Lucid.Html ()
modal look shown close rootAttrs content =
  Lucid.div_ ([class_ $ base (rootClasses look), xShow_ shown] <> rootAttrs) $ do
    Lucid.div_ [class_ $ base (backdropClasses look), xOnClick_ close] mempty
    Lucid.div_ [class_ $ base (boxClasses look)] content

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that this module can render. "Component.Tailwind" puts them in
-- the safelist, because a modal can first appear in an HTMX swap.
safelist :: [Text]
safelist = nub $ foldMap classes [minBound .. maxBound]

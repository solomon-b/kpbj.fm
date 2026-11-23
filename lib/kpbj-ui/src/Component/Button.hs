{-# LANGUAGE QuasiQuotes #-}

-- | Buttons for the dashboard, the form builder, and the public pages.
--
-- Every dashboard button, every form builder button, and the main buttons of
-- the public pages render through this module. A caller describes a button with a 'Spec': its style by the purpose
-- of the action, its size, its corners, its width, its kind (a button, a
-- submit, a link, a label, or a toggle), and its extra attributes. A caller
-- starts from 'defaults', changes it with the named modifiers or with the
-- lenses, and draws it with 'render'.
--
-- The module re-exports '(&)', so a caller needs no lens import:
--
-- > import Component.Button ((&))
-- > import Component.Button qualified as Button
-- >
-- > Button.render (Button.defaults & Button.primary & Button.with [hxPost_ url]) "Approve"
-- > Button.render (Button.defaults & Button.small & Button.submit) "Filter"
-- > Button.render (Button.defaults & Button.dashboardLink editUrl) "Edit"
-- > Button.render (Button.defaults & Button.accent & Button.large & Button.square & Button.submit) "LOGIN"
--
-- A modifier is a lens setter with its value given, so a later modifier wins
-- over an earlier one for the same field. 'with' is the exception: it adds
-- to the attributes, so two 'with' calls join their lists.
module Component.Button
  ( -- * Spec
    Spec,
    ButtonStyle (..),
    Tone (..),
    Size (..),
    Corners (..),
    Width (..),
    Kind (..),
    defaults,

    -- * Lenses
    style,
    size,
    corners,
    width,
    kind,
    attributes,

    -- * Modifiers
    (&),
    primary,
    secondary,
    danger,
    tinted,
    accent,
    muted,
    bare,
    small,
    large,
    square,
    fullWidth,
    submit,
    dashboardLink,
    pageLink,
    labelFor,
    toggle,
    with,

    -- * Rendering
    render,
    group,
    downloadLink,

    -- * Tailwind
    safelist,
  )
where

--------------------------------------------------------------------------------

import Control.Lens (Lens', lens, (&), (.~), (<>~), (^.))
import Data.List (nub)
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
import Design.Theme qualified as Theme
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.Alpine (xBindClass_)
import Lucid.HTMX

--------------------------------------------------------------------------------
-- Spec

-- | The look of a button, by the purpose of its action.
data ButtonStyle
  = -- | The main action of a page, row, or form.
    Primary
  | -- | Every other action.
    Secondary
  | -- | A destructive action.
    Danger
  | -- | A status action, where the color carries meaning.
    Tinted Tone
  | -- | The main action in the accent color of the theme, such as the submit
    -- button of a public form.
    Accent
  | -- | A quiet action on the alternate background, such as the cancel link
    -- of a public form.
    Muted
  | -- | A quiet action with no fill, in the text color of its tone, such as
    -- the link below a public form to a different form.
    Bare Tone
  deriving stock (Eq, Show)

-- | The status color of a 'Tinted' or 'Bare' button.
data Tone
  = Success
  | Warning
  | Error
  | Info
  deriving stock (Eq, Show, Enum, Bounded)

-- | The padding of a button.
data Size
  = -- | For a button next to small inputs, such as a top-bar filter form.
    Small
  | Medium
  | -- | For the footer of a public form. The text keeps the size of its
    -- parent.
    Large
  deriving stock (Eq, Show, Enum, Bounded)

-- | The corners of a button.
data Corners
  = Rounded
  | Square
  deriving stock (Eq, Show, Enum, Bounded)

-- | The width of a button.
data Width
  = -- | As wide as its label.
    Auto
  | -- | As wide as its container.
    Full
  deriving stock (Eq, Show, Enum, Bounded)

-- | The element a button renders as, and what it does.
data Kind
  = -- | A button that runs its attributes, such as an HTMX request.
    Action
  | -- | A button that submits its form.
    Submit
  | -- | A link to a dashboard page. HTMX swaps the page into the dashboard
    -- frame and pushes the URL, the same as a click on a row cell.
    DashboardLink Text
  | -- | A link that leaves the dashboard frame, such as a public page or an
    -- @#anchor@ on the same page.
    PageLink Text
  | -- | A label for the input with the given id. A click on the label has
    -- the same effect as a click on the input. The form builder uses it to
    -- open a hidden file input.
    LabelFor Text
  | -- | A button with a selected state. It takes the Alpine condition that
    -- is true when the button is selected, and the style when it is not. The
    -- style of the spec is the style when selected.
    Toggle Text ButtonStyle
  deriving stock (Eq, Show)

-- | Everything that a button is.
data Spec = Spec
  { _style :: ButtonStyle,
    _size :: Size,
    _corners :: Corners,
    _width :: Width,
    _kind :: Kind,
    _attributes :: [Lucid.Attributes]
  }

-- | A 'Secondary' 'Action' button of 'Medium' size, 'Rounded' corners, and
-- 'Auto' width, with no extra attributes.
defaults :: Spec
defaults =
  Spec
    { _style = Secondary,
      _size = Medium,
      _corners = Rounded,
      _width = Auto,
      _kind = Action,
      _attributes = []
    }

--------------------------------------------------------------------------------
-- Lenses

style :: Lens' Spec ButtonStyle
style = lens _style (\s x -> s {_style = x})

size :: Lens' Spec Size
size = lens _size (\s x -> s {_size = x})

corners :: Lens' Spec Corners
corners = lens _corners (\s x -> s {_corners = x})

width :: Lens' Spec Width
width = lens _width (\s x -> s {_width = x})

kind :: Lens' Spec Kind
kind = lens _kind (\s x -> s {_kind = x})

attributes :: Lens' Spec [Lucid.Attributes]
attributes = lens _attributes (\s x -> s {_attributes = x})

--------------------------------------------------------------------------------
-- Modifiers

primary :: Spec -> Spec
primary = style .~ Primary

secondary :: Spec -> Spec
secondary = style .~ Secondary

danger :: Spec -> Spec
danger = style .~ Danger

tinted :: Tone -> Spec -> Spec
tinted tone = style .~ Tinted tone

accent :: Spec -> Spec
accent = style .~ Accent

muted :: Spec -> Spec
muted = style .~ Muted

bare :: Tone -> Spec -> Spec
bare tone = style .~ Bare tone

small :: Spec -> Spec
small = size .~ Small

large :: Spec -> Spec
large = size .~ Large

square :: Spec -> Spec
square = corners .~ Square

fullWidth :: Spec -> Spec
fullWidth = width .~ Full

submit :: Spec -> Spec
submit = kind .~ Submit

dashboardLink ::
  -- | Target URL
  Text ->
  Spec ->
  Spec
dashboardLink url = kind .~ DashboardLink url

pageLink ::
  -- | Target URL
  Text ->
  Spec ->
  Spec
pageLink url = kind .~ PageLink url

labelFor ::
  -- | Id of the input
  Text ->
  Spec ->
  Spec
labelFor inputId = kind .~ LabelFor inputId

toggle ::
  -- | Alpine condition that is true when the button is selected
  Text ->
  -- | Style when not selected
  ButtonStyle ->
  Spec ->
  Spec
toggle condition unselected = kind .~ Toggle condition unselected

-- | Add attributes, such as @hx-*@ or Alpine attributes.
with :: [Lucid.Attributes] -> Spec -> Spec
with attrs = attributes <>~ attrs

--------------------------------------------------------------------------------
-- Classes

-- | The classes that every button has.
--
-- @ease-in-out@ in Tailwind is a cubic Bezier curve, so the transition uses
-- the CSS @ease-in-out@ as an arbitrary value.
baseClasses :: [Text]
baseClasses =
  [ Tokens.fontBold,
    "whitespace-nowrap",
    "cursor-pointer",
    "no-underline",
    "transition-all",
    "duration-150",
    "ease-[ease-in-out]",
    "disabled:opacity-50",
    "disabled:cursor-not-allowed"
  ]

sizeClasses :: Size -> [Text]
sizeClasses = \case
  Small -> ["px-2", "py-1", Tokens.textSm]
  Medium -> [Tokens.px3, Tokens.py2, Tokens.textSm]
  Large -> [Tokens.px6, "py-3"]

cornerClasses :: Corners -> [Text]
cornerClasses = \case
  Rounded -> ["rounded"]
  Square -> []

widthClasses :: Width -> [Text]
widthClasses = \case
  Auto -> []
  Full -> ["w-full"]

-- | The classes of a button that do not change with the style.
shapeClasses :: Spec -> [Text]
shapeClasses spec =
  baseClasses
    <> sizeClasses (spec ^. size)
    <> cornerClasses (spec ^. corners)
    <> widthClasses (spec ^. width)

-- | The classes that give a style its colors and its hover effect.
colorClasses :: ButtonStyle -> [Text]
colorClasses = \case
  Primary -> [Tokens.bgInverse, Tokens.fgInverse, fade]
  Secondary -> [Tokens.bgButton, Tokens.fgPrimary, fade]
  Danger -> [Tokens.bgButton, Tokens.errorText, fade]
  Tinted Success -> [tint "success", Tokens.successText, fade]
  Tinted Warning -> [tint "warning", Tokens.warningText, fade]
  Tinted Error -> [tint "error", Tokens.errorText, fade]
  Tinted Info -> [tint "info", Tokens.infoText, fade]
  Accent -> [Theme.accent, Theme.accentFg, "hover:opacity-90"]
  Muted -> [Tokens.bgAlt, Tokens.fgPrimary, Tokens.hoverBg]
  Bare Success -> [Tokens.successText, Tokens.hoverBg]
  Bare Warning -> [Tokens.warningText, Tokens.hoverBg]
  Bare Error -> [Tokens.errorText, Tokens.hoverBg]
  Bare Info -> [Tokens.infoText, Tokens.hoverBg]
  where
    fade :: Text
    fade = "hover:opacity-80"

    -- A mix with transparent gives the tint, the same as the status tokens
    -- such as 'Tokens.warningBg'. A button tint is 15%, so it is stronger
    -- than the 10% of a token.
    tint :: Text -> Text
    tint color = [i|bg-[color-mix(in_srgb,var(--theme-#{color})_15%,transparent)]|]

-- | Every style, for the safelist.
--
-- Each case names the next style. A new constructor with no case here is a
-- compile error, so the safelist cannot miss a style.
styles :: [ButtonStyle]
styles = from Primary
  where
    from :: ButtonStyle -> [ButtonStyle]
    from s = case s of
      Primary -> s : from Secondary
      Secondary -> s : from Danger
      Danger -> s : from Accent
      Accent -> s : from Muted
      Muted -> s : map Tinted [minBound .. maxBound] <> map Bare [minBound .. maxBound]
      Tinted _ -> []
      Bare _ -> []

--------------------------------------------------------------------------------
-- Rendering

-- | Draw a button with the given label.
render :: Spec -> Lucid.Html () -> Lucid.Html ()
render spec label = case spec ^. kind of
  Action -> Lucid.button_ ([Lucid.type_ "button", styled] <> extra) label
  Submit -> Lucid.button_ ([Lucid.type_ "submit", styled] <> extra) label
  DashboardLink url ->
    Lucid.a_
      ( [ Lucid.href_ url,
          hxGet_ url,
          hxTarget_ "#main-content",
          hxPushUrl_ "true",
          styled
        ]
          <> extra
      )
      label
  PageLink url -> Lucid.a_ ([Lucid.href_ url, styled] <> extra) label
  -- A label is an inline element, so it needs inline-block for its padding.
  LabelFor inputId ->
    Lucid.label_
      ([Lucid.for_ inputId, class_ $ base ("inline-block" : shape <> colorClasses (spec ^. style))] <> extra)
      label
  Toggle condition unselected ->
    Lucid.button_
      ( [ Lucid.type_ "button",
          class_ $ base shape,
          xBindClass_ [i|#{condition} ? '#{colors (spec ^. style)}' : '#{colors unselected}'|]
        ]
          <> extra
      )
      label
  where
    shape = shapeClasses spec
    styled = class_ $ base (shape <> colorClasses (spec ^. style))
    extra = spec ^. attributes
    colors = Text.unwords . colorClasses

-- | A set of buttons, centered on one line.
--
-- The buttons do not wrap. A narrow table scrolls sideways in its wrapper.
group :: Lucid.Html () -> Lucid.Html ()
group =
  Lucid.div_ [class_ $ base ["flex", "items-center", "justify-center", Tokens.gap2]]

--------------------------------------------------------------------------------
-- Tailwind

-- | Every class that 'render' can give a button.
--
-- "Component.Tailwind" puts these classes in the safelist of the Tailwind CDN
-- config that every frame uses. In Firefox, the CDN does not write the rule
-- for a class that first appears in an HTMX swap, and some button classes
-- appear on one page only.
safelist :: [Text]
safelist =
  nub $
    baseClasses
      <> ["inline-block"]
      <> foldMap sizeClasses [minBound .. maxBound]
      <> foldMap cornerClasses [minBound .. maxBound]
      <> foldMap widthClasses [minBound .. maxBound]
      <> foldMap colorClasses styles

-- | An icon link that downloads a file.
--
-- It is not a styled button, so it does not take a 'Spec'.
downloadLink ::
  -- | File URL
  Text ->
  Lucid.Html ()
downloadLink url =
  Lucid.a_
    [ Lucid.href_ url,
      Lucid.download_ "",
      class_ $ base ["p-2", "rounded", Tokens.hoverBg, "transition-colors"],
      Lucid.title_ "Download"
    ]
    $ Lucid.i_ [class_ $ base ["fa-solid", "fa-download", Tokens.fgMuted]] mempty

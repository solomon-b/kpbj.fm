{-# LANGUAGE QuasiQuotes #-}

-- | Buttons for the dashboard.
--
-- Every dashboard button renders through this module. A caller describes a
-- button with a 'Spec': its style by the purpose of the action, its size, its
-- width, its kind (a button, a submit, a link, or a toggle), and its extra
-- attributes. A caller starts from 'defaults', changes it with the named
-- modifiers or with the lenses, and draws it with 'render'.
--
-- The module re-exports '(&)', so a caller needs no lens import:
--
-- > import Component.Button ((&))
-- > import Component.Button qualified as Button
-- >
-- > Button.render (Button.defaults & Button.primary & Button.with [hxPost_ url]) "Approve"
-- > Button.render (Button.defaults & Button.small & Button.submit) "Filter"
-- > Button.render (Button.defaults & Button.dashboardLink editUrl) "Edit"
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
    Width (..),
    Kind (..),
    defaults,

    -- * Lenses
    style,
    size,
    width,
    kind,
    attributes,

    -- * Modifiers
    (&),
    primary,
    secondary,
    danger,
    tinted,
    small,
    fullWidth,
    submit,
    dashboardLink,
    pageLink,
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
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Design (base, class_)
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
  deriving stock (Eq, Show)

-- | The status color of a 'Tinted' button.
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
  | -- | A button with a selected state. It takes the Alpine condition that
    -- is true when the button is selected, and the style when it is not. The
    -- style of the spec is the style when selected.
    Toggle Text ButtonStyle
  deriving stock (Eq, Show)

-- | Everything that a button is.
data Spec = Spec
  { _style :: ButtonStyle,
    _size :: Size,
    _width :: Width,
    _kind :: Kind,
    _attributes :: [Lucid.Attributes]
  }

-- | A 'Secondary' 'Action' button of 'Medium' size and 'Auto' width, with no
-- extra attributes.
defaults :: Spec
defaults =
  Spec
    { _style = Secondary,
      _size = Medium,
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

small :: Spec -> Spec
small = size .~ Small

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
baseClasses :: [Text]
baseClasses =
  [ Tokens.fontBold,
    "rounded",
    "whitespace-nowrap",
    "hover:opacity-80",
    "disabled:opacity-50",
    "disabled:cursor-not-allowed"
  ]

sizeClasses :: Size -> [Text]
sizeClasses = \case
  Small -> ["px-2", "py-1", Tokens.textSm]
  Medium -> [Tokens.px3, Tokens.py2, Tokens.textSm]

widthClasses :: Width -> [Text]
widthClasses = \case
  Auto -> []
  Full -> ["w-full"]

-- | The classes that do not change with the style.
shapeClasses :: Spec -> [Text]
shapeClasses spec = baseClasses <> sizeClasses (spec ^. size) <> widthClasses (spec ^. width)

-- | The classes that give a style its colors.
colorClasses :: ButtonStyle -> [Text]
colorClasses = \case
  Primary -> [Tokens.bgInverse, Tokens.fgInverse]
  Secondary -> [Tokens.bgButton, Tokens.fgPrimary]
  Danger -> [Tokens.bgButton, Tokens.errorText]
  Tinted Success -> [tint "success", Tokens.successText]
  Tinted Warning -> [tint "warning", Tokens.warningText]
  Tinted Error -> [tint "error", Tokens.errorText]
  Tinted Info -> [tint "info", Tokens.infoText]
  where
    -- The status tokens such as 'Tokens.warningBg' use an opacity modifier,
    -- which Tailwind cannot apply to a color from a CSS variable. A mix with
    -- transparent gives the tint.
    tint :: Text -> Text
    tint color = [i|bg-[color-mix(in_srgb,var(--theme-#{color})_15%,transparent)]|]

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
  Toggle condition unselected ->
    Lucid.button_
      ( [ Lucid.type_ "button",
          class_ $ base (shapeClasses spec),
          xBindClass_ [i|#{condition} ? '#{colors (spec ^. style)}' : '#{colors unselected}'|]
        ]
          <> extra
      )
      label
  where
    styled = class_ $ base (shapeClasses spec <> colorClasses (spec ^. style))
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
  baseClasses
    <> foldMap sizeClasses [minBound .. maxBound]
    <> foldMap widthClasses [minBound .. maxBound]
    <> foldMap colorClasses styles
  where
    styles = [Primary, Secondary, Danger] <> map Tinted [minBound .. maxBound]

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
    $ Lucid.i_ [Lucid.class_ [i|fa-solid fa-download #{Tokens.fgMuted}|]] mempty

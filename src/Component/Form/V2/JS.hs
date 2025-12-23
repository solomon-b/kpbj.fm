-- | Type-safe JavaScript expression building.
--
-- This module provides a deep embedding of JavaScript expressions,
-- allowing type-safe composition of validation logic without string interpolation.
--
-- Inspired by the experimental 'Component.Form.JSArrow' module but using
-- a simpler ADT representation instead of Arrow abstraction.
--
-- Example:
--
-- > -- Check that email contains @ and is non-empty
-- > emailValidation :: JSExpr
-- > emailValidation = allOf
-- >   [ checkRequired
-- >   , method "includes" [str "@"] (var "value")
-- >   ]
-- >
-- > -- Render to JavaScript
-- > renderExpr emailValidation
-- > -- "(value.trim().length > 0) && (value.includes('@'))"
module Component.Form.V2.JS
  ( -- * Core Types
    JSExpr (..),
    JSValue (..),

    -- * Literal Builders
    str,
    num,
    int,
    bool,
    null_,

    -- * Expression Builders
    var,
    field,
    index,
    call,
    method,
    ternary,
    raw,
    obj,
    arr,

    -- * Operators
    (.&&),
    (.||),
    (.==),
    (.!=),
    (.<),
    (.<=),
    (.>),
    (.>=),
    (.+),
    (.-),
    not_,

    -- * Validation Combinators
    checkRequired,
    checkMinLength,
    checkMaxLength,
    checkPattern,
    allOf,
    anyOf,

    -- * Rendering
    renderExpr,
    renderValue,
    escapeJsString,

    -- * Utilities
    capitalizeFirst,
  )
where

--------------------------------------------------------------------------------

import Data.Char (toUpper)
import Data.Text (Text)
import Data.Text qualified as Text

--------------------------------------------------------------------------------
-- Core Types

-- | A JavaScript expression represented as an ADT.
--
-- This is a "deep embedding" that we render to text at the end,
-- providing type safety during construction.
data JSExpr
  = -- | Literal value
    JSLit JSValue
  | -- | Variable reference
    JSVar Text
  | -- | Property access: @expr.field@
    JSField JSExpr Text
  | -- | Index access: @expr[index]@
    JSIndex JSExpr JSExpr
  | -- | Function call: @expr(args...)@
    JSCall JSExpr [JSExpr]
  | -- | Method call: @expr.method(args...)@
    JSMethod JSExpr Text [JSExpr]
  | -- | Binary operator: @left op right@
    JSBinOp Text JSExpr JSExpr
  | -- | Unary prefix operator: @op expr@
    JSUnaryOp Text JSExpr
  | -- | Ternary: @cond ? then : else@
    JSTernary JSExpr JSExpr JSExpr
  | -- | Object literal: @{ k1: v1, k2: v2 }@
    JSObject [(Text, JSExpr)]
  | -- | Array literal: @[e1, e2, ...]@
    JSArray [JSExpr]
  | -- | Raw JavaScript (escape hatch)
    JSRaw Text
  deriving stock (Show, Eq)

-- | JavaScript literal values.
data JSValue
  = JSString Text
  | JSNumber Double
  | JSInt Int
  | JSBool Bool
  | JSNull
  | JSUndefined
  deriving stock (Show, Eq)

--------------------------------------------------------------------------------
-- Literal Builders

-- | String literal.
str :: Text -> JSExpr
str = JSLit . JSString

-- | Number literal (floating point).
num :: Double -> JSExpr
num = JSLit . JSNumber

-- | Integer literal.
int :: Int -> JSExpr
int = JSLit . JSInt

-- | Boolean literal.
bool :: Bool -> JSExpr
bool = JSLit . JSBool

-- | Null literal.
null_ :: JSExpr
null_ = JSLit JSNull

--------------------------------------------------------------------------------
-- Expression Builders

-- | Variable reference.
var :: Text -> JSExpr
var = JSVar

-- | Property access: @expr.fieldName@
field :: Text -> JSExpr -> JSExpr
field name expr = JSField expr name

-- | Index access: @expr[index]@
index :: JSExpr -> JSExpr -> JSExpr
index = JSIndex

-- | Function call: @fn(args...)@
call :: JSExpr -> [JSExpr] -> JSExpr
call = JSCall

-- | Method call: @obj.methodName(args...)@
method :: Text -> [JSExpr] -> JSExpr -> JSExpr
method name args expr = JSMethod expr name args

-- | Ternary conditional: @cond ? ifTrue : ifFalse@
ternary :: JSExpr -> JSExpr -> JSExpr -> JSExpr
ternary = JSTernary

-- | Raw JavaScript expression (escape hatch).
--
-- Use sparingly - prefer the typed constructors.
raw :: Text -> JSExpr
raw = JSRaw

-- | Object literal: @{ k1: v1, k2: v2 }@
obj :: [(Text, JSExpr)] -> JSExpr
obj = JSObject

-- | Array literal: @[e1, e2, ...]@
arr :: [JSExpr] -> JSExpr
arr = JSArray

--------------------------------------------------------------------------------
-- Operators

-- | Logical AND: @left && right@
(.&&) :: JSExpr -> JSExpr -> JSExpr
(.&&) = JSBinOp "&&"

infixr 3 .&&

-- | Logical OR: @left || right@
(.||) :: JSExpr -> JSExpr -> JSExpr
(.||) = JSBinOp "||"

infixr 2 .||

-- | Strict equality: @left === right@
(.==) :: JSExpr -> JSExpr -> JSExpr
(.==) = JSBinOp "==="

infix 4 .==

-- | Strict inequality: @left !== right@
(.!=) :: JSExpr -> JSExpr -> JSExpr
(.!=) = JSBinOp "!=="

infix 4 .!=

-- | Less than: @left < right@
(.<) :: JSExpr -> JSExpr -> JSExpr
(.<) = JSBinOp "<"

infix 4 .<

-- | Less than or equal: @left <= right@
(.<=) :: JSExpr -> JSExpr -> JSExpr
(.<=) = JSBinOp "<="

infix 4 .<=

-- | Greater than: @left > right@
(.>) :: JSExpr -> JSExpr -> JSExpr
(.>) = JSBinOp ">"

infix 4 .>

-- | Greater than or equal: @left >= right@
(.>=) :: JSExpr -> JSExpr -> JSExpr
(.>=) = JSBinOp ">="

infix 4 .>=

-- | Addition: @left + right@
(.+) :: JSExpr -> JSExpr -> JSExpr
(.+) = JSBinOp "+"

infixl 6 .+

-- | Subtraction: @left - right@
(.-) :: JSExpr -> JSExpr -> JSExpr
(.-) = JSBinOp "-"

infixl 6 .-

-- | Logical negation: @!expr@
not_ :: JSExpr -> JSExpr
not_ = JSUnaryOp "!"

--------------------------------------------------------------------------------
-- Validation Combinators

-- | Check if value is non-empty after trimming.
--
-- Generates: @value.trim().length > 0@
checkRequired :: JSExpr
checkRequired =
  field "length" (method "trim" [] (var "value")) .> int 0

-- | Check minimum length after trimming.
--
-- Generates: @value.trim().length >= n@
checkMinLength :: Int -> JSExpr
checkMinLength n =
  field "length" (method "trim" [] (var "value")) .>= int n

-- | Check maximum length after trimming.
--
-- Generates: @value.trim().length <= n@
checkMaxLength :: Int -> JSExpr
checkMaxLength n =
  field "length" (method "trim" [] (var "value")) .<= int n

-- | Check regex pattern match.
--
-- Generates: @/pattern/.test(value)@
checkPattern :: Text -> JSExpr
checkPattern pat =
  JSMethod (JSRaw ("/" <> pat <> "/")) "test" [var "value"]

-- | Combine validations with AND (all must pass).
allOf :: [JSExpr] -> JSExpr
allOf [] = bool True
allOf [x] = x
allOf (x : xs) = x .&& allOf xs

-- | Combine validations with OR (any must pass).
anyOf :: [JSExpr] -> JSExpr
anyOf [] = bool False
anyOf [x] = x
anyOf (x : xs) = x .|| anyOf xs

--------------------------------------------------------------------------------
-- Rendering

-- | Render a JSExpr to JavaScript source text.
renderExpr :: JSExpr -> Text
renderExpr = \case
  JSLit v -> renderValue v
  JSVar name -> name
  JSField expr f -> renderExpr expr <> "." <> f
  JSIndex expr i -> renderExpr expr <> "[" <> renderExpr i <> "]"
  JSCall fn args ->
    renderExpr fn <> "(" <> Text.intercalate ", " (map renderExpr args) <> ")"
  JSMethod expr m args ->
    renderExpr expr <> "." <> m <> "(" <> Text.intercalate ", " (map renderExpr args) <> ")"
  JSBinOp op left right ->
    "(" <> renderExpr left <> " " <> op <> " " <> renderExpr right <> ")"
  JSUnaryOp op expr ->
    op <> renderExpr expr
  JSTernary cond t f ->
    "(" <> renderExpr cond <> " ? " <> renderExpr t <> " : " <> renderExpr f <> ")"
  JSObject fields ->
    "{ " <> Text.intercalate ", " (map renderField fields) <> " }"
    where
      renderField (k, v) = k <> ": " <> renderExpr v
  JSArray items ->
    "[" <> Text.intercalate ", " (map renderExpr items) <> "]"
  JSRaw t -> t

-- | Render a JSValue to JavaScript source text.
renderValue :: JSValue -> Text
renderValue = \case
  JSString s -> "'" <> escapeJsString s <> "'"
  JSNumber n -> Text.pack (show n)
  JSInt n -> Text.pack (show n)
  JSBool True -> "true"
  JSBool False -> "false"
  JSNull -> "null"
  JSUndefined -> "undefined"

-- | Escape special characters in a JavaScript string.
escapeJsString :: Text -> Text
escapeJsString = Text.concatMap escapeChar
  where
    escapeChar '\\' = "\\\\"
    escapeChar '\'' = "\\'"
    escapeChar '"' = "\\\""
    escapeChar '\n' = "\\n"
    escapeChar '\r' = "\\r"
    escapeChar '\t' = "\\t"
    escapeChar '`' = "\\`"
    escapeChar c = Text.singleton c

--------------------------------------------------------------------------------
-- Utilities

-- | Capitalize the first character of a Text.
capitalizeFirst :: Text -> Text
capitalizeFirst t = case Text.uncons t of
  Nothing -> ""
  Just (c, rest) -> Text.cons (toUpper c) rest

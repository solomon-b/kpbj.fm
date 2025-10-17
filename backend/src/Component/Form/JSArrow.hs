{-# LANGUAGE Arrows #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuasiQuotes #-}

-- | EXPERIMENTAL: Arrow-based JavaScript generation
--
-- This module explores using Arrow abstraction for type-safe, compositional
-- JavaScript generation. Inspired by Conal Elliott's "Compiling to Categories"
-- work, but using Arrow (which is already in base) instead of requiring
-- custom Cartesian Category typeclasses.
--
-- Key ideas:
-- - JavaScript expressions as arrows (JSArrow a b)
-- - Use (&&&) for product/fanout (building objects with multiple fields)
-- - Use (>>>) for composition (chaining computations)
-- - Arrow notation (proc/do) for readable pipeline building
--
-- This is separate from the production form builder code to allow experimentation.
module Component.Form.JSArrow
  ( -- * Core Type
    JSArrow,
    toJS,

    -- * Smart Constructors
    jsVar,
    jsField,
    jsLit,
    jsObject,
    jsFunction,
    jsTernary,

    -- * Validation Combinators
    checkRequired,
    checkMinLength,
    checkMaxLength,
    allValid,

    -- * Examples
    exampleFieldState,
    exampleValidator,
    exampleAlpineState,
    exampleAlpineState',
  )
where

--------------------------------------------------------------------------------

import Control.Arrow
import Control.Category qualified as Cat
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Prelude hiding (id, (.))

--------------------------------------------------------------------------------

-- | Arrow representing JavaScript expressions
--
-- A value of type @JSArrow a b@ represents a JavaScript function from @a@ to @b@.
-- The phantom types track input/output but the actual representation is
-- generated JavaScript text.
--
-- This is a "shallow embedding" - we generate JavaScript directly rather than
-- building an AST. For our use case (Alpine.js generation), this is simpler
-- and sufficient.
newtype JSArrow a b = JSArrow
  { -- | Generate JavaScript code for this arrow
    toJS :: Text
  }
  deriving (Show)

--------------------------------------------------------------------------------

instance Cat.Category JSArrow where
  id :: JSArrow a a
  id = JSArrow "x => x"

  -- x => f(g(x))
  (.) :: JSArrow b c -> JSArrow a b -> JSArrow a c
  JSArrow f . JSArrow g = JSArrow [i|(x => (#{f})((#{g})(x)))|]

instance Arrow JSArrow where
  -- Lift a pure Haskell function into JavaScript
  -- NOTE: This is a bit of a lie - we can't actually embed arbitrary Haskell functions!
  -- In practice, we use smart constructors instead of 'arr'
  arr :: (b -> c) -> JSArrow b c
  arr _ = JSArrow "x => x" -- Default to identity

  -- first f: ([a,b]) => [f(a), b]
  first :: JSArrow b c -> JSArrow (b, d) (c, d)
  first (JSArrow f) = JSArrow [i|([a, b]) => [(#{f})(a), b]|]

  -- Fanout: f &&& g: x => [f(x), g(x)]
  (&&&) :: JSArrow b c -> JSArrow b c' -> JSArrow b (c, c')
  JSArrow f &&& JSArrow g = JSArrow [i|(x => [(#{f})(x), (#{g})(x)])|]

  -- Parallel: f *** g: ([a,b]) => [f(a), g(b)]
  JSArrow f *** JSArrow g = JSArrow [i|(([a, b]) => [(#{f})(a), (#{g})(b)])|]

--------------------------------------------------------------------------------

-- | Reference a JavaScript variable by name
-- Example: jsVar "title" generates: "title"
jsVar :: Text -> JSArrow env a
jsVar = JSArrow

-- | Access a field on an object
-- Example: jsField "value" generates: "obj => obj.value"
jsField :: Text -> JSArrow obj field
jsField name = JSArrow [i|obj => obj.#{name}|]

-- | Literal JavaScript value (already a string)
-- Example: jsLit "true" generates: "() => true"
jsLit :: Text -> JSArrow () a
jsLit value = JSArrow [i|(() => #{value})|]

-- | Constant value that ignores its input
-- Example: jsConst "true" generates: "_ => true"
jsConst :: Text -> JSArrow a b
jsConst value = JSArrow [i|(_ => #{value})|]

-- | Create a JavaScript object from field arrows
-- Each field gets the same input value
jsObject :: [(Text, JSArrow input field)] -> JSArrow input obj
jsObject fields =
  let fieldExprs = map (\(name, JSArrow expr) -> [i|#{name}: (#{expr})(input)|]) fields
      fieldsJS = Text.intercalate ", " fieldExprs
   in JSArrow [i|(input => ({ #{fieldsJS} }))|]

-- | Create a named JavaScript function
-- Example: jsFunction "validate" body generates: "function validate(x) { return body(x); }"
jsFunction :: Text -> JSArrow a b -> JSArrow () (a -> b)
jsFunction name (JSArrow body) =
  JSArrow [i|function #{name}(x) { return (#{body})(x); }|]

-- | Ternary conditional: condition ? ifTrue : ifFalse
jsTernary :: JSArrow a Bool -> JSArrow a b -> JSArrow a b -> JSArrow a b
jsTernary (JSArrow cond) (JSArrow ifTrue) (JSArrow ifFalse) =
  JSArrow [i|(x => (#{cond})(x) ? (#{ifTrue})(x) : (#{ifFalse})(x))|]

--------------------------------------------------------------------------------
-- Validation Combinators

-- | Check if value is non-empty (after trimming)
-- Generates: value => value.trim().length > 0
checkRequired :: Bool -> JSArrow Text Bool
checkRequired False = jsConst "true" -- Not required, always valid
checkRequired True = JSArrow "(value => value.trim().length > 0)"

-- | Check minimum length
-- Generates: value => value.trim().length >= n
checkMinLength :: Maybe Int -> JSArrow Text Bool
checkMinLength Nothing = jsConst "true" -- No minimum
checkMinLength (Just n) = JSArrow [i|(value => value.trim().length >= #{n})|]

-- | Check maximum length
-- Generates: value => value.trim().length <= n
checkMaxLength :: Maybe Int -> JSArrow Text Bool
checkMaxLength Nothing = jsConst "true" -- No maximum
checkMaxLength (Just n) = JSArrow [i|(value => value.trim().length <= #{n})|]

-- | Combine multiple boolean arrows with AND
-- Takes a list of validation arrows and combines them
allValid :: [JSArrow a Bool] -> JSArrow a Bool
allValid [] = jsConst "true"
allValid [single] = single
allValid validators =
  let combine (JSArrow v1) (JSArrow v2) =
        JSArrow [i|(x => (#{v1})(x) && (#{v2})(x))|]
   in foldr1 combine validators

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- | Example: Build a field state object with value and isValid
--
-- >>> toJS $ exampleFieldState "Hello" True
-- "(input => ({ value: (() => Hello)(input), isValid: (() => true)(input) }))"
exampleFieldState :: Text -> Bool -> JSArrow () obj
exampleFieldState initialValue isValid =
  jsObject
    [ ("value", jsLit initialValue),
      ("isValid", jsLit (if isValid then "true" else "false"))
    ]

-- | Example: Build a validator that checks required + min/max length
--
-- This shows how validators compose nicely with allValid!
--
-- >>> toJS $ exampleValidator True (Just 3) (Just 200)
exampleValidator :: Bool -> Maybe Int -> Maybe Int -> JSArrow Text Bool
exampleValidator required minLen maxLen =
  allValid
    [ checkRequired required,
      checkMinLength minLen,
      checkMaxLength maxLen
    ]

-- | Example: Build a simple Alpine.js state with arrow notation
--
-- This demonstrates using proc/do syntax for readability.
-- The result is a nested tuple structure representing the Alpine state.
exampleAlpineState :: JSArrow () (obj1, (obj2, Text))
exampleAlpineState = proc () -> do
  -- Build field states
  title <- exampleFieldState "" True -< ()
  description <- exampleFieldState "" True -< ()

  -- Other state
  showErrors <- jsLit "false" -< ()

  -- Combine using tuple (which translates to array in JS)
  returnA -< (title, (description, showErrors))

-- | Alternative using (&&&) directly
--
-- Shows the same structure as exampleAlpineState but using
-- the combinator directly instead of arrow notation.
exampleAlpineState' :: JSArrow () (obj1, (obj2, Text))
exampleAlpineState' =
  exampleFieldState "" True
    &&& (exampleFieldState "" True &&& jsLit "false")

--------------------------------------------------------------------------------
-- Usage Notes
--------------------------------------------------------------------------------

-- $usage
--
-- = Building JavaScript Compositionally
--
-- The key insight is that Alpine.js state is fundamentally a product type:
--
-- @
-- {
--   fields: { ... },      -- Product of field states
--   showErrors: false,    -- Boolean component
--   validators: { ... }   -- Product of validator functions
-- }
-- @
--
-- With Arrow, we can build this compositionally:
--
-- @
-- alpineState =
--   buildFields fields
--   &&& buildShowErrors
--   &&& buildValidators fields
-- @
--
-- = Comparison with String Interpolation
--
-- Old approach:
-- @
-- generateValidator name rules =
--   let checks = [
--         "value.trim().length > 0" | vrRequired rules,
--         "value.trim().length >= " <> show n | Just n <- vrMinLength rules
--       ]
--   in "function validate() { return " <> intercalate " && " checks <> "; }"
-- @
--
-- Arrow approach:
-- @
-- generateValidator rules =
--   toJS $ allValid [
--     checkRequired (vrRequired rules),
--     checkMinLength (vrMinLength rules),
--     checkMaxLength (vrMaxLength rules)
--   ]
-- @
--
-- The Arrow version is:
-- - More compositional (validators are values that compose)
-- - Type-safe (phantom types track input/output)
-- - Easier to test (each combinator is independently testable)
-- - More declarative (what validators to use, not how to generate strings)

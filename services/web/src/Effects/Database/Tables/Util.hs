-- | Shared utilities for rel8 table modules.
module Effects.Database.Tables.Util
  ( nextId,
  )
where

--------------------------------------------------------------------------------

import Data.Coerce (Coercible, coerce)
import Data.Int (Int64)
import Rel8 (Expr, nextval)
import Rel8 qualified

--------------------------------------------------------------------------------

-- | Generate the next ID from a PostgreSQL sequence.
--
-- Wraps @nextval@ with a coercion from @Expr Int64@ to the target ID newtype.
-- Requires the target type to be a newtype over @Int64@.
--
-- @
-- insertFoo = ... Foo { fooId = nextId "foo_id_seq", ... }
-- @
nextId :: (Coercible (Expr Int64) (Expr a)) => String -> Expr a
nextId seqName = coerce (nextval (Rel8.QualifiedName {Rel8.name = seqName, Rel8.schema = Nothing}))

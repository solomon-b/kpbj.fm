module Test.Gen.EmailAddress where

--------------------------------------------------------------------------------

import Data.Foldable
import Data.Text
import Domain.Types.EmailAddress
import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

--------------------------------------------------------------------------------

genEmail :: (MonadGen m) => m EmailAddress
genEmail = do
  username <- Gen.text (Range.linear 1 10) Gen.alphaNum
  domain <- Gen.text (Range.linear 1 10) Gen.alpha
  tld <- Gen.text (Range.linear 1 10) Gen.alpha
  pure . mkEmailAddress . toLower $
    fold
      [ username,
        "@",
        domain,
        ".",
        tld
      ]

module Test.Gen.Tables.NewsletterSubscribers where

--------------------------------------------------------------------------------

import Effects.Database.Tables.NewsletterSubscribers qualified as NewsletterSubscribers
import Hedgehog (MonadGen)
import Test.Gen.EmailAddress (genEmail)

--------------------------------------------------------------------------------

newsletterSubscriberInsertGen :: (MonadGen m) => m NewsletterSubscribers.Insert
newsletterSubscriberInsertGen = do
  email <- genEmail
  pure NewsletterSubscribers.Insert {nsiEmail = email}

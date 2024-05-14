module Effects.Email.Class where

--------------------------------------------------------------------------------

import Network.Mail.Mime qualified as Mime

--------------------------------------------------------------------------------

class MonadEmail m where
  sendEmail :: Mime.Mail -> m ()

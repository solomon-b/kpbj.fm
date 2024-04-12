module Main where

--------------------------------------------------------------------------------

import Control.Lens
import Crypto.JOSE.JWK
import Data.Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Text.Strict.Lens

--------------------------------------------------------------------------------

main :: IO ()
main = BL.writeFile "jwk.json" . encode =<< doGen

-- Generate RSA JWK and set "kid" param to
-- base64url-encoded SHA-256 thumbprint of key.
--
doGen :: IO JWK
doGen = do
  jwk <- genJWK (RSAGenParam (4096 `div` 8))
  let h = view thumbprint jwk :: Digest SHA256
      kid = view (re (base64url . digest) . utf8) h
  pure $ set jwkKid (Just kid) jwk

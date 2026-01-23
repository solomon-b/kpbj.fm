{-# LANGUAGE TemplateHaskell #-}

module Assets.Embedded
  ( rangePng,
  )
where

--------------------------------------------------------------------------------

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

--------------------------------------------------------------------------------

-- | The range.png image embedded at compile time.
rangePng :: ByteString
rangePng = $(embedFile "static/range.png")

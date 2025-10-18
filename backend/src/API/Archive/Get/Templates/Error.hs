module API.Archive.Get.Templates.Error (errorTemplate) where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Lucid qualified

--------------------------------------------------------------------------------

errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMessage = do
  Lucid.section_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-600"] "Error"
    Lucid.p_ [Lucid.class_ "text-gray-600"] $ Lucid.toHtml errorMessage

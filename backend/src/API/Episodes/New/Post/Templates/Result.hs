module API.Episodes.New.Post.Templates.Result
  ( successTemplate,
    errorTemplate,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Effects.Database.Tables.Episodes qualified as Episodes
import Lucid qualified

--------------------------------------------------------------------------------

-- | Success template
successTemplate :: Episodes.Id -> Lucid.Html ()
successTemplate episodeId = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-green-600"] "Episode Uploaded Successfully!"
    Lucid.p_ [Lucid.class_ "mb-6 text-gray-600"] $ do
      "Your episode has been created with ID: " <> Lucid.toHtml (show episodeId)
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Back to Dashboard"
      Lucid.a_ [Lucid.href_ "/episodes/upload", Lucid.class_ "border-2 border-gray-800 bg-white text-gray-800 px-6 py-3 font-bold hover:bg-gray-100"] "Upload Another"

-- | Error template
errorTemplate :: Text -> Lucid.Html ()
errorTemplate errorMsg = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-4 text-red-600"] "Upload Failed"
    Lucid.p_ [Lucid.class_ "mb-6 text-gray-600"] $ Lucid.toHtml errorMsg
    Lucid.div_ [Lucid.class_ "flex gap-4 justify-center"] $ do
      Lucid.a_ [Lucid.href_ "/episodes/upload", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Try Again"
      Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "border-2 border-gray-800 bg-white text-gray-800 px-6 py-3 font-bold hover:bg-gray-100"] "Back to Dashboard"

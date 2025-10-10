module API.Episodes.Upload.Get.Templates.Error
  ( notLoggedInTemplate,
    showLoadErrorTemplate,
  )
where

--------------------------------------------------------------------------------

import Lucid qualified

--------------------------------------------------------------------------------

-- | Template shown when user is not logged in
notLoggedInTemplate :: Lucid.Html ()
notLoggedInTemplate = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4"] "Authentication Required"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "You must be logged in to upload episodes."
    Lucid.a_ [Lucid.href_ "/user/login", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Login"

-- | Template shown when failed to load user's shows
showLoadErrorTemplate :: Lucid.Html ()
showLoadErrorTemplate = do
  Lucid.div_ [Lucid.class_ "bg-white border-2 border-red-600 p-8 text-center"] $ do
    Lucid.h2_ [Lucid.class_ "text-xl font-bold mb-4 text-red-600"] "Error"
    Lucid.p_ [Lucid.class_ "mb-4 text-gray-600"] "Failed to load your shows."
    Lucid.a_ [Lucid.href_ "/host/dashboard", Lucid.class_ "bg-blue-600 text-white px-6 py-3 font-bold hover:bg-blue-700"] "Back to Dashboard"

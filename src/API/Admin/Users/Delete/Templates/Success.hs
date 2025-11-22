{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module API.Admin.Users.Delete.Templates.Success (renderSuccessBanner) where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Lucid qualified
import Lucid.Base qualified as LucidBase

--------------------------------------------------------------------------------

-- | Render success banner for user deletion (using hx-swap-oob for out-of-band update)
renderSuccessBanner :: Text -> Lucid.Html ()
renderSuccessBanner userEmail =
  Lucid.div_
    [ Lucid.id_ "success-banner-container",
      LucidBase.makeAttributes "hx-swap-oob" "true"
    ]
    $ do
      Lucid.div_
        [ Lucid.id_ "success-banner",
          Lucid.class_ "bg-green-100 border-2 border-green-600 p-4 mb-6 w-full"
        ]
        $ do
          Lucid.div_ [Lucid.class_ "flex items-center justify-between"] $ do
            Lucid.div_ [Lucid.class_ "flex items-center gap-3"] $ do
              Lucid.span_ [Lucid.class_ "text-2xl"] "✓"
              Lucid.div_ $ do
                Lucid.h3_ [Lucid.class_ "font-bold text-green-800"] "User Deleted"
                Lucid.p_ [Lucid.class_ "text-sm text-green-700"] $
                  Lucid.toHtml ([i|User #{userEmail} has been successfully deleted. They can no longer authenticate.|] :: Text)
            Lucid.button_
              [ Lucid.onclick_ "this.closest('#success-banner').remove()",
                Lucid.class_ "text-green-600 hover:text-green-800 font-bold text-xl"
              ]
              "✕"

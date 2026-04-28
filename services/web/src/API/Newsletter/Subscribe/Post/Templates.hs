{-# LANGUAGE OverloadedRecordDot #-}

module API.Newsletter.Subscribe.Post.Templates
  ( newsletterSignupContainerId,
    thanksFragment,
    signupForm,
  )
where

--------------------------------------------------------------------------------

import API.Links (apiLinks, rootLink)
import API.Types (Routes (..))
import Data.Text (Text)
import Lucid qualified
import Lucid.Base (makeAttributes)
import Lucid.HTMX

--------------------------------------------------------------------------------

-- | DOM id of the wrapper that HTMX swaps. Lives here so the homepage
-- template and the success fragment agree.
newsletterSignupContainerId :: Text
newsletterSignupContainerId = "newsletter-signup"

-- | Replacement content shown after a successful subscribe.
thanksFragment :: Lucid.Html ()
thanksFragment =
  Lucid.div_
    [ Lucid.id_ newsletterSignupContainerId,
      Lucid.class_ "mt-2 text-[var(--theme-success-text)] text-center"
    ]
    "Thanks for subscribing!"

-- | The signup form. Used by the homepage template.
--
-- Posts to @/api/newsletter/subscribe@ via HTMX, swapping
-- @#newsletter-signup@ on success.
signupForm :: Lucid.Html ()
signupForm =
  Lucid.div_ [Lucid.id_ newsletterSignupContainerId] $
    Lucid.form_
      [ hxPost_ (rootLink apiLinks.newsletterSubscribePost),
        hxTarget_ ("#" <> newsletterSignupContainerId),
        hxSwap_ "outerHTML",
        Lucid.class_ "w-full max-w-sm"
      ]
      $ do
        Lucid.div_ [Lucid.class_ "flex items-center border-b border-[var(--theme-border)] py-2"] $ do
          Lucid.input_
            [ Lucid.type_ "email",
              Lucid.name_ "email",
              Lucid.required_ "",
              Lucid.placeholder_ "you@example.com",
              makeAttributes "aria-label" "Email Address",
              Lucid.class_ "appearance-none bg-transparent border-none w-full text-[var(--theme-fg)] mr-3 py-1 px-2 leading-tight focus:outline-none"
            ]
          Lucid.button_
            [ Lucid.type_ "submit",
              Lucid.class_ "px-3 py-2 text-xs font-medium text-center inline-flex items-center text-[var(--theme-fg-inverse)] bg-[var(--theme-bg-inverse)] rounded-md hover:bg-[var(--theme-hover-bg)] focus:ring-4 focus:outline-none focus:ring-[var(--theme-border)]"
            ]
            "Subscribe"

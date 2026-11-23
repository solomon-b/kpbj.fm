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
import Component.Button ((&))
import Component.Button qualified as Button
import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
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
      class_ $ base [Tokens.mt2, Tokens.successText, "text-center"]
    ]
    "Thanks for subscribing!"

-- | The signup form. Used by the homepage template.
--
-- Posts to @/api/newsletter/subscribe@ via HTMX, swapping
-- @#newsletter-signup@ on success.
signupForm :: Lucid.Html ()
signupForm =
  Lucid.div_ [Lucid.id_ newsletterSignupContainerId]
    $ Lucid.form_
      [ hxPost_ (rootLink apiLinks.newsletterSubscribePost),
        hxTarget_ ("#" <> newsletterSignupContainerId),
        hxSwap_ "outerHTML",
        class_ $ base [Tokens.fullWidth, "max-w-sm"]
      ]
    $ do
      Lucid.div_ [class_ $ base ["flex", "items-center", "border-b", Tokens.borderDefault, Tokens.py2]] $ do
        Lucid.input_
          [ Lucid.type_ "email",
            Lucid.name_ "email",
            Lucid.required_ "",
            Lucid.placeholder_ "you@example.com",
            makeAttributes "aria-label" "Email Address",
            class_ $ base ["appearance-none", "bg-transparent", "border-none", Tokens.fullWidth, Tokens.fgPrimary, "mr-3", "py-1", "px-2", "leading-tight", "focus:outline-none"]
          ]
        Button.render (Button.defaults & Button.accent & Button.large & Button.square & Button.submit) "Subscribe"

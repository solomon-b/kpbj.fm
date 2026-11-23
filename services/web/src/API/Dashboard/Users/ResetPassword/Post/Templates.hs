{-# LANGUAGE OverloadedRecordDot #-}

module API.Dashboard.Users.ResetPassword.Post.Templates
  ( renderPasswordModal,
  )
where

--------------------------------------------------------------------------------

import Component.Button ((&))
import Component.Button qualified as Button
import Component.Modal qualified as Modal
import Data.Text (Text)
import Data.Text.Display (display)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified
import Lucid.Alpine

--------------------------------------------------------------------------------

-- | Render a one-time modal showing a user's newly assigned password.
--
-- Displayed on the dashboard users index after an admin assigns a password. The
-- plaintext is shown once (in a read-only input the admin can select or copy)
-- with a note that the user has been signed out of all sessions and that the
-- password will not be shown again.
renderPasswordModal ::
  UserMetadata.UserWithMetadata ->
  -- | The generated plaintext password (shown once, never persisted).
  Text ->
  Lucid.Html ()
renderPasswordModal user plaintext =
  Modal.modal Modal.Standard "open" "open = false" [xData_ "{ open: true, copied: false }"] $ do
    Lucid.h2_
      [class_ $ base [Tokens.headingLg, Tokens.fontBold, Tokens.mb4]]
      "Password Reset"

    Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.mb2]] $ do
      "The new password for "
      Lucid.span_ [class_ $ base [Tokens.fontBold]] $ Lucid.toHtml displayName
      " ("
      Lucid.toHtml email
      ") is below."

    Lucid.p_ [class_ $ base [Tokens.textSm, Tokens.fgMuted, Tokens.mb4]] $
      "Copy it now — it will not be shown again. The user has been signed out of all "
        <> "sessions and must log in with this password."

    -- Password field + copy button
    Lucid.div_ [class_ $ base ["flex", Tokens.gap2, Tokens.mb6]] $ do
      Lucid.input_
        [ Lucid.type_ "text",
          Lucid.readonly_ "readonly",
          xRef_ "pw",
          Lucid.value_ plaintext,
          Lucid.onclick_ "this.select()",
          class_ $ base ["flex-1", "min-w-0", Tokens.p4, "font-mono", Tokens.textLg, Tokens.bgAlt, Tokens.fgPrimary, Tokens.border2, Tokens.borderDefault]
        ]
      Button.render
        ( Button.defaults
            & Button.with [xOnClick_ "navigator.clipboard.writeText($refs.pw.value).then(() => { copied = true; setTimeout(() => copied = false, 2000) })"]
        )
        $ do
          Lucid.span_ [xShow_ "!copied"] "Copy"
          Lucid.span_ [xShow_ "copied"] "Copied!"

    -- Done button
    Lucid.div_ [class_ $ base ["flex", "justify-end"]] $
      Button.render (Button.defaults & Button.primary & Button.with [xOnClick_ "open = false"]) "Done"
  where
    displayName = display user.uwmDisplayName :: Text
    email = display user.uwmEmail :: Text

module API.Invite.Token.Post.Route where

--------------------------------------------------------------------------------

import Data.Either (fromRight)
import Data.Text (Text)
import Data.Text qualified as Text
import Domain.Types.Cookie (Cookie)
import Effects.Database.Tables.HostInvitation qualified as HostInvitation
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FileData,
    FromMultipart (..),
    Mem,
    MultipartForm,
    fdFileName,
    lookupFile,
    lookupInput,
  )
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Form data for the invite onboarding flow.
--
-- Combines account creation fields with initial show setup fields.
data InviteOnboardingForm = InviteOnboardingForm
  { iofFullName :: Text,
    iofEmail :: Text,
    iofDisplayName :: Text,
    iofPassword :: Text,
    iofNewsletter :: Maybe Bool,
    iofShowTitle :: Text,
    iofShowDescription :: Text,
    iofShowTags :: Maybe Text,
    iofShowLogo :: Maybe (FileData Mem)
  }
  deriving (Show)

instance FromMultipart Mem InviteOnboardingForm where
  fromMultipart multipartData =
    InviteOnboardingForm
      <$> lookupInput "full_name" multipartData
      <*> lookupInput "email" multipartData
      <*> lookupInput "display_name" multipartData
      <*> lookupInput "password" multipartData
      <*> pure (parseCheckbox (lookupInput "newsletter" multipartData))
      <*> lookupInput "show_title" multipartData
      <*> pure (fromRight "" (lookupInput "show_description" multipartData))
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "show_tags" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "show_logo" multipartData))
    where
      parseCheckbox :: Either String Text -> Maybe Bool
      parseCheckbox (Right _) = Just True
      parseCheckbox (Left _) = Nothing

      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing (Just t) | Text.null (Text.strip t) = Nothing
      emptyToNothing x = x

      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

--------------------------------------------------------------------------------

-- | "POST /invite/:token"
type Route =
  "invite"
    :> Servant.Capture "token" HostInvitation.Token
    :> Servant.Header "Cookie" Cookie
    :> Servant.RemoteHost
    :> Servant.Header "User-Agent" Text
    :> MultipartForm Mem InviteOnboardingForm
    :> Servant.Post
         '[HTML]
         ( Servant.Headers
             '[Servant.Header "Set-Cookie" Text, Servant.Header "HX-Redirect" Text]
             (Lucid.Html ())
         )

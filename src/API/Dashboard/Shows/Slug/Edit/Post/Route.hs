module API.Dashboard.Shows.Slug.Edit.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import Domain.Types.Cookie (Cookie)
import Domain.Types.Slug (Slug)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FileData,
    FromMultipart,
    Input (..),
    Mem,
    MultipartData (..),
    MultipartForm,
    fdFileName,
    fromMultipart,
    lookupFile,
    lookupInput,
  )
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Form data for show editing
data ShowEditForm = ShowEditForm
  { sefTitle :: Text,
    sefDescription :: Text,
    sefTags :: Maybe Text, -- Comma-separated tag names
    sefLogoFile :: Maybe (FileData Mem),
    sefBannerFile :: Maybe (FileData Mem),
    sefStatus :: Text,
    sefHosts :: [User.Id],
    sefSchedulesJson :: Maybe Text
  }
  deriving (Show)

-- | Schedule slot info parsed from JSON form data
data ScheduleSlotInfo = ScheduleSlotInfo
  { dayOfWeek :: Text,
    weeksOfMonth :: [Int64],
    startTime :: Text,
    endTime :: Text
  }
  deriving stock (Show, Generic, Eq)
  deriving anyclass (FromJSON, ToJSON)

instance FromMultipart Mem ShowEditForm where
  fromMultipart multipartData =
    ShowEditForm
      <$> lookupInput "title" multipartData
      <*> lookupInput "description" multipartData
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "tags" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "logo_file" multipartData))
      <*> pure (either (const Nothing) (fileDataToNothing . Just) (lookupFile "banner_file" multipartData))
      <*> lookupInput "status" multipartData
      <*> pure (parseHosts $ fromRight [] (lookupInputs "hosts" multipartData))
      <*> pure (either (const Nothing) (emptyToNothing . Just) (lookupInput "schedules_json" multipartData))
    where
      emptyToNothing :: Maybe Text -> Maybe Text
      emptyToNothing (Just "") = Nothing
      emptyToNothing (Just t) | Text.null (Text.strip t) = Nothing
      emptyToNothing x = x

      -- \| Convert empty filename FileData to Nothing
      fileDataToNothing :: Maybe (FileData Mem) -> Maybe (FileData Mem)
      fileDataToNothing (Just fileData)
        | Text.null (fdFileName fileData) = Nothing
        | otherwise = Just fileData
      fileDataToNothing Nothing = Nothing

      parseHosts :: [Text] -> [User.Id]
      parseHosts = mapMaybe parseUserId

      parseUserId :: Text -> Maybe User.Id
      parseUserId t = case Text.Read.decimal t of
        Right (n, "") -> Just (User.Id n)
        _ -> Nothing

      -- Helper to lookup all values for a given input name (for multi-select)
      lookupInputs :: Text -> MultipartData Mem -> Either String [Text]
      lookupInputs name multipart =
        Right [iValue input | input <- inputs multipart, iName input == name]

--------------------------------------------------------------------------------

type Route =
  Observability.WithSpan
    "POST /dashboard/shows/:slug/edit"
    ( "dashboard"
        :> "shows"
        :> Servant.Capture "slug" Slug
        :> "edit"
        :> Servant.Header "Cookie" Cookie
        :> MultipartForm Mem ShowEditForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Redirect" Text] (Lucid.Html ()))
    )

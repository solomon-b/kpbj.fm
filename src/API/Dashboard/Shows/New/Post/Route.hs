module API.Dashboard.Shows.New.Post.Route where

--------------------------------------------------------------------------------

import Data.Aeson (FromJSON, ToJSON)
import Data.Either (fromRight)
import Data.Int (Int64)
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Read qualified as Text.Read
import Domain.Types.Cookie (Cookie)
import Domain.Types.HxRequest (HxRequest)
import Effects.Database.Tables.User qualified as User
import Effects.Observability qualified as Observability
import GHC.Generics (Generic)
import Lucid qualified
import Servant ((:>))
import Servant qualified
import Servant.Multipart
  ( FileData,
    FromMultipart (..),
    Input (..),
    Mem,
    MultipartData (..),
    MultipartForm,
    fdFileName,
    lookupFile,
    lookupInput,
  )
import Text.HTML (HTML)

--------------------------------------------------------------------------------

-- | Form data for creating a new show
data NewShowForm = NewShowForm
  { nsfTitle :: Text,
    nsfDescription :: Text,
    nsfTags :: Maybe Text, -- Comma-separated tag names
    nsfLogoFile :: Maybe (FileData Mem),
    nsfBannerFile :: Maybe (FileData Mem),
    nsfStatus :: Text,
    nsfHosts :: [User.Id],
    nsfSchedulesJson :: Maybe Text
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

instance FromMultipart Mem NewShowForm where
  fromMultipart multipartData =
    NewShowForm
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
    "POST /dashboard/shows/new"
    ( "dashboard"
        :> "shows"
        :> "new"
        :> Servant.Header "Cookie" Cookie
        :> Servant.Header "HX-Request" HxRequest
        :> MultipartForm Mem NewShowForm
        :> Servant.Post '[HTML] (Servant.Headers '[Servant.Header "HX-Push-Url" Text] (Lucid.Html ()))
    )

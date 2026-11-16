-- | Render tests for the hints on the staged audio upload fields.
--
-- A user must see the length limit before they choose a file.
module Templates.UploadHintsSpec (spec) where

--------------------------------------------------------------------------------

import API.Dashboard.AirBreaks.BreakItems.Shared (Section (..), psaSection)
import API.Dashboard.AirBreaks.StationIds.New.Get.Templates.Form (stationIdUploadForm)
import API.Dashboard.EphemeralUploads.Id.Edit.Get.Templates.Form (ephemeralUploadEditForm)
import API.Dashboard.Shows.Slug.Promos.Get.Templates qualified as Promos
import Component.BreakItems (renderUploadForm)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Lazy qualified as LText
import Data.Time (Day, UTCTime (..), fromGregorian)
import Domain.Types.Slug (mkSlug)
import Domain.Types.StorageBackend (StorageBackend (..), defaultLocalConfig)
import Effects.Database.Tables.EphemeralUploads qualified as EphemeralUploads
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.User qualified as User
import Lucid qualified
import Test.Hspec

--------------------------------------------------------------------------------

spec :: Spec
spec = describe "Staged audio upload hints" $ do
  it "shows the 10 second limit on the station ID form" $
    render (stationIdUploadForm uploadUrl) `shouldSatisfy` Text.isInfixOf "and 10 seconds."

  it "shows the 30 second limit on the show promo form" $
    render (Promos.template backend sampleShow uploadUrl sampleDay [])
      `shouldSatisfy` Text.isInfixOf "and 30 seconds."

  it "shows the 30 second limit on the PSA form" $
    render (renderUploadForm psaSection.secUrls uploadUrl sampleDay Nothing)
      `shouldSatisfy` Text.isInfixOf "and 30 seconds."

  it "asks for an MP3 on the station ID form" $
    render (stationIdUploadForm uploadUrl) `shouldSatisfy` Text.isInfixOf "Upload an MP3."

  it "asks for an MP3 on the show promo form" $
    render (Promos.template backend sampleShow uploadUrl sampleDay [])
      `shouldSatisfy` Text.isInfixOf "Upload an MP3."

  it "asks for an MP3 on the PSA form" $
    render (renderUploadForm psaSection.secUrls uploadUrl sampleDay Nothing)
      `shouldSatisfy` Text.isInfixOf "Upload an MP3."

  it "states 500MB on the ephemeral replacement form" $
    render (ephemeralUploadEditForm uploadUrl sampleEphemeralUpload)
      `shouldSatisfy` Text.isInfixOf "Maximum 500MB."

  it "allows a 500MB file on the ephemeral replacement form" $
    render (ephemeralUploadEditForm uploadUrl sampleEphemeralUpload)
      `shouldSatisfy` Text.isInfixOf "File exceeds 500MB limit"

  it "replaces the default text when a field has a hint" $
    render (stationIdUploadForm uploadUrl) `shouldNotSatisfy` Text.isInfixOf "MP3 only"

--------------------------------------------------------------------------------

render :: Lucid.Html () -> Text
render = LText.toStrict . Lucid.renderText

uploadUrl :: Text
uploadUrl = "/api/uploads/audio"

backend :: StorageBackend
backend = LocalStorage defaultLocalConfig

sampleDay :: Day
sampleDay = fromGregorian 2026 1 1

sampleEphemeralUpload :: EphemeralUploads.Model
sampleEphemeralUpload =
  EphemeralUploads.EphemeralUpload
    { EphemeralUploads.eumId = EphemeralUploads.Id 1,
      EphemeralUploads.eumTitle = "Sample Clip",
      EphemeralUploads.eumDescription = "A sample clip.",
      EphemeralUploads.eumAudioFilePath = "audio/ephemeral/sample.mp3",
      EphemeralUploads.eumMimeType = "audio/mpeg",
      EphemeralUploads.eumFileSize = 1024,
      EphemeralUploads.eumDurationSeconds = 600,
      EphemeralUploads.eumCreatorId = User.Id 1,
      EphemeralUploads.eumCreatedAt = UTCTime sampleDay 0,
      EphemeralUploads.eumFlaggedAt = Nothing,
      EphemeralUploads.eumFlaggedBy = Nothing,
      EphemeralUploads.eumFlagReason = Nothing
    }

sampleShow :: Shows.Model
sampleShow =
  Shows.Show
    { Shows.id = Shows.Id 1,
      Shows.title = "Sample Show",
      Shows.slug = mkSlug "Sample Show",
      Shows.description = Nothing,
      Shows.logoUrl = Nothing,
      Shows.status = Shows.Active,
      Shows.createdAt = UTCTime sampleDay 0,
      Shows.updatedAt = UTCTime sampleDay 0,
      Shows.deletedAt = Nothing
    }

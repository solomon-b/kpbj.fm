module API.Dashboard.Events.Slug.Get.Templates.Page where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design.StyleBuilder.Internal (cls)
import Design.Tokens qualified as Tokens
import Effects.Database.Tables.EventTags qualified as EventTags
import Effects.Database.Tables.Events qualified as Events
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

-- | Event detail template
template ::
  Events.Model ->
  [EventTags.Model] ->
  Maybe UserMetadata.Model ->
  Lucid.Html ()
template event tags mAuthor = do
  Lucid.div_ [Lucid.class_ Tokens.fullWidth] $ do
    -- Header with title
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb6]] $ do
      Lucid.div_ [Lucid.class_ Tokens.mb4] $ do
        Lucid.h2_ [Lucid.class_ $ cls [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] $
          Lucid.toHtml event.emTitle
        renderStatusBadge event.emStatus

      -- Metadata
      Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm, Tokens.textGray600, "mt-4", "pt-4", "border-t", "border-gray-200"]] $ do
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ Tokens.fontBold] "Organizer: "
          case mAuthor of
            Just author -> Lucid.toHtml author.mDisplayName
            Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400"] "Unknown"
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ Tokens.fontBold] "Created: "
          Lucid.toHtml $ formatDateTime event.emCreatedAt
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ Tokens.fontBold] "Updated: "
          Lucid.toHtml $ formatDateTime event.emUpdatedAt

    -- Date & Time
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb6]] $ do
      Lucid.h3_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Date & Time"
      Lucid.div_ [Lucid.class_ $ cls ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm]] $ do
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textGray600]] "Start: "
          Lucid.toHtml $ formatDateTimeFull event.emStartsAt
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ $ cls [Tokens.fontBold, Tokens.textGray600]] "End: "
          Lucid.toHtml $ formatDateTimeFull event.emEndsAt

    -- Location
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb6]] $ do
      Lucid.h3_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Location"
      Lucid.div_ [Lucid.class_ Tokens.textSm] $ do
        Lucid.p_ [Lucid.class_ Tokens.fontBold] $
          Lucid.toHtml event.emLocationName
        Lucid.p_ [Lucid.class_ $ cls [Tokens.textGray600, "mt-1"]] $
          Lucid.toHtml event.emLocationAddress

    -- Tags
    if null tags
      then mempty
      else Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6, Tokens.mb6]] $ do
        Lucid.h3_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Tags"
        Lucid.div_ [Lucid.class_ $ cls ["flex", "flex-wrap", Tokens.gap2]] $
          mapM_ renderTag tags

    -- Description
    Lucid.div_ [Lucid.class_ $ cls [Tokens.bgWhite, Tokens.cardBorder, Tokens.p6]] $ do
      Lucid.h3_ [Lucid.class_ $ cls [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Description"
      Lucid.div_ [Lucid.class_ $ cls ["prose", "prose-sm", "max-w-none", Tokens.textGray700, "whitespace-pre-wrap"]] $
        Lucid.toHtml $
          truncateContent 2000 event.emDescription

renderStatusBadge :: Events.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Events.Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Events.Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")

  Lucid.span_
    [Lucid.class_ $ cls ["inline-block", "px-3", "py-1", Tokens.textSm, Tokens.fontBold, "rounded", bgClass, textClass]]
    $ Lucid.toHtml statusText

renderTag :: EventTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.span_
    [Lucid.class_ $ cls ["px-3", "py-1", Tokens.textSm, "bg-gray-200", "rounded"]]
    $ Lucid.toHtml tag.etmName

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

formatDateTimeFull :: UTCTime -> String
formatDateTimeFull = formatTime defaultTimeLocale "%b %d, %Y at %H:%M"

truncateContent :: Int -> Text -> Text
truncateContent maxLen content
  | Text.length content <= maxLen = content
  | otherwise = Text.take maxLen content <> "..."

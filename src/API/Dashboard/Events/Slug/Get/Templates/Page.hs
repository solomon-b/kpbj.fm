{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.Events.Slug.Get.Templates.Page where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
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
  Lucid.div_ [Lucid.class_ "w-full"] $ do
    -- Header with title
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
      Lucid.div_ [Lucid.class_ "mb-4"] $ do
        Lucid.h2_ [Lucid.class_ "text-2xl font-bold mb-2"] $
          Lucid.toHtml event.emTitle
        renderStatusBadge event.emStatus

      -- Metadata
      Lucid.div_ [Lucid.class_ "grid grid-cols-2 gap-4 text-sm text-gray-600 mt-4 pt-4 border-t border-gray-200"] $ do
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "Organizer: "
          case mAuthor of
            Just author -> Lucid.toHtml author.mDisplayName
            Nothing -> Lucid.span_ [Lucid.class_ "text-gray-400"] "Unknown"
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "Created: "
          Lucid.toHtml $ formatDateTime event.emCreatedAt
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ "font-bold"] "Updated: "
          Lucid.toHtml $ formatDateTime event.emUpdatedAt

    -- Date & Time
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-3"] "Date & Time"
      Lucid.div_ [Lucid.class_ "grid grid-cols-2 gap-4 text-sm"] $ do
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ "font-bold text-gray-600"] "Start: "
          Lucid.toHtml $ formatDateTimeFull event.emStartsAt
        Lucid.div_ [] $ do
          Lucid.span_ [Lucid.class_ "font-bold text-gray-600"] "End: "
          Lucid.toHtml $ formatDateTimeFull event.emEndsAt

    -- Location
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-3"] "Location"
      Lucid.div_ [Lucid.class_ "text-sm"] $ do
        Lucid.p_ [Lucid.class_ "font-bold"] $
          Lucid.toHtml event.emLocationName
        Lucid.p_ [Lucid.class_ "text-gray-600 mt-1"] $
          Lucid.toHtml event.emLocationAddress

    -- Tags
    if null tags
      then mempty
      else Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6 mb-6"] $ do
        Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-3"] "Tags"
        Lucid.div_ [Lucid.class_ "flex flex-wrap gap-2"] $
          mapM_ renderTag tags

    -- Description
    Lucid.div_ [Lucid.class_ "bg-white border-2 border-gray-800 p-6"] $ do
      Lucid.h3_ [Lucid.class_ "text-lg font-bold mb-3"] "Description"
      Lucid.div_ [Lucid.class_ "prose prose-sm max-w-none text-gray-700 whitespace-pre-wrap"] $
        Lucid.toHtml $
          truncateContent 2000 event.emDescription

renderStatusBadge :: Events.Status -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Events.Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Events.Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml statusText

renderTag :: EventTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.span_
    [Lucid.class_ "px-3 py-1 text-sm bg-gray-200 rounded"]
    $ Lucid.toHtml tag.etmName

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y"

formatDateTimeFull :: UTCTime -> String
formatDateTimeFull = formatTime defaultTimeLocale "%b %d, %Y at %H:%M"

truncateContent :: Int -> Text -> Text
truncateContent maxLen content
  | Text.length content <= maxLen = content
  | otherwise = Text.take maxLen content <> "..."

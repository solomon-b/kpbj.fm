{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module API.Dashboard.StationBlog.Slug.Get.Templates.Page where

--------------------------------------------------------------------------------

import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Time (UTCTime, defaultTimeLocale, formatTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.PostStatus (BlogPostStatus (..))
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Lucid qualified

-- | Station blog post detail template
template ::
  BlogPosts.Model ->
  [BlogTags.Model] ->
  Maybe UserMetadata.Model ->
  Lucid.Html ()
template post tags mAuthor = do
  Lucid.div_ [class_ $ base [Tokens.fullWidth]] $ do
    -- Header with title
    Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded", Tokens.p6, Tokens.mb6]] $ do
      Lucid.div_ [class_ $ base [Tokens.mb4]] $ do
        Lucid.h2_ [class_ $ base [Tokens.text2xl, Tokens.fontBold, Tokens.mb2]] $
          Lucid.toHtml post.bpmTitle
        renderStatusBadge post.bpmStatus

      -- Metadata
      Lucid.div_ [class_ $ base ["grid", "grid-cols-2", Tokens.gap4, Tokens.textSm, Tokens.textGray600, "mt-4", "pt-4", "border-t", "border-gray-200 dark:border-gray-600"]] $ do
        Lucid.div_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold]] "Author: "
          case mAuthor of
            Just author -> Lucid.toHtml author.mDisplayName
            Nothing -> Lucid.span_ [class_ $ base ["text-gray-400 dark:text-gray-500"]] "Unknown"
        Lucid.div_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold]] "Created: "
          Lucid.toHtml $ formatDateTime post.bpmCreatedAt
        Lucid.div_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold]] "Updated: "
          Lucid.toHtml $ formatDateTime post.bpmUpdatedAt
        Lucid.div_ [] $ do
          Lucid.span_ [class_ $ base [Tokens.fontBold]] "Published: "
          case post.bpmPublishedAt of
            Just pubAt -> Lucid.toHtml $ formatDateTime pubAt
            Nothing -> Lucid.span_ [class_ $ base ["text-gray-400 dark:text-gray-500"]] "Not published"

    -- Tags
    if null tags
      then mempty
      else Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded", Tokens.p6, Tokens.mb6]] $ do
        Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Tags"
        Lucid.div_ [class_ $ base ["flex", "flex-wrap", Tokens.gap2]] $
          mapM_ renderTag tags

    -- Excerpt
    case post.bpmExcerpt of
      Just excerpt | not (Text.null excerpt) ->
        Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded", Tokens.p6, Tokens.mb6]] $ do
          Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Excerpt"
          Lucid.p_ [class_ $ base [Tokens.textGray600]] $
            Lucid.toHtml excerpt
      _ -> mempty

    -- Content preview
    Lucid.div_ [class_ $ base [Tokens.bgWhite, "rounded", Tokens.p6]] $ do
      Lucid.h3_ [class_ $ base [Tokens.textLg, Tokens.fontBold, "mb-3"]] "Content"
      Lucid.div_ [class_ $ base ["prose", "prose-sm", "max-w-none", Tokens.textGray600, "whitespace-pre-wrap"]] $
        Lucid.toHtml $
          truncateContent 2000 post.bpmContent

renderStatusBadge :: BlogPostStatus -> Lucid.Html ()
renderStatusBadge status = do
  let (bgClass, textClass, statusText) = case status of
        Published -> ("bg-green-100", "text-green-800", "Published") :: (Text, Text, Text)
        Draft -> ("bg-yellow-100", "text-yellow-800", "Draft")
        Deleted -> ("bg-gray-100 dark:bg-gray-700", "text-gray-800 dark:text-gray-200", "Deleted")

  Lucid.span_
    [Lucid.class_ [i|inline-block px-3 py-1 text-sm font-bold rounded #{bgClass} #{textClass}|]]
    $ Lucid.toHtml statusText

renderTag :: BlogTags.Model -> Lucid.Html ()
renderTag tag =
  Lucid.span_
    [Lucid.class_ "px-3 py-1 text-sm bg-gray-200 rounded"]
    $ Lucid.toHtml tag.btmName

formatDateTime :: UTCTime -> String
formatDateTime = formatTime defaultTimeLocale "%b %d, %Y at %H:%M"

truncateContent :: Int -> Text -> Text
truncateContent maxLen content
  | Text.length content <= maxLen = content
  | otherwise = Text.take maxLen content <> "..."

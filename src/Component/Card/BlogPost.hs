{-# LANGUAGE QuasiQuotes #-}

module Component.Card.BlogPost
  ( -- * Generic card renderer
    renderBlogPostCard,
    BlogPostCardData (..),

    -- * Model-specific wrappers
    renderStationBlogPostCard,
    renderShowBlogPostCard,
  )
where

--------------------------------------------------------------------------------

import API.Links (blogLinks, showBlogLinks)
import API.Types
import Data.String.Interpolate (i)
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Text.Display (display)
import Data.Time (UTCTime)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Domain.Types.Slug (Slug)
import Domain.Types.StorageBackend (StorageBackend, buildMediaUrl)
import Effects.Database.Tables.BlogPosts qualified as BlogPosts
import Effects.Database.Tables.BlogTags qualified as BlogTags
import Effects.Database.Tables.ShowBlogPosts qualified as ShowBlogPosts
import Effects.Database.Tables.Shows qualified as Shows
import Effects.Database.Tables.UserMetadata qualified as UserMetadata
import Helpers.RelativeTime (formatRelativeTime)
import Lucid qualified
import Lucid.HTMX
import Servant.Links qualified as Links

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Generic card data and renderer

-- | Data needed to render a blog post card.
data BlogPostCardData = BlogPostCardData
  { cardUrl :: Text,
    cardTitle :: Text,
    cardExcerpt :: Maybe Text,
    cardContent :: Text,
    cardHeroImageUrl :: Maybe Text,
    cardAuthorName :: Maybe Text,
    cardPublishedAt :: Maybe UTCTime,
    cardCurrentTime :: Maybe UTCTime
  }

-- | Render a blog post card from generic data.
renderBlogPostCard :: StorageBackend -> BlogPostCardData -> Lucid.Html ()
renderBlogPostCard backend card = do
  Lucid.a_
    [ Lucid.href_ card.cardUrl,
      hxGet_ card.cardUrl,
      hxTarget_ "#main-content",
      hxPushUrl_ "true",
      class_ $ base ["block"]
    ]
    $ do
      Lucid.article_ [class_ $ base [Tokens.cardBase, Tokens.mb6]] $ do
        -- Post title
        Lucid.h2_ [class_ $ base [Tokens.heading2xl, Tokens.mb4, "leading-tight"]] $
          Lucid.toHtml card.cardTitle

        -- Hero image (if present)
        case card.cardHeroImageUrl of
          Just heroImageUrl ->
            Lucid.div_ [Lucid.class_ Tokens.mb4] $
              Lucid.img_
                [ Lucid.src_ (buildMediaUrl backend heroImageUrl),
                  Lucid.alt_ card.cardTitle,
                  class_ $ base [Tokens.fullWidth, "h-auto", "border", "border-gray-300"]
                ]
          Nothing -> pure ()

        -- Excerpt
        let excerptText = case card.cardExcerpt of
              Just exc -> exc
              Nothing ->
                let truncated = Text.take 200 card.cardContent
                 in truncated <> if Text.length card.cardContent > 200 then "..." else ""
        Lucid.p_ [class_ $ base [Tokens.textGray700, "leading-relaxed"]] $
          Lucid.toHtml excerptText

        -- Author row: name + relative time
        Lucid.div_ [class_ $ base ["flex", "items-center", "justify-between", Tokens.mt4]] $ do
          case card.cardAuthorName of
            Just authorName ->
              Lucid.span_ [class_ $ base [Tokens.fontBold, Tokens.textGray800]] $
                Lucid.toHtml authorName
            Nothing -> mempty
          Lucid.span_ [class_ $ base [Tokens.textXs, Tokens.textGray600]] $
            Lucid.toHtml $ case (card.cardCurrentTime, card.cardPublishedAt) of
              (Just now, Just publishedAt) -> formatRelativeTime now publishedAt
              (Nothing, Just _) -> "" -- No relative time without current time
              _ -> "Draft"

--------------------------------------------------------------------------------
-- Station blog wrapper

blogPostGetUrl :: BlogPosts.Id -> Slug -> Links.URI
blogPostGetUrl postId slug = Links.linkURI $ blogLinks.postWithSlug postId slug

-- | Render a station blog post card.
renderStationBlogPostCard :: StorageBackend -> UTCTime -> (BlogPosts.Model, UserMetadata.Model, [BlogTags.Model]) -> Lucid.Html ()
renderStationBlogPostCard backend currentTime (post, author, _tags) = do
  let url = [i|/#{blogPostGetUrl (BlogPosts.bpmId post) (BlogPosts.bpmSlug post)}|]
  renderBlogPostCard
    backend
    BlogPostCardData
      { cardUrl = url,
        cardTitle = BlogPosts.bpmTitle post,
        cardExcerpt = BlogPosts.bpmExcerpt post,
        cardContent = BlogPosts.bpmContent post,
        cardHeroImageUrl = BlogPosts.bpmHeroImageUrl post,
        cardAuthorName = Just (display (UserMetadata.mDisplayName author)),
        cardPublishedAt = BlogPosts.bpmPublishedAt post,
        cardCurrentTime = Just currentTime
      }

--------------------------------------------------------------------------------
-- Show blog wrapper

showBlogPostGetUrl :: Shows.Id -> ShowBlogPosts.Id -> Slug -> Links.URI
showBlogPostGetUrl showId postId slug = Links.linkURI $ showBlogLinks.postWithSlug showId postId slug

-- | Render a show blog post card.
renderShowBlogPostCard :: StorageBackend -> Shows.Model -> ShowBlogPosts.Model -> Lucid.Html ()
renderShowBlogPostCard backend showModel post = do
  let url = [i|/#{showBlogPostGetUrl (Shows.id showModel) (ShowBlogPosts.id post) (ShowBlogPosts.slug post)}|]
  renderBlogPostCard
    backend
    BlogPostCardData
      { cardUrl = url,
        cardTitle = ShowBlogPosts.title post,
        cardExcerpt = ShowBlogPosts.excerpt post,
        cardContent = ShowBlogPosts.content post,
        cardHeroImageUrl = Nothing, -- Show blog posts don't have hero images
        cardAuthorName = Nothing, -- Show is implicit from context
        cardPublishedAt = ShowBlogPosts.publishedAt post,
        cardCurrentTime = Nothing
      }

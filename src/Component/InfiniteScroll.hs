-- | Infinite scroll component using HTMX.
--
-- Provides sentinel-based infinite scroll that triggers loading when
-- the user scrolls near the bottom of the list. Falls back to traditional
-- pagination for browsers without JavaScript.
module Component.InfiniteScroll
  ( -- * Sentinel Element
    renderSentinel,

    -- * Loading Indicator
    renderLoadingIndicator,

    -- * End of Content
    renderEndOfContent,
  )
where

--------------------------------------------------------------------------------

import Data.Text (Text)
import Design (base, class_)
import Design.Tokens qualified as Tokens
import Lucid qualified
import Lucid.HTMX

--------------------------------------------------------------------------------

-- | Render the sentinel element that triggers infinite scroll.
--
-- The sentinel uses HTMX's 'revealed' trigger which fires when the element
-- enters the viewport. Each response should include a new sentinel with the
-- updated next page URL to create a chain of automatic loading.
--
-- Example:
--
-- > renderSentinel "/blog?page=2" "#blog-posts-list"
renderSentinel ::
  -- | URL for the next page of items
  Text ->
  -- | CSS selector for the container to append items to
  Text ->
  Lucid.Html ()
renderSentinel nextPageUrl targetSelector =
  Lucid.div_
    [ Lucid.id_ "load-more-sentinel",
      hxGet_ nextPageUrl,
      hxTrigger_ "revealed",
      hxTarget_ targetSelector,
      hxSwap_ "beforeend",
      hxIndicator_ "#loading-indicator",
      class_ $ base ["h-1", "col-span-full"] -- Span all grid columns to avoid blank spots
    ]
    mempty

-- | Render the loading indicator shown during HTMX requests.
--
-- Uses HTMX's built-in indicator system - the element is hidden by default
-- and shown automatically when a request using this indicator is in flight.
-- Requires CSS: @.htmx-indicator { display: none; } .htmx-request .htmx-indicator { display: flex; }@
renderLoadingIndicator :: Lucid.Html ()
renderLoadingIndicator =
  Lucid.div_
    [ Lucid.id_ "loading-indicator",
      class_ $ base ["htmx-indicator", "flex", "justify-center", "py-8"]
    ]
    $ Lucid.div_ [class_ $ base ["flex", "items-center", "gap-2"]]
    $ do
      -- Simple spinner using CSS animation
      Lucid.div_
        [ class_ $
            base
              [ "w-5",
                "h-5",
                "border-2",
                "border-gray-300",
                "border-t-gray-600",
                "rounded-full",
                "animate-spin"
              ]
        ]
        mempty
      Lucid.span_ "Loading more..."

-- | Render the end-of-content indicator when there are no more items to load.
--
-- This replaces the sentinel when hasMore = False, signaling to users
-- that they've reached the end of the list.
renderEndOfContent :: Lucid.Html ()
renderEndOfContent =
  Lucid.div_
    [ Lucid.id_ "end-of-content",
      class_ $ base ["text-center", "py-8", Tokens.fgMuted, "col-span-full"]
    ]
    mempty

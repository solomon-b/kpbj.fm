# KPBJ Events System Architecture Analysis

**Analysis Date:** September 27, 2025  
**Focus:** Understanding current events system to plan WeekView integration  

## Architecture Overview

The KPBJ events system is built using a clean layered architecture with strong type safety and separation of concerns:

### Main Components and Responsibilities

1. **API Handler Layer** (`API/Events/Get.hs`):
   - Handles HTTP requests with query parameters for filtering and view switching
   - Manages HTMX vs. full page rendering logic  
   - Orchestrates database queries and template rendering
   - Provides type-safe route link generation

2. **Database Layer** (`Effects/Database/Tables/Events.hs`):
   - Defines domain models (`EventModel`, `EventTagModel`) and API types
   - Provides SQL queries using Hasql with compile-time safety
   - Handles event filtering by tags and date ranges
   - Manages event status (draft/published) and tag assignments

3. **Domain Types** (`Domain/Types/PageView.hs`):
   - Defines view enumeration: `ListView | MonthView Year MonthOfYear | WeekView`
   - Provides Servant instances for URL parameter parsing/generation
   - Contains view-specific helper functions like `isMonthView`

4. **Component System** (`Component/Frame.hs`):
   - Provides site-wide template with persistent navigation and music player
   - Handles authenticated vs. unauthenticated user states
   - Uses HTMX for progressive enhancement with `#main-content` target

### Data Flow and Component Interactions

```
HTTP Request → Servant Route → Events.Get.handler
    ↓
Database Query (tag filter + view type) → Events Table
    ↓  
Template Rendering (List/Month/Week) → Lucid HTML
    ↓
HTMX Response OR Full Page (with Frame.template)
```

**Key Flow Details:**
- `eventsGetLink` generates type-safe URLs with optional tag and view parameters
- Handler dispatches to `renderListTemplate` vs `renderMonthTemplate` based on PageView
- HTMX requests return content-only; regular requests wrap in site frame
- View controls use HTMX to swap `#events-content-container` innerHTML

### Key Architectural Decisions

1. **Type-Safe Routing**: All URLs generated through Servant's `safeLink` system prevents broken links
2. **HTMX Progressive Enhancement**: Graceful degradation with full page fallbacks
3. **View State in URL**: PageView query parameter maintains state across navigation
4. **Component-Based Templates**: Shared header/controls with swappable content sections
5. **Database Query Abstraction**: Hasql provides compile-time query validation

## Implementation Patterns

### Naming Conventions and Code Organization

**File Structure Pattern:**
```
API/
  Events/
    Get.hs           # Main events listing handler
    Event/
      Get.hs         # Individual event detail handler
    New/
      Get.hs         # Event creation form
      Post.hs        # Event creation handler
```

**Function Naming:**
- URL generators: `eventsGetUrl`, `eventGetUrl` (top-level URI functions)
- Link functions: `eventsGetLink`, `eventGetLink` (in API.hs)  
- Handlers: `handler` (main entry point per module)
- Templates: `renderListTemplate`, `renderMonthTemplate`, `renderListContent`

### Common Patterns

**1. Safe Link Generation Pattern** (`API/Events/Get.hs:49-60`):
```haskell
eventsGetUrl :: Links.URI
eventsGetUrl = Links.linkURI $ eventsGetLink Nothing Nothing

eventsGetMonthUrl :: Year -> MonthOfYear -> Maybe Text -> Links.URI
eventsGetMonthUrl year month maybeTag =
  Links.linkURI $ eventsGetLink maybeTag (Just $ MonthView year month)
```

**2. HTMX Progressive Enhancement Pattern** (`API/Events/Get.hs:251-269`):
```haskell
Lucid.a_
  [ Lucid.href_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just ListView)}|],
    hxGet_ [i|/#{Links.linkURI $ eventsGetLink maybeTagFilter (Just ListView)}|],
    hxTarget_ "#main-content",
    hxPushUrl_ "true",
    Lucid.class_ listClasses
  ]
  "LIST"
```

**3. Template Factory Pattern** (`API/Events/Get.hs:397-415`):
```haskell
template <-
  case view of
    MonthView year month -> do
      monthEvents <- execQuerySpan (Events.getEventsForMonth tagFilter year month)
      renderMonthTemplate year month tagFilter eventTagsWithCounts monthEvents
    _ -> do
      events <- execQuerySpan (Events.getPublishedEvents tagFilter limit offset)
      renderListTemplate (utcTimeToYearMonth now) tagFilter eventTagsWithCounts events
```

### Error Handling Approaches

**Database Query Error Pattern** (`API/Events/Get.hs:492-498`):
```haskell
renderListTemplate currentMonth maybeTagFilter eventTagsWithCounts = \case
  Left err -> do
    Log.logAttention "Failed to fetch events from database" (Aeson.object ["error" .= show err])
    pure (errorTemplate "Failed to load events. Please try again.")
  Right events -> pure $ do
    header ListView maybeTagFilter currentMonth eventTagsWithCounts
    renderListContent events
```

### Testing Patterns

The codebase uses Cabal's built-in test framework. Key testing concepts:
- Database operations are abstracted through effects for testability
- Handlers are pure functions that can be unit tested
- Template rendering produces `Lucid.Html ()` for output validation

### Configuration and Setup Patterns

**Environment-Based Configuration:**
- Database pool injection via `Has HSQL.Pool.Pool env`
- Tracer for observability via `Has Tracer env`
- Effect constraints define handler capabilities

## Key Definitions & Interfaces

### Core Types and Data Structures

**PageView Type** (`Domain/Types/PageView.hs:12-13`):
```haskell
data PageView = ListView | MonthView Year MonthOfYear | WeekView
  deriving (Show, Eq)
```

**EventModel** (`Effects/Database/Tables/Events.hs:96-112`):
```haskell
data EventModel = EventModel
  { emId :: EventId,
    emTitle :: Text,
    emSlug :: Text,
    emDescription :: Text,
    emStartsAt :: UTCTime,
    emEndsAt :: UTCTime,
    emLocationName :: Text,
    emLocationAddress :: Text,
    emStatus :: EventStatus,
    emAuthorId :: User.Id,
    emCreatedAt :: UTCTime,
    emUpdatedAt :: UTCTime
  }
```

**CalendarDay Type** (`API/Events/Get.hs:317-321`):
```haskell
data CalendarDay = CalendarDay
  { cdDay :: Int,
    cdIsCurrentMonth :: Bool,
    cdEvents :: [Events.EventModel]
  }
```

### Key Function Signatures

**Main Handler** (`API/Events/Get.hs:368-388`):
```haskell
handler ::
  ( Has Tracer env,
    Log.MonadLog m,
    MonadReader env m,
    MonadUnliftIO m,
    MonadCatch m,
    MonadIO m,
    MonadDB m,
    Has HSQL.Pool.Pool env,
    MonadClock m
  ) =>
  Tracer ->
  Maybe Text ->         -- Tag Query Param
  Maybe PageView ->     -- Page View Query Param  
  Maybe Text ->         -- Cookie
  Maybe HxRequest ->    -- @hx-request@ header
  m (Lucid.Html ())
```

**Database Queries** (`Effects/Database/Tables/Events.hs:235-248`):
```haskell
getPublishedEvents :: Maybe Text -> Int64 -> Int64 -> Hasql.Statement () [EventModel]
getEventsForMonth :: Maybe Text -> Year -> MonthOfYear -> Hasql.Statement () [EventModel]
getEventTagsWithCounts :: Hasql.Statement () [EventTagWithCount]
```

### Configuration Objects and Properties

**Route Configuration** (`API/Events/Get.hs:64-73`):
```haskell
type Route =
  Observability.WithSpan
    "GET /events"
    ( "events"
        :> Servant.QueryParam "tag" Text
        :> Servant.QueryParam "view" PageView
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" HxRequest
        :> Servant.Get '[HTML] (Lucid.Html ())
    )
```

**Database Schema** (from migration `20250922213552_create_events_tables.sql`):
- `events` table: Core event data with timestamptz fields
- `event_tags` table: Tag definitions  
- `event_tag_assignments` table: Many-to-many relationship
- Indexes on status, starts_at, slug, author_id for performance

### Important Constants and Enums

**EventStatus** (`Effects/Database/Tables/Events.hs:28-34`):
```haskell
data EventStatus = Draft | Published
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)
```

**HxRequest Detection** (`API/Events/Get.hs:75-81`):
```haskell
data HxRequest = IsHxRequest | IsNotHxRequest
  deriving (Show)

instance Servant.FromHttpApiData HxRequest where
  parseQueryParam = \case
    "true" -> Right IsHxRequest
    _ -> Right IsNotHxRequest
```

## Current Events System Implementation

### ListView Implementation

**Template Structure:**
- `header` function renders controls and filters
- `renderListContent` displays event cards in chronological order  
- Each event card includes image placeholder, metadata, description preview
- HTMX links for progressive navigation to individual events

**Key Features:**
- Pagination support (currently shows 50 events with offset)
- Tag filtering via dropdown with event counts
- Event cards show: title, date/time, location, description preview, tags

### MonthView Implementation

**Calendar Generation** (`API/Events/Get.hs:324-361`):
- `generateCalendarGrid` creates 6-week grid (42 days)
- Handles month transitions and previous/next month padding
- `eventsForDay` filters events by calendar day
- Each calendar cell shows day number + first event (truncated)

**Template Structure:**
- Month navigation with previous/next buttons
- 7-column grid layout (SUN-SAT header row)
- Calendar cells with different styling for current vs. adjacent months
- Event display truncated to first event per day

### View Switching Mechanism

**URL Pattern:**
- `?view=list` - Default chronological listing
- `?view=month-2024-4` - Monthly calendar for April 2024
- `?view=week` - Weekly view (WeekView constructor exists but not implemented)

**HTMX Integration:**
- View control buttons trigger `hx-get` with `hx-target="#main-content"`
- `hx-push-url="true"` updates browser URL
- `hx-swap="innerHTML"` replaces content container
- Falls back to full page navigation without JavaScript

**Control Rendering** (`API/Events/Get.hs:236-298`):
- Shared `renderViewControls` function used by all view templates
- Buttons dynamically styled based on current view state
- Tag filter dropdown triggers HTMX content updates
- Current month context passed for month view links

## Extension Points for WeekView

### Required Implementation Steps

1. **Extend PageView Parsing** (`Domain/Types/PageView.hs:20-34`):
   ```haskell
   -- Current WeekView parsing needs week parameters
   ["week", yearText, weekText] -> 
     let (year, week) = (read @Year $ Text.unpack yearText, read @Int $ Text.unpack weekText)
      in Right $ WeekView year week
   ```

2. **Add Week-Specific Database Query** (`Effects/Database/Tables/Events.hs`):
   ```haskell
   getEventsForWeek :: Maybe Text -> Year -> Int -> Hasql.Statement () [EventModel]
   -- WHERE starts_at >= week_start AND starts_at < week_end
   ```

3. **Implement Week Calendar Logic** (`API/Events/Get.hs`):
   ```haskell
   generateWeekGrid :: Year -> Int -> [Events.EventModel] -> [CalendarDay]
   -- 7-day grid for specific week of year
   ```

4. **Add renderWeekTemplate Function**:
   ```haskell
   renderWeekTemplate :: Year -> Int -> Maybe Text -> [Events.EventTagWithCount] -> 
                        Either err [Events.EventModel] -> m (Lucid.Html ())
   ```

5. **Update Handler Dispatch** (`API/Events/Get.hs:397-404`):
   ```haskell
   case view of
     MonthView year month -> renderMonthTemplate...  
     WeekView year week -> renderWeekTemplate...
     _ -> renderListTemplate...
   ```

### Architectural Integration Guidelines

**Follow Existing Patterns:**
- Use same error handling pattern as month view
- Generate week navigation URLs using safe links
- Implement HTMX progressive enhancement
- Share `renderViewControls` component
- Use consistent CSS classes and layout structure

**Database Query Strategy:**
- Follow `getEventsForMonth` pattern using week calculation
- Use PostgreSQL's `EXTRACT(week FROM starts_at)` functionality
- Consider ISO week numbering vs. simple week calculation
- Maintain tag filtering capability

**Template Structure:**
- Header with week navigation (Previous Week / Next Week)
- 7-column grid similar to month view but single row
- Larger cells to accommodate multiple events per day
- Event display similar to month view with truncation

---

**WeekView Wireframe Reference:** The wireframes show a week view at `wireframes/events.html:399-447` with:
- Week date range header "WEEK OF APRIL 6 - 12, 2024"  
- 7-column responsive grid (stacks on mobile)
- Each day shows date and list of events with times
- "No events" state for empty days
- Consistent styling with month view calendar

This analysis provides the complete architectural foundation needed to implement WeekView following KPBJ's established patterns and conventions.

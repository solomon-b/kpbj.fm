# Show Profiles Architecture Analysis

*Analysis Date: 2025-09-27*  
*Target: KPBJ.fm Community Radio Show Management System*

## Architecture Overview

The KPBJ.fm system is built using Haskell with the Servant web framework, featuring server-rendered HTML using Lucid and progressive enhancement with HTMX. The architecture follows a modular design with strict separation between API handlers, database effects, domain types, and UI components.

### Core System Components

**API Layer**: Type-safe routing with Servant where each route is organized as `src/API/{Feature}/{Action}.hs`. Routes support both traditional page loads and HTMX partial updates through content negotiation.

**Database Layer**: PostgreSQL with Hasql for type-safe queries. Effects are abstracted through the `Effects.Database` system, with table-specific modules containing all database operations for each entity.

**Authentication**: JWT-based session management with a hierarchical role system: User → Host → Staff → Admin. Each role has specific privileges that cascade upward.

**Rendering**: Server-side rendering with Lucid HTML library, using qualified imports (`Lucid.div_`, `Lucid.input_`, etc.) and safe link generation through Servant's type-safe URL construction.

### Show Management Domain

Based on wireframe analysis, the show management system requires:

**Shows**: Central entity representing radio programs with metadata (title, description, genre, schedule, host assignments)

**Episodes**: Individual recordings/broadcasts linked to shows, containing audio files, track listings, and metadata

**Hosts**: Users with Host role or higher who can manage shows and create episodes

**Audio Files**: Uploaded content stored in `/tmp` with S3-style paths, including metadata extraction

## Implementation Patterns

### CPS Error Handling Pattern

The codebase uses continuation-passing style for error handling, demonstrated in `API.Blog.New.Post.handler`:

```haskell
handler ::
  ( Has Tracer env, Log.MonadLog m, MonadReader env m,
    MonadUnliftIO m, MonadCatch m, MonadIO m, MonadDB m,
    Has HSQL.Pool.Pool env ) =>
  Tracer -> Maybe Text -> Maybe Text -> NewBlogPostForm -> m (Lucid.Html ())
handler _tracer cookie hxRequest form = do
  let isHtmxRequest = checkHtmxRequest hxRequest

  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn ->
      renderWithoutAuth isHtmxRequest loginRequiredTemplate
    Auth.IsLoggedIn user -> do
      execQuerySpan (UserMetadata.getUserMetadata (User.mId user)) >>= \case
        Right (Just userMetadata) ->
          case UserMetadata.mUserRole userMetadata of
            role | UserMetadata.isStaffOrHigher role ->
              case validateNewBlogPost form (UserMetadata.mUserId userMetadata) of
                Left errorMsg ->
                  renderWithUserAuth isHtmxRequest userMetadata (errorTemplate errorMsg)
                Right blogPostData ->
                  handlePostCreation isHtmxRequest userMetadata blogPostData form
            _ ->
              renderWithoutAuth isHtmxRequest permissionDeniedTemplate
        _ ->
          renderWithoutAuth isHtmxRequest userMetadataErrorTemplate
```

**Key Characteristics**:
- Each operation returns either success continuation or error handling
- Database operations wrapped in `execQuerySpan` return `Either DBError Success`
- Authentication checked via `Auth.userLoginState` pattern matching
- Role-based authorization using `UserMetadata.isStaffOrHigher`
- HTMX-aware rendering with `renderWithUserAuth`/`renderWithoutAuth`

### Database Effects Architecture

Database operations follow a consistent pattern in `Effects.Database.Tables.*`:

```haskell
-- From Effects.Database.Tables.Blog.hs
data BlogPostInsert = BlogPostInsert
  { bpiTitle :: Text,
    bpiSlug :: Text,
    bpiContent :: Text,
    bpiExcerpt :: Maybe Text,
    bpiAuthorId :: User.Id,
    bpiCategory :: Text,
    bpiStatus :: BlogPostStatus
  }
  deriving stock (Generic, Show, Eq)
  deriving (EncodeRow) via BlogPostInsert

insertBlogPost :: BlogPostInsert -> Hasql.Statement () BlogPostId
insertBlogPost BlogPostInsert {..} =
  case bpiStatus of
    Published -> getOneRow <$> interp False [sql|
      INSERT INTO blog_posts(title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at)
      VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiCategory}, #{bpiStatus}, NOW(), NOW(), NOW())
      RETURNING id
    |]
    _ -> getOneRow <$> interp False [sql|
      INSERT INTO blog_posts(title, slug, content, excerpt, author_id, category, status, published_at, created_at, updated_at)
      VALUES (#{bpiTitle}, #{bpiSlug}, #{bpiContent}, #{bpiExcerpt}, #{bpiAuthorId}, #{bpiCategory}, #{bpiStatus}, NULL, NOW(), NOW())
      RETURNING id
    |]
```

### Authentication & Authorization

Role-based system in `Effects.Database.Tables.UserMetadata`:

```haskell
data UserRole = User | Host | Staff | Admin
  deriving stock (Generic, Show, Eq, Ord, Enum, Bounded)

isHostOrHigher :: UserRole -> Bool
isHostOrHigher Host = True
isHostOrHigher Staff = True  
isHostOrHigher Admin = True
isHostOrHigher _ = False

isStaffOrHigher :: UserRole -> Bool
isStaffOrHigher Staff = True
isStaffOrHigher Admin = True
isStaffOrHigher _ = False
```

### Safe Links Pattern

All internal URLs use Servant's type-safe link generation:

```haskell
-- From API.hs
blogGetLink :: Maybe Int64 -> Maybe Text -> Maybe Text -> Links.Link
blogGetLink = Links.safeLink (Proxy @API) (Proxy @Blog.Get.Route)

-- Usage in handlers
blogGetUrl :: Links.URI
blogGetUrl = Links.linkURI $ blogGetLink Nothing Nothing Nothing

-- In templates with string interpolation
Lucid.href_ [i|/#{blogGetUrl}|]
```

### Component Architecture

Reusable UI components with user-aware rendering:

```haskell
-- From Component.Frame
template :: Maybe UserInfo -> Lucid.Html () -> Lucid.Html ()
template mUser main = 
  Lucid.doctypehtml_ $ do
    -- Navigation varies based on authentication state
    case mUser of
      Nothing -> -- Show login/register links
      Just user -> -- Show user info and dashboard links

loadFrameWithUser :: (Log.MonadLog m, MonadThrow m) => UserInfo -> Lucid.Html () -> m (Lucid.Html ())
loadContentOnly :: (Log.MonadLog m, MonadThrow m) => Lucid.Html () -> m (Lucid.Html ())
```

## Key Definitions & Interfaces

### Core Database Types

**User System**:
```haskell
-- From Effects.Database.Tables.User
newtype Id = Id Int64
data Model = Model
  { mId :: Id,
    mEmail :: EmailAddress, 
    mPassword :: PasswordHash Argon2,
    mCreatedAt :: UTCTime,
    mUpdatedAt :: UTCTime }

-- From Effects.Database.Tables.UserMetadata  
data UserRole = User | Host | Staff | Admin
data Model = Model
  { mId :: Id,
    mUserId :: User.Id,
    mDisplayName :: DisplayName,
    mFullName :: FullName,
    mAvatarUrl :: Maybe Text,
    mUserRole :: UserRole }
```

**Blog System Pattern** (model for Shows/Episodes):
```haskell
-- Database Model
data BlogPostModel = BlogPostModel
  { bpmId :: BlogPostId,
    bpmTitle :: Text,
    bpmSlug :: Text,
    bpmContent :: Text,
    bpmExcerpt :: Maybe Text,
    bpmAuthorId :: User.Id,
    bpmCategory :: Text,
    bpmStatus :: BlogPostStatus,
    bpmPublishedAt :: Maybe UTCTime,
    bpmCreatedAt :: UTCTime,
    bpmUpdatedAt :: UTCTime }

-- API Domain Type  
data BlogPostDomain = BlogPostDomain
  { bpdId :: BlogPostId, ... } -- Same fields as Model
  deriving anyclass (FromJSON, ToJSON)

-- Insert Type
data BlogPostInsert = BlogPostInsert
  { bpiTitle :: Text,
    bpiSlug :: Text,
    bpiContent :: Text,
    bpiExcerpt :: Maybe Text,
    bpiAuthorId :: User.Id,
    bpiCategory :: Text,
    bpiStatus :: BlogPostStatus }
```

### Servant Route Structure

```haskell
type Route =
  Observability.WithSpan
    "POST /blog/new"
    ( "blog"
        :> "new"
        :> Servant.Header "Cookie" Text
        :> Servant.Header "HX-Request" Text
        :> Servant.ReqBody '[Servant.FormUrlEncoded] NewBlogPostForm
        :> Servant.Post '[HTML] (Lucid.Html ()) )
```

### File Upload Support

The system includes `servant-multipart` dependency for handling file uploads:

```haskell
-- Available for file upload routes
:> Servant.MultipartForm Servant.Mem (Servant.MultipartData Servant.Mem)
:> Servant.Post '[HTML] (Lucid.Html ())
```

### Form Handling

```haskell
data NewBlogPostForm = NewBlogPostForm
  { nbpfTitle :: Text,
    nbpfContent :: Text,
    nbpfCategory :: Text,
    nbpfExcerpt :: Maybe Text,
    nbpfStatus :: Maybe Text,
    nbpfTags :: [Text] }

instance FromForm NewBlogPostForm where
  fromForm form = do
    title <- parseUnique "title" form
    content <- parseUnique "content" form
    -- ... parse other fields
```

## Wireframe Requirements Analysis

### Show Page Requirements (`show-page.html`)

**Core Show Data**:
- Show title, description, genre, schedule
- Host information with bio and contact
- Show image/artwork
- Tags for categorization
- Episode archive with search/filter

**Episode Data** (excluding social features):
- Episode title, description, air date, duration  
- Audio player integration
- Track listing with timestamps
- Episode-specific artwork
- Download capability

### Schedule Management (`shows-schedule.html`)

**Schedule Grid**:
- Weekly view with time slots
- Show assignments per time slot
- Current/live show highlighting
- Mobile-responsive day-by-day view

**Show Directory**:
- Filterable by genre, time slot, day
- Show cards with basic info
- Featured show highlighting

### Host Dashboard (`host-dashboard.html`)

**Dashboard Features**:
- Recent episodes management (edit/delete)
- Draft episodes with completion status
- Blog post management
- Show statistics (episodes, downloads, followers)
- Schedule information
- Quick actions (prepare show, new blog post, edit profile)

### Episode Preparation (`prepare-show.html`)

**Episode Creation Workflow**:
- Episode details (title, description, tags)
- Track listing management (add/remove/reorder)
- Audio file upload with validation
- Episode artwork upload
- Status management (draft/published)
- Schedule assignment

**File Upload Requirements**:
- Audio: MP3, WAV, FLAC up to 500MB
- Images: JPG, PNG up to 5MB, recommended 800x800px
- Storage in `/tmp` directory with S3-style paths
- Metadata extraction for duration, format validation

## Show Profiles Implementation Requirements

### Database Schema Extensions

**Shows Table**:
```sql
CREATE TABLE shows (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    description TEXT NOT NULL,
    genre TEXT NOT NULL,
    image_url TEXT,
    tags TEXT[], -- PostgreSQL array
    status TEXT NOT NULL DEFAULT 'active', -- active, inactive, archived
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Host assignments (many-to-many)
CREATE TABLE show_hosts (
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    role TEXT NOT NULL DEFAULT 'host', -- host, co-host
    PRIMARY KEY (show_id, user_id)
);

-- Schedule assignments
CREATE TABLE show_schedule (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    day_of_week INTEGER NOT NULL CHECK (day_of_week BETWEEN 0 AND 6), -- 0=Sunday
    start_time TIME NOT NULL,
    end_time TIME NOT NULL,
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);
```

**Episodes Table**:
```sql
CREATE TABLE episodes (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    title TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    description TEXT NOT NULL,
    audio_file_url TEXT NOT NULL,
    image_url TEXT,
    duration_seconds INTEGER,
    file_size_bytes BIGINT,
    status TEXT NOT NULL DEFAULT 'draft', -- draft, published, archived
    episode_number INTEGER,
    aired_at TIMESTAMPTZ,
    tags TEXT[],
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Track listings
CREATE TABLE episode_tracks (
    id BIGSERIAL PRIMARY KEY,
    episode_id BIGINT NOT NULL REFERENCES episodes(id) ON DELETE CASCADE,
    track_number INTEGER NOT NULL,
    title TEXT NOT NULL,
    artist TEXT NOT NULL,
    album TEXT,
    year INTEGER,
    label TEXT,
    timestamp_seconds INTEGER NOT NULL,
    duration_seconds INTEGER,
    is_exclusive_premiere BOOLEAN NOT NULL DEFAULT FALSE
);
```

### API Route Structure

**Show Management Routes**:
```haskell
-- GET /shows - Shows listing and schedule
-- GET /shows/:slug - Individual show page
-- GET /shows/new - Create show form (Staff+)
-- POST /shows/new - Create show (Staff+)
-- GET /shows/:slug/edit - Edit show form (Host+ for own shows)
-- POST /shows/:slug/edit - Update show (Host+ for own shows)

-- GET /shows/:slug/episodes - Episode listing for show
-- GET /shows/:slug/episodes/new - Create episode form
-- POST /shows/:slug/episodes/new - Create episode with file upload
-- GET /episodes/:slug - Individual episode page
-- GET /episodes/:slug/edit - Edit episode form
-- POST /episodes/:slug/edit - Update episode
```

**File Upload Routes**:
```haskell
-- POST /upload/audio - Audio file upload endpoint
-- POST /upload/image - Image upload endpoint  
-- GET /files/:path - Serve uploaded files
```

### Domain Types Required

```haskell
-- Show Management
data ShowStatus = Active | Inactive | Archived
data HostRole = Host | CoHost

data ShowModel = ShowModel
  { smId :: ShowId,
    smTitle :: Text,
    smSlug :: Text,
    smDescription :: Text,
    smGenre :: Text,
    smImageUrl :: Maybe Text,
    smTags :: [Text],
    smStatus :: ShowStatus,
    smCreatedAt :: UTCTime,
    smUpdatedAt :: UTCTime }

-- Episode Management  
data EpisodeStatus = Draft | Published | Archived

data EpisodeModel = EpisodeModel
  { emId :: EpisodeId,
    emShowId :: ShowId,
    emTitle :: Text,
    emSlug :: Text,
    emDescription :: Text,
    emAudioFileUrl :: Text,
    emImageUrl :: Maybe Text,
    emDurationSeconds :: Maybe Int,
    emFileSizeBytes :: Maybe Int64,
    emStatus :: EpisodeStatus,
    emEpisodeNumber :: Maybe Int,
    emAiredAt :: Maybe UTCTime,
    emTags :: [Text],
    emCreatedAt :: UTCTime,
    emUpdatedAt :: UTCTime }

-- Track Listings
data TrackModel = TrackModel
  { tmId :: TrackId,
    tmEpisodeId :: EpisodeId,
    tmTrackNumber :: Int,
    tmTitle :: Text,
    tmArtist :: Text,
    tmAlbum :: Maybe Text,
    tmYear :: Maybe Int,
    tmLabel :: Maybe Text,
    tmTimestampSeconds :: Int,
    tmDurationSeconds :: Maybe Int,
    tmIsExclusivePremiere :: Bool }
```

### File Upload Implementation

**Storage Structure**:
```
/tmp/uploads/
├── audio/
│   ├── 2025/09/27/
│   │   ├── abc123-episode-title.mp3
│   │   └── def456-another-episode.wav
├── images/
│   ├── shows/
│   │   ├── 2025/09/27/
│   │   │   └── ghi789-show-artwork.jpg
│   ├── episodes/
│   │   ├── 2025/09/27/
│   │   │   └── jkl012-episode-cover.png
```

**File Upload Handler Pattern**:
```haskell
uploadAudioHandler ::
  ( Has Tracer env, Log.MonadLog m, MonadReader env m,
    MonadUnliftIO m, MonadCatch m, MonadIO m ) =>
  Tracer ->
  Maybe Text -> -- Cookie
  MultipartData Mem ->
  m (Lucid.Html ())
uploadAudioHandler _tracer cookie multipartData = do
  Auth.userLoginState cookie >>= \case
    Auth.IsNotLoggedIn -> 
      loadContentOnly loginRequiredTemplate
    Auth.IsLoggedIn user -> do
      -- Validate file type and size
      -- Generate unique filename with date path
      -- Save to /tmp/uploads/audio/YYYY/MM/DD/
      -- Extract metadata (duration, format)
      -- Return success with file URL
```

### Authorization Rules

**Show Management**:
- **View Shows/Episodes**: All users (public)
- **Create Shows**: Staff+ only  
- **Edit Shows**: Host+ for assigned shows, Staff+ for all shows
- **Delete Shows**: Staff+ only
- **Manage Schedule**: Staff+ only

**Episode Management**:
- **Create Episodes**: Host+ for assigned shows
- **Edit Episodes**: Host+ for own show episodes, Staff+ for all
- **Publish Episodes**: Host+ for own shows, Staff+ for all
- **Delete Episodes**: Host+ for own shows, Staff+ for all

**File Uploads**:
- **Upload Audio**: Host+ only
- **Upload Images**: Host+ only
- **Delete Files**: Host+ for own uploads, Staff+ for all

This architecture provides a comprehensive foundation for implementing show profiles with proper authentication, file upload handling, and the existing CPS error handling patterns while maintaining type safety throughout the system.

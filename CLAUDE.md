# CLAUDE.md

## CRITICAL RULES

**NEVER read `.envrc.local`, `.env`, or any file containing secrets.** These contain credentials that should not be sent to external services.

**NEVER run `cabal clean` without explicit user approval.** Forces full rebuild which takes a very long time.

**If you encounter a contradiction or impossible situation, STOP and ask the user for guidance.** Do not make assumptions.

## Project Overview

KPBJ 95.9FM community radio station. Three services:

- **Web Service** — Haskell/Servant app (Fly.io). Website, dashboard, playout API.
- **Liquidsoap** — Audio automation (VPS). Polls web API, streams to Icecast.
- **Icecast** — Streaming server (VPS). Serves listeners.

See [ARCHITECTURE.md](ARCHITECTURE.md) for system topology and CI/CD. See [README.md](README.md) for setup and deployment.

**Web Service Tech**: Haskell + Servant | Lucid2 + HTMX | PostgreSQL + Hasql | Tailwind CSS | Nix

## Architecture

### HTML-Over-The-Wire
- HTMX swaps HTML fragments (no JSON API + SPA)
- `renderTemplate hxRequest mUserInfo content` returns full page or fragment based on `HX-Request` header
- Persistent music player survives navigation

### HTMX Response Patterns

**Pattern A - Redirect with Banner** (POST success, navigate away):
```haskell
let banner = BannerParams Success "Created" "Item created successfully."
pure $ Servant.addHeader [i|/#{targetUrl}|] $ redirectWithBanner [i|/#{targetUrl}|] banner
```

**Pattern B - Row Update with OOB Banner** (update item in list):
```haskell
pure $ do
  renderItemRow updatedItem
  renderBanner Success "Updated" "Item updated."
```

**Pattern C - Row Delete with OOB Banner** (remove from list):
```haskell
pure $ do
  mempty  -- removes target element
  renderBanner Success "Deleted" "Item deleted."
```

**Errors**: Always return OOB banner only: `pure $ renderBanner Error "Failed" "Error message."`

### Type-Safe Links (MANDATORY)

**NEVER hardcode URLs.** All internal links use Servant compile-time verification.

1. Define in `API.hs`: `blogGetLink :: Maybe Int64 -> Links.Link`
2. Import with `{-# SOURCE #-}`: `import {-# SOURCE #-} API (blogGetLink)`
3. Create top-level URI: `blogGetUrl = Links.linkURI $ blogGetLink Nothing`
4. Use in templates: `Lucid.href_ [i|/#{blogGetUrl}|]`

### Concrete AppM Monad

All handlers use `AppM` directly (no MTL constraints):
```haskell
handler :: Tracer -> Maybe Cookie -> Maybe HxRequest -> AppM (Lucid.Html ())
```

Key operations: `execQuery`, `Log.logInfo`, `asks`, `liftIO`

## Code Conventions

### Import Qualifiers (MANDATORY)

**Lucid - MUST qualify**: `import Lucid qualified` → `Lucid.div_`, `Lucid.class_`, etc.

**Lucid.Extras - Unqualified**: `import Lucid.Extras` → `hxGet_`, `hxPost_`, `xData_`, `xModel_`

**External libraries - Qualify**: `import Servant.Links qualified as Links`

**Specific functions - Direct**: `import Data.String.Interpolate (i)`

### String Interpolation

Use `[i|...|]` quasi-quoter for all interpolation:
```haskell
Lucid.href_ [i|/#{blogPostUrl slug}|]
hxGet_ [i|/api/shows/#{showId}/episodes|]
```

### Haddock Comments

```haskell
-- | Main description of function.
functionName ::
  User.Id ->    -- ^ Parameter description
  Text ->       -- ^ Another parameter
  AppM Result
```

## Handler Pattern

File: `API/<Feature>/<Action>/<Method>.hs`

```haskell
type Route = Observability.WithSpan "GET /feature" ("feature" :> Servant.Header "Cookie" Cookie :> Servant.Header "HX-Request" HxRequest :> Servant.Get '[HTML] (Lucid.Html ()))

featureGetUrl :: Links.URI
featureGetUrl = Links.linkURI featureGetLink

handler :: Tracer -> Maybe Cookie -> Maybe HxRequest -> AppM (Lucid.Html ())
handler _tracer cookie (foldHxReq -> hxRequest) = do
  mUserInfo <- getUserInfo cookie <&> fmap snd
  renderTemplate hxRequest mUserInfo pageTemplate
```

## Database

### Tables
- **Users**: `users`, `server_sessions`, `user_metadata`, `host_details`
- **Shows**: `shows`, `show_hosts`, `show_schedules`, `episodes`, `episode_tracks`
- **Blog**: `blog_posts`, `blog_tags`, `show_blog_posts`, `show_blog_tags`
- **Events**: `events`, `event_tags`

### Access Pattern

Module: `Effects.Database.Tables.<TableName>` with `Id`, `Model`, `Insert` types and queries using `hasql-interpolate`:
```haskell
getById :: Id -> Hasql.Statement () (Maybe Model)
getById id = interp False [sql| SELECT ... FROM table WHERE id = #{id} |]
```

    Execute: `result <- execQuery (Table.getById id)`

## Role System

**Hierarchy**: User → Host → Staff → Admin (progressive permissions)

- **User**: View content, newsletter, account management
- **Host**: Upload episodes for assigned shows, create show blogs, submit events
- **Staff**: Station-wide blog posts, moderate content, manage multiple shows
- **Admin**: User management, system configuration

Enforcement: `user_metadata.role` enum, `requireRole` middleware, role in session cookie

## File Storage

**Backends**: Local filesystem (dev) or S3-compatible (prod via DigitalOcean Spaces)

**Staged Uploads**: Two-phase commit pattern
1. Upload to staging with UUID token → `/api/uploads/audio`
2. Claim with `claimAndRelocateUpload` → moves to final location

**Structure**: `media/{episodes,shows,events}/{slug}/{YYYY}/{MM}/{DD}/{type}/`

## Development Commands

```bash
# Build & Run
just run              # Run server
just build            # Build all
just test             # Run tests

# Formatting (ormolu 0.7.2.0 required)
just format-changed   # Format changed files
just hlint-changed    # Lint changed files

# Database
just dev-postgres-start/stop/psql
just dev-migrations-add NAME / dev-migrations-run / dev-migrations-reset
just dev-mock-data
```

See [README.md](README.md) for full setup, deployment, and release process.

## Adding New Features

### New Route
1. Create `src/API/Feature/Action/Get.hs` with `Route` type and `handler`
2. Add to `API.hs`: route type, server handler, link function
3. Add to cabal exposed-modules

### New Table
1. `just dev-migrations-add create_table_name`
2. Write SQL migration
3. Create `src/Effects/Database/Tables/TableName.hs`
4. Add to cabal exposed-modules

## Environment

- **Development**: Port 4000, PostgreSQL on port 5433, local file storage (`/tmp/kpbj`)
- **Production**: DigitalOcean VPS, local PostgreSQL, DigitalOcean Spaces

```haskell
data Environment = Development | Production
```

In Development, the app always uses local storage regardless of S3 env vars.

## Design Context

### Users

KPBJ serves three overlapping audiences:
- **Local community members** in the Sun Valley/Burbank area seeking connection to local radio, events, and neighborhood culture
- **Music enthusiasts** discovering independent and eclectic programming
- **Radio hosts and volunteers** managing shows, uploading episodes, and running station operations

Users typically arrive wanting to listen, discover shows, check the schedule, or (for hosts) manage content. The interface should serve all these needs without friction.

### Brand Personality

**Voice**: Direct, unpretentious, community-first
**Tone**: Authentic, welcoming, slightly irreverent
**Three words**: Honest, Local, Independent

The station embodies DIY community radio values—substance over polish, authenticity over slickness.

### Aesthetic Direction

**Visual tone**: Brutalist with purpose
- Monospace typography throughout (unified font stack)
- Bold 2px borders defining clear boundaries
- High contrast, minimal decoration
- Functional color only (status indicators, not decoration)

**References to draw from**:
- Web 1.0 simplicity and directness
- TUI (terminal user interface) aesthetics
- Newspaper layout clarity and information density
- Harsh Patel's design work

**What to avoid**:
- Decorative elements that don't serve function
- Corporate polish or startup aesthetics
- Excessive whitespace or "airy" layouts

**Theme support**: Light/dark modes via CSS custom properties. Multiple terminal-inspired color schemes available (default, solarized, gruvbox, dracula, nord).

### Design Principles

1. **Function dictates form** — Every element earns its place. If it doesn't help users listen, discover, or manage content, remove it.

2. **Honest materials** — Show the structure. Borders are visible, not hidden. Typography is monospace because that's what it is. No pretense.

3. **Information density over decoration** — Pack useful information tightly. Users scanning a schedule or show list should see maximum content with minimum scrolling.

4. **Consistent rhythm** — Use the spacing scale (4px base) and typography scale consistently. Predictable patterns help users navigate faster.

5. **Progressive disclosure** — Show what matters immediately. Details expand on interaction (HTMX fragments, Alpine toggles). Don't overwhelm the first view.

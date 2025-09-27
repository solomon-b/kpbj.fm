-- Create show status and frequency domains
CREATE DOMAIN show_status AS TEXT CHECK (VALUE IN ('active', 'inactive', 'hiatus', 'archived'));
CREATE DOMAIN show_frequency AS TEXT CHECK (VALUE IN ('weekly', 'biweekly', 'monthly', 'occasional', 'one-time'));

-- Create shows table
CREATE TABLE shows (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE, -- URL-friendly version of title
    description TEXT NOT NULL, -- Show description
    genre TEXT, -- Music genre or show category
    logo_url TEXT, -- Path to show logo/artwork
    banner_url TEXT, -- Path to show banner image
    status show_status NOT NULL DEFAULT 'active',
    frequency show_frequency NOT NULL DEFAULT 'weekly',
    duration_minutes BIGINT, -- Typical episode duration
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create show hosts junction table (many-to-many relationship)
CREATE TABLE show_hosts (
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    user_id BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    role TEXT NOT NULL DEFAULT 'host', -- host, co-host, guest
    is_primary BOOLEAN NOT NULL DEFAULT FALSE, -- primary host for the show
    joined_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    left_at TIMESTAMPTZ, -- NULL if still active
    PRIMARY KEY (show_id, user_id)
);

-- Create episodes table
CREATE TABLE episodes (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    title TEXT NOT NULL,
    slug TEXT NOT NULL, -- URL-friendly version, unique per show
    description TEXT, -- Episode description/notes
    episode_number INTEGER, -- Episode number within the show
    season_number INTEGER DEFAULT 1, -- Season number (defaults to 1)
    
    -- Audio file information
    audio_file_path TEXT, -- Path to the main audio file
    audio_file_size BIGINT, -- File size in bytes
    audio_mime_type TEXT, -- MIME type (audio/mpeg, audio/wav, etc.)
    duration_seconds INTEGER, -- Actual duration of the episode
    
    -- Episode artwork
    artwork_url TEXT, -- Path to episode-specific artwork
    
    -- Scheduling and publishing
    scheduled_at TIMESTAMPTZ, -- When the episode is scheduled to air
    published_at TIMESTAMPTZ, -- When the episode was published
    status TEXT NOT NULL DEFAULT 'draft', -- draft, scheduled, published, archived
    
    -- Metadata
    created_by BIGINT NOT NULL REFERENCES users(id), -- Who created this episode
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create episode tracks table for track listings
CREATE TABLE episode_tracks (
    id BIGSERIAL PRIMARY KEY,
    episode_id BIGINT NOT NULL REFERENCES episodes(id) ON DELETE CASCADE,
    track_number INTEGER NOT NULL, -- Order within the episode
    
    -- Track information
    title TEXT NOT NULL,
    artist TEXT NOT NULL,
    album TEXT, -- Album name
    year INTEGER, -- Release year
    duration TEXT, -- Duration as string (e.g., "4:23")
    label TEXT, -- Record label
    
    -- Special flags
    is_exclusive_premiere BOOLEAN NOT NULL DEFAULT FALSE,
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create show schedules table for recurring schedules
CREATE TABLE show_schedules (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    day_of_week BIGINT NOT NULL, -- 0=Sunday, 1=Monday, etc.
    start_time TEXT NOT NULL, -- Time as string (HH:MM)
    end_time TEXT NOT NULL, -- Time as string (HH:MM)
    timezone TEXT NOT NULL DEFAULT 'America/Los_Angeles', -- Show timezone
    is_active BOOLEAN NOT NULL DEFAULT TRUE,
    effective_from DATE NOT NULL DEFAULT CURRENT_DATE,
    effective_until DATE, -- NULL means ongoing
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create indexes for performance
CREATE INDEX idx_shows_slug ON shows(slug);
CREATE INDEX idx_shows_status ON shows(status);

CREATE INDEX idx_show_hosts_show_id ON show_hosts(show_id);
CREATE INDEX idx_show_hosts_user_id ON show_hosts(user_id);
CREATE INDEX idx_show_hosts_active ON show_hosts(show_id, user_id) WHERE left_at IS NULL;

CREATE INDEX idx_episodes_show_id ON episodes(show_id);
CREATE INDEX idx_episodes_slug ON episodes(show_id, slug);
CREATE INDEX idx_episodes_status ON episodes(status);
CREATE INDEX idx_episodes_published_at ON episodes(published_at DESC);
CREATE INDEX idx_episodes_scheduled_at ON episodes(scheduled_at);

CREATE INDEX idx_episode_tracks_episode_id ON episode_tracks(episode_id);
CREATE INDEX idx_episode_tracks_order ON episode_tracks(episode_id, track_number);

CREATE INDEX idx_show_schedules_show_id ON show_schedules(show_id);
CREATE INDEX idx_show_schedules_day_time ON show_schedules(day_of_week, start_time);

-- Add constraints
ALTER TABLE show_hosts ADD CONSTRAINT check_role CHECK (role IN ('host', 'co-host', 'guest'));
ALTER TABLE episodes ADD CONSTRAINT check_status CHECK (status IN ('draft', 'scheduled', 'published', 'archived'));
ALTER TABLE episodes ADD CONSTRAINT unique_episode_slug UNIQUE (show_id, slug);
ALTER TABLE show_schedules ADD CONSTRAINT check_day_of_week CHECK (day_of_week >= 0 AND day_of_week <= 6);
ALTER TABLE show_schedules ADD CONSTRAINT check_times CHECK (start_time < end_time);

-- Add comments for documentation
COMMENT ON TABLE shows IS 'Radio shows - managed by Host+ users';
COMMENT ON TABLE show_hosts IS 'Show host assignments - many-to-many relationship between shows and users';
COMMENT ON TABLE episodes IS 'Individual episodes within shows';
COMMENT ON TABLE episode_tracks IS 'Track listings for episodes';
COMMENT ON TABLE show_schedules IS 'Recurring show schedules';

COMMENT ON COLUMN show_hosts.role IS 'Role: host, co-host, guest';
COMMENT ON COLUMN episodes.status IS 'Status: draft, scheduled, published, archived';
COMMENT ON COLUMN show_schedules.day_of_week IS '0=Sunday, 1=Monday, 2=Tuesday, 3=Wednesday, 4=Thursday, 5=Friday, 6=Saturday';

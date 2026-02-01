-- Create show status domain
CREATE DOMAIN show_status AS TEXT CHECK (VALUE IN ('active', 'inactive', 'hiatus', 'deleted'));

-- Create day of week enum
CREATE TYPE day_of_week AS ENUM (
    'sunday',
    'monday',
    'tuesday',
    'wednesday',
    'thursday',
    'friday',
    'saturday'
);

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
    episode_number INTEGER NOT NULL DEFAULT 1, -- Episode number within the show

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
    status TEXT NOT NULL DEFAULT 'draft', -- draft, scheduled, published, deleted

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

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create schedule templates table for recurring schedules
CREATE TABLE schedule_templates (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    day_of_week day_of_week, -- Day of week (NULL for one-time shows)
    weeks_of_month BIGINT[], -- [1,2,3,4,5] for which weeks of month (NULL for one-time shows)
    start_time TIME NOT NULL, -- Time of day (supports midnight crossing)
    end_time TIME NOT NULL, -- Time of day (supports midnight crossing)
    timezone TEXT NOT NULL DEFAULT 'America/Los_Angeles', -- Show timezone
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    -- Ensure weeks_of_month only contains valid values [1,2,3,4,5] and is non-empty
    CHECK (
        weeks_of_month IS NULL OR (
            weeks_of_month <@ ARRAY[1,2,3,4,5]::BIGINT[] AND
            array_length(weeks_of_month, 1) > 0
        )
    ),
    -- Ensure both day_of_week and weeks_of_month are NULL or both are NOT NULL
    CHECK (
        (day_of_week IS NOT NULL AND weeks_of_month IS NOT NULL)
        OR (day_of_week IS NULL AND weeks_of_month IS NULL)
    )
);

-- Create schedule template validity table for time-bounded schedules
CREATE TABLE schedule_template_validity (
    id BIGSERIAL PRIMARY KEY,
    template_id BIGINT NOT NULL REFERENCES schedule_templates(id) ON DELETE CASCADE,
    effective_from DATE NOT NULL, -- Inclusive start date
    effective_until DATE, -- Exclusive end date (NULL = currently active)
    UNIQUE (template_id, effective_from)
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

CREATE INDEX idx_schedule_templates_show_id ON schedule_templates(show_id);
CREATE INDEX idx_schedule_templates_day_time ON schedule_templates(day_of_week, start_time);
CREATE INDEX idx_schedule_templates_weeks ON schedule_templates(show_id, weeks_of_month) WHERE weeks_of_month IS NOT NULL;

CREATE INDEX idx_schedule_template_validity_template_id ON schedule_template_validity(template_id);
CREATE INDEX idx_schedule_template_validity_dates ON schedule_template_validity(effective_from, effective_until);
CREATE INDEX idx_schedule_template_validity_active ON schedule_template_validity(template_id) WHERE effective_until IS NULL;

-- Add constraints
ALTER TABLE show_hosts ADD CONSTRAINT check_role CHECK (role IN ('host', 'co-host', 'guest'));
ALTER TABLE episodes ADD CONSTRAINT check_status CHECK (status IN ('draft', 'scheduled', 'published', 'deleted'));
ALTER TABLE episodes ADD CONSTRAINT unique_episode_slug UNIQUE (show_id, slug);
ALTER TABLE episodes ADD CONSTRAINT unique_episode_scheduled_at UNIQUE (show_id, scheduled_at);

-- Create function to auto-increment episode numbers per show
CREATE OR REPLACE FUNCTION set_episode_number()
RETURNS TRIGGER AS $$
BEGIN
    -- If episode_number is not explicitly set or is default value
    IF NEW.episode_number = 1 OR NEW.episode_number IS NULL THEN
        -- Get the next episode number for this show
        SELECT COALESCE(MAX(episode_number), 0) + 1
        INTO NEW.episode_number
        FROM episodes
        WHERE show_id = NEW.show_id;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- Create trigger to auto-set episode numbers
CREATE TRIGGER episode_number_trigger
    BEFORE INSERT ON episodes
    FOR EACH ROW
    EXECUTE FUNCTION set_episode_number();

-- Add comments for documentation
COMMENT ON TABLE shows IS 'Radio shows - managed by Host+ users';
COMMENT ON TABLE show_hosts IS 'Show host assignments - many-to-many relationship between shows and users';
COMMENT ON TABLE episodes IS 'Individual episodes within shows';
COMMENT ON TABLE episode_tracks IS 'Track listings for episodes';
COMMENT ON TABLE schedule_templates IS 'Immutable schedule patterns - either recurring (N-of-month) or one-time shows';
COMMENT ON TABLE schedule_template_validity IS 'Mutable time bounds for when schedule templates are active';

COMMENT ON COLUMN show_hosts.role IS 'Role: host, co-host, guest';
COMMENT ON COLUMN episodes.status IS 'Status: draft, scheduled, published, deleted';
COMMENT ON COLUMN episodes.episode_number IS 'Auto-incremented per show via trigger';
COMMENT ON COLUMN schedule_templates.day_of_week IS 'Day of week (sunday, monday, tuesday, wednesday, thursday, friday, saturday); NULL for one-time shows';
COMMENT ON COLUMN schedule_templates.weeks_of_month IS 'Array of week numbers [1,2,3,4,5] indicating which weeks of the month; NULL for one-time shows; Use [1,2,3,4,5] for every week';
COMMENT ON COLUMN schedule_template_validity.effective_from IS 'Inclusive start date';
COMMENT ON COLUMN schedule_template_validity.effective_until IS 'Exclusive end date; NULL means currently active';

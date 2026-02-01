CREATE TABLE events (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE,
    description TEXT NOT NULL,

    -- Date and time
    starts_at TIMESTAMPTZ NOT NULL,
    ends_at TIMESTAMPTZ NOT NULL,

    -- Location
    location_name TEXT NOT NULL,
    location_address TEXT NOT NULL,

    -- Basic publishing
    status TEXT NOT NULL DEFAULT 'draft', -- draft, published
    author_id BIGINT NOT NULL REFERENCES users(id),
    poster_image_url TEXT,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),

    CHECK (ends_at > starts_at)
);

CREATE TABLE event_tags (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE TABLE event_tag_assignments (
    event_id BIGINT NOT NULL REFERENCES events(id) ON DELETE CASCADE,
    tag_id BIGINT NOT NULL REFERENCES event_tags(id) ON DELETE CASCADE,
    PRIMARY KEY (event_id, tag_id)
);

CREATE INDEX idx_events_status ON events(status);
CREATE INDEX idx_events_starts_at ON events(starts_at);
CREATE INDEX idx_events_slug ON events(slug);
CREATE INDEX idx_events_author ON events(author_id);

COMMENT ON TABLE events IS 'Community events table';
COMMENT ON TABLE event_tags IS 'Tags for categorizing events';
COMMENT ON TABLE event_tag_assignments IS 'Many-to-many relationship between events and tags';
COMMENT ON COLUMN events.poster_image_url IS 'Relative path to the event poster image, stored under /static/media/';

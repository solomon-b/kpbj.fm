-- Break items are the short audio clips that fill a break window.
--
-- A break window is the last two minutes of a scheduled show slot, and the last
-- two minutes of an hour that no slot spans. Hosts deliver show audio two
-- minutes short of the slot, so show audio ends where the break begins.
--
-- One table holds both categories. The dashboard splits them into two sections
-- so each can carry its own permission gate. The playout rotation draws from
-- both at once.

-- A domain over TEXT rather than an enum, matching show_status.
-- Rel8 writes this column and casts the value to text, which an enum column
-- rejects and a text domain accepts.
CREATE DOMAIN break_item_category AS TEXT CHECK (VALUE IN ('psa', 'advertisement'));

CREATE TABLE break_items (
    id               BIGSERIAL PRIMARY KEY,
    title            TEXT NOT NULL,
    category         break_item_category NOT NULL,
    audio_file_path  TEXT NOT NULL,
    mime_type        TEXT NOT NULL,
    file_size        BIGINT NOT NULL,
    duration_seconds BIGINT NOT NULL CHECK (duration_seconds > 0),
    starts_on        DATE NOT NULL,
    ends_on          DATE,
    priority         BIGINT NOT NULL DEFAULT 0,
    last_played_at   TIMESTAMPTZ,
    creator_id       BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at       TIMESTAMPTZ,
    CONSTRAINT break_items_date_order CHECK (ends_on IS NULL OR ends_on >= starts_on)
);

-- The playout query filters on the on-air date range and takes live rows only.
CREATE INDEX idx_break_items_eligible ON break_items (starts_on, ends_on)
    WHERE deleted_at IS NULL;

-- The two dashboard lists page through one category at a time, newest first.
CREATE INDEX idx_break_items_category_created_at ON break_items (category, created_at DESC)
    WHERE deleted_at IS NULL;

CREATE INDEX idx_break_items_creator ON break_items (creator_id);

COMMENT ON TABLE break_items IS 'PSAs and advertisement spots that fill break windows';
COMMENT ON COLUMN break_items.title IS 'Display name for the break item';
COMMENT ON COLUMN break_items.category IS 'Which dashboard section owns this row';
COMMENT ON COLUMN break_items.audio_file_path IS 'Path to the audio file in storage (local or S3)';
COMMENT ON COLUMN break_items.mime_type IS 'MIME type of the audio file';
COMMENT ON COLUMN break_items.file_size IS 'Size of the audio file in bytes';
COMMENT ON COLUMN break_items.duration_seconds IS 'Audio length, measured by ffprobe when the file was staged. The break budget needs it to decide what fits';
COMMENT ON COLUMN break_items.starts_on IS 'First Pacific date this item may air on, inclusive';
COMMENT ON COLUMN break_items.ends_on IS 'Last Pacific date this item may air on, inclusive. NULL runs open ended';
COMMENT ON COLUMN break_items.priority IS 'Higher values are selected first. Ties break on last_played_at';
COMMENT ON COLUMN break_items.last_played_at IS 'When the playout API last handed this item out, which is not the same instant it aired. Rotation stays correct when the /played callback lags or never arrives';
COMMENT ON COLUMN break_items.creator_id IS 'User who uploaded this item';
COMMENT ON COLUMN break_items.deleted_at IS 'Soft delete. A deleted row leaves the rotation but keeps its playback history';

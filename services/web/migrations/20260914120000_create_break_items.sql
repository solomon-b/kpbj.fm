-- Break items are the short audio clips that fill a break.
--
-- A break is the two minutes before a half-hour boundary. Hosts deliver show
-- audio two minutes short of the slot, so show audio ends where the break
-- begins.
--
-- One table holds both categories: PSAs, and underwriting announcements. KPBJ
-- holds a noncommercial license, so it airs underwriting, not advertising. The
-- dashboard splits the two into sections so each can carry its own permission
-- gate.

-- An underwriter is a business that pays for underwriting announcements.
CREATE TABLE underwriters (
    id         BIGSERIAL PRIMARY KEY,
    name       TEXT NOT NULL CHECK (btrim(name) <> ''),
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- A domain over TEXT rather than an enum, matching show_status.
-- Rel8 writes this column and casts the value to text, which an enum column
-- rejects and a text domain accepts.
CREATE DOMAIN break_item_category AS TEXT CHECK (VALUE IN ('psa', 'underwriting'));

CREATE TABLE break_items (
    id               BIGSERIAL PRIMARY KEY,
    title            TEXT NOT NULL,
    category         break_item_category NOT NULL,
    underwriter_id   BIGINT REFERENCES underwriters(id),
    spots_per_month  BIGINT CHECK (spots_per_month > 0),
    audio_file_path  TEXT NOT NULL,
    mime_type        TEXT NOT NULL,
    file_size        BIGINT NOT NULL,
    duration_seconds BIGINT NOT NULL CHECK (duration_seconds > 0),
    starts_on        DATE NOT NULL,
    ends_on          DATE,
    creator_id       BIGINT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    created_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at       TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    deleted_at       TIMESTAMPTZ,
    CONSTRAINT break_items_date_order CHECK (ends_on IS NULL OR ends_on >= starts_on),
    -- An underwriting row has an underwriter and a monthly count. A PSA has neither.
    CONSTRAINT break_items_category_fields CHECK (
        (category = 'psa' AND underwriter_id IS NULL AND spots_per_month IS NULL)
        OR (category = 'underwriting' AND underwriter_id IS NOT NULL AND spots_per_month IS NOT NULL)
    )
);

-- The planner reads live rows whose date range covers a day.
CREATE INDEX idx_break_items_eligible ON break_items (starts_on, ends_on)
    WHERE deleted_at IS NULL;

-- The two dashboard lists page through one category at a time, newest first.
CREATE INDEX idx_break_items_category_created_at ON break_items (category, created_at DESC)
    WHERE deleted_at IS NULL;

CREATE INDEX idx_break_items_creator ON break_items (creator_id);

CREATE INDEX idx_break_items_underwriter ON break_items (underwriter_id)
    WHERE deleted_at IS NULL;

COMMENT ON TABLE underwriters IS 'Businesses that pay for underwriting announcements';
COMMENT ON TABLE break_items IS 'PSAs and underwriting announcements that fill breaks';
COMMENT ON COLUMN break_items.title IS 'Display name for the break item';
COMMENT ON COLUMN break_items.category IS 'Which dashboard section owns this row';
COMMENT ON COLUMN break_items.underwriter_id IS 'The underwriter who pays for this announcement. NULL for a PSA';
COMMENT ON COLUMN break_items.spots_per_month IS 'Airings sold per Pacific calendar month. A partial month owes its share, rounded up. NULL for a PSA';
COMMENT ON COLUMN break_items.audio_file_path IS 'Path to the audio file in storage (local or S3)';
COMMENT ON COLUMN break_items.mime_type IS 'MIME type of the audio file';
COMMENT ON COLUMN break_items.file_size IS 'Size of the audio file in bytes';
COMMENT ON COLUMN break_items.duration_seconds IS 'Audio length, measured by ffprobe when the file was staged. The planner needs it to decide what fits';
COMMENT ON COLUMN break_items.starts_on IS 'First Pacific date this item may air on, inclusive';
COMMENT ON COLUMN break_items.ends_on IS 'Last Pacific date this item may air on, inclusive. NULL runs open ended';
COMMENT ON COLUMN break_items.creator_id IS 'User who uploaded this item';
COMMENT ON COLUMN break_items.deleted_at IS 'Soft delete. A deleted row stops airing at once but keeps its playback history';

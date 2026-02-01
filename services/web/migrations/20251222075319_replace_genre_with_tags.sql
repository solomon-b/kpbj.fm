-- Replace genre column with tags system
-- Following the show_blog_tags pattern for many-to-many tag relationships

-- Create show_tags table (reusable tags for categorizing shows)
CREATE TABLE show_tags (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create junction table for show-tag relationships
CREATE TABLE show_tag_assignments (
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    tag_id BIGINT NOT NULL REFERENCES show_tags(id) ON DELETE CASCADE,
    PRIMARY KEY (show_id, tag_id)
);

-- Create indexes for performance
CREATE INDEX idx_show_tag_assignments_show_id ON show_tag_assignments(show_id);
CREATE INDEX idx_show_tag_assignments_tag_id ON show_tag_assignments(tag_id);

-- Add comments for documentation
COMMENT ON TABLE show_tags IS 'Tags for categorizing radio shows (genres, styles, etc.)';
COMMENT ON TABLE show_tag_assignments IS 'Many-to-many relationship between shows and tags';

-- Drop the old genre column (no data migration per user request)
ALTER TABLE shows DROP COLUMN IF EXISTS genre;

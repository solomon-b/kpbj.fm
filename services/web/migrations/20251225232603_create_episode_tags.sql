-- Create episode tags system
-- Following the show_tags pattern for many-to-many tag relationships

-- Create episode_tags table (reusable tags for categorizing episodes)
CREATE TABLE episode_tags (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create junction table for episode-tag relationships
CREATE TABLE episode_tag_assignments (
    episode_id BIGINT NOT NULL REFERENCES episodes(id) ON DELETE CASCADE,
    tag_id BIGINT NOT NULL REFERENCES episode_tags(id) ON DELETE CASCADE,
    PRIMARY KEY (episode_id, tag_id)
);

-- Create indexes for performance
CREATE INDEX idx_episode_tag_assignments_episode_id ON episode_tag_assignments(episode_id);
CREATE INDEX idx_episode_tag_assignments_tag_id ON episode_tag_assignments(tag_id);

-- Add comments for documentation
COMMENT ON TABLE episode_tags IS 'Tags for categorizing episodes (genres, themes, etc.)';
COMMENT ON TABLE episode_tag_assignments IS 'Many-to-many relationship between episodes and tags';

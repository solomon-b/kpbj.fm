-- Create show blog posts table (separate from main station blog)
CREATE TABLE show_blog_posts (
    id BIGSERIAL PRIMARY KEY,
    show_id BIGINT NOT NULL REFERENCES shows(id) ON DELETE CASCADE,
    title TEXT NOT NULL,
    slug TEXT NOT NULL, -- URL-friendly version, unique per show
    content TEXT NOT NULL, -- Full article content
    excerpt TEXT, -- Short preview (optional, can be generated)
    author_id BIGINT NOT NULL REFERENCES users(id), -- Must be a host of the show
    
    -- Publishing
    status TEXT NOT NULL DEFAULT 'draft', -- draft, published, archived
    published_at TIMESTAMPTZ,
    
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create show blog tags table (separate from main blog tags)
CREATE TABLE show_blog_tags (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE, -- track-review, mix-spotlight, guest-interview, etc.
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create junction table for show blog post-tag relationships
CREATE TABLE show_blog_post_tags (
    post_id BIGINT NOT NULL REFERENCES show_blog_posts(id) ON DELETE CASCADE,
    tag_id BIGINT NOT NULL REFERENCES show_blog_tags(id) ON DELETE CASCADE,
    PRIMARY KEY (post_id, tag_id)
);

-- Create indexes for performance
CREATE INDEX idx_show_blog_posts_show_id ON show_blog_posts(show_id);
CREATE INDEX idx_show_blog_posts_author ON show_blog_posts(author_id);
CREATE INDEX idx_show_blog_posts_status ON show_blog_posts(status);
CREATE INDEX idx_show_blog_posts_published_at ON show_blog_posts(published_at DESC);
CREATE INDEX idx_show_blog_posts_slug ON show_blog_posts(show_id, slug);

-- Add constraints
ALTER TABLE show_blog_posts ADD CONSTRAINT check_status CHECK (status IN ('draft', 'published', 'archived'));
ALTER TABLE show_blog_posts ADD CONSTRAINT unique_show_blog_slug UNIQUE (show_id, slug);

-- Add comments for documentation
COMMENT ON TABLE show_blog_posts IS 'Show-specific blog posts - only hosts of the show can create posts';
COMMENT ON TABLE show_blog_tags IS 'Tags specific to show blog posts';
COMMENT ON TABLE show_blog_post_tags IS 'Many-to-many relationship between show blog posts and tags';

COMMENT ON COLUMN show_blog_posts.author_id IS 'Must be a host of the associated show';
COMMENT ON COLUMN show_blog_posts.status IS 'Status: draft, published, archived';

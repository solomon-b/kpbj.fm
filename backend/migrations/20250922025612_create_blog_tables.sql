-- Create blog posts table
CREATE TABLE blog_posts (
    id BIGSERIAL PRIMARY KEY,
    title TEXT NOT NULL,
    slug TEXT NOT NULL UNIQUE, -- URL-friendly version of title
    content TEXT NOT NULL, -- Full article content
    excerpt TEXT, -- Short preview (optional, can be generated)
    author_id BIGINT NOT NULL REFERENCES users(id),
    category TEXT NOT NULL, -- Station News, Community, Events, Music, etc.
    status TEXT NOT NULL DEFAULT 'draft', -- draft, published, archived
    published_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create blog tags table
CREATE TABLE blog_tags (
    id BIGSERIAL PRIMARY KEY,
    name TEXT NOT NULL UNIQUE, -- award, community, underground, etc.
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create junction table for post-tag relationships
CREATE TABLE blog_post_tags (
    post_id BIGINT NOT NULL REFERENCES blog_posts(id) ON DELETE CASCADE,
    tag_id BIGINT NOT NULL REFERENCES blog_tags(id) ON DELETE CASCADE,
    PRIMARY KEY (post_id, tag_id)
);

-- Create indexes for performance
CREATE INDEX idx_blog_posts_author ON blog_posts(author_id);
CREATE INDEX idx_blog_posts_category ON blog_posts(category);
CREATE INDEX idx_blog_posts_status ON blog_posts(status);
CREATE INDEX idx_blog_posts_published_at ON blog_posts(published_at DESC);
CREATE INDEX idx_blog_posts_slug ON blog_posts(slug);

-- Add constraints to ensure only Staff and Admin users can create blog posts
-- This will be enforced at the application level, but we add a comment for documentation
COMMENT ON TABLE blog_posts IS 'Blog posts - only users with Staff or Admin roles can create posts';
COMMENT ON COLUMN blog_posts.author_id IS 'Must be a user with Staff or Admin role';
COMMENT ON COLUMN blog_posts.category IS 'Categories: Station News, Community, Events, Music, Host Spotlights, Local Scene';
COMMENT ON COLUMN blog_posts.status IS 'Status: draft, published, archived';

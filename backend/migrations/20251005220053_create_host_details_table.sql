-- Create host_details table for additional host bio information
CREATE TABLE host_details (
    id BIGSERIAL PRIMARY KEY,
    user_id BIGINT NOT NULL UNIQUE REFERENCES users(id) ON DELETE CASCADE,

    -- Bio information
    bio TEXT, -- Host biography paragraph
    website_url TEXT, -- Personal website

    -- Social links
    instagram_handle TEXT,
    twitter_handle TEXT,
    soundcloud_url TEXT,
    bandcamp_url TEXT,

    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW(),
    updated_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

-- Create index for performance
CREATE INDEX idx_host_details_user_id ON host_details(user_id);

-- Add comments for documentation
COMMENT ON TABLE host_details IS 'Extended host profile information for show hosts';
COMMENT ON COLUMN host_details.bio IS 'Host biography/about text';

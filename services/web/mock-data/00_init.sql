-- Mock Data Initialization
-- Clears existing data and prepares for fresh mock data load
-- Password for all users: "password"

-- Clear existing data in dependency order
TRUNCATE TABLE
    playback_history,
    server_sessions,
    email_verification_tokens,
    password_reset_tokens,
    staged_uploads,
    ephemeral_uploads,
    host_details,
    user_metadata,
    schedule_template_validity,
    schedule_templates,
    show_hosts,
    episode_tracks,
    episode_tag_assignments,
    episode_tags,
    episodes,
    show_tag_assignments,
    show_tags,
    shows,
    events,
    show_blog_post_tags,
    show_blog_posts,
    show_blog_tags,
    blog_post_tags,
    blog_tags,
    blog_posts,
    site_page_revisions,
    site_pages,
    users
RESTART IDENTITY CASCADE;

-- Mock Data Initialization
-- Clears existing data and prepares for fresh mock data load
-- Password for all users: "password"

-- Clear existing data in dependency order
TRUNCATE TABLE
    schedule_template_validity,
    schedule_templates,
    show_hosts,
    episode_tracks,
    episodes,
    show_tag_assignments,
    show_tags,
    shows,
    events,
    blog_post_tags,
    blog_tags,
    blog_posts
RESTART IDENTITY CASCADE;

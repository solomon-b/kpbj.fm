-- Summary Statistics
-- Display counts of all inserted data

SELECT
    'Shows' as metric,
    COUNT(*) as count
FROM shows
UNION ALL
SELECT
    'Host Users' as metric,
    COUNT(*) as count
FROM users WHERE email LIKE 'host-%'
UNION ALL
SELECT
    'Regular Users' as metric,
    COUNT(*) as count
FROM users WHERE email LIKE '%@example.com'
UNION ALL
SELECT
    'Host Details' as metric,
    COUNT(*) as count
FROM host_details
UNION ALL
SELECT
    'Schedule Templates' as metric,
    COUNT(*) as count
FROM schedule_templates
UNION ALL
SELECT
    'Episodes' as metric,
    COUNT(*) as count
FROM episodes
UNION ALL
SELECT
    'Episode Tracks' as metric,
    COUNT(*) as count
FROM episode_tracks
UNION ALL
SELECT
    'Episode Tags' as metric,
    COUNT(*) as count
FROM episode_tags
UNION ALL
SELECT
    'Episode Tag Assignments' as metric,
    COUNT(*) as count
FROM episode_tag_assignments
UNION ALL
SELECT
    'Events' as metric,
    COUNT(*) as count
FROM events
UNION ALL
SELECT
    'Blog Posts' as metric,
    COUNT(*) as count
FROM blog_posts
UNION ALL
SELECT
    'Blog Tags' as metric,
    COUNT(*) as count
FROM blog_tags;

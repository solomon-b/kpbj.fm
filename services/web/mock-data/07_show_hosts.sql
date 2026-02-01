-- Show Hosts
-- Links hosts to their shows with roles

-- Primary host for each show
INSERT INTO show_hosts (show_id, user_id, role, is_primary)
SELECT s.id, u.id, 'host', TRUE
FROM shows s
JOIN users u ON u.email = 'host-' || s.slug || '@kpbj.fm';

-- Add Willow Hart as co-host to indie-mixtape (for testing multi-show selector)
INSERT INTO show_hosts (show_id, user_id, role, is_primary)
SELECT
    (SELECT id FROM shows WHERE slug = 'indie-mixtape'),
    (SELECT id FROM users WHERE email = 'host-acoustic-sessions@kpbj.fm'),
    'co-host',
    FALSE;

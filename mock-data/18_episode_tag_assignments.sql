-- Episode Tag Assignments
-- Assigns tags to episodes based on patterns

-- Assign 'classics' to episodes from jazz, blues, soul, and classical shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'classics'
AND s.slug IN ('sunday-morning-jazz', 'blues-after-dark', 'soul-kitchen', 'classical-sundays', 'coffee-and-classics')
ON CONFLICT DO NOTHING;

-- Assign 'chill' to ambient, lounge, and downtempo shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'chill'
AND s.slug IN ('midnight-ambient', 'late-night-chill', 'jazz-lounge')
ON CONFLICT DO NOTHING;

-- Assign 'high-energy' to punk, metal, and electronic shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'high-energy'
AND s.slug IN ('punk-power-hour', 'metal-madness', 'electronic-sunday')
ON CONFLICT DO NOTHING;

-- Assign 'acoustic' to acoustic and folk shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'acoustic'
AND s.slug IN ('acoustic-sessions', 'folk-tales')
ON CONFLICT DO NOTHING;

-- Assign 'international' to world music, latin, and reggae shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'international'
AND s.slug IN ('world-music-passport', 'latin-grooves', 'reggae-vibes')
ON CONFLICT DO NOTHING;

-- Assign 'experimental' to experimental shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'experimental'
AND s.slug = 'experimental-sounds'
ON CONFLICT DO NOTHING;

-- Assign 'electronic' to electronic shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'electronic'
AND s.slug IN ('electronic-sunday', 'midnight-ambient', 'late-night-chill')
ON CONFLICT DO NOTHING;

-- Assign 'new-releases' to some indie shows
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
JOIN shows s ON e.show_id = s.id
CROSS JOIN episode_tags et
WHERE et.name = 'new-releases'
AND s.slug = 'indie-mixtape'
ON CONFLICT DO NOTHING;

-- Randomly assign 'live-performance' to ~20% of episodes
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
CROSS JOIN episode_tags et
WHERE et.name = 'live-performance'
AND random() < 0.2
ON CONFLICT DO NOTHING;

-- Randomly assign 'guest-dj' to ~15% of episodes
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
CROSS JOIN episode_tags et
WHERE et.name = 'guest-dj'
AND random() < 0.15
ON CONFLICT DO NOTHING;

-- Randomly assign 'local-artists' to ~25% of episodes
INSERT INTO episode_tag_assignments (episode_id, tag_id)
SELECT e.id, et.id
FROM episodes e
CROSS JOIN episode_tags et
WHERE et.name = 'local-artists'
AND random() < 0.25
ON CONFLICT DO NOTHING;

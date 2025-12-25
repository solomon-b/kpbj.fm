-- Schedule Templates
-- Each show airs once per week at a specific time (24/7 coverage)
-- All shows are weekly (weeks_of_month = [1,2,3,4,5]) and use Pacific timezone

INSERT INTO schedule_templates (show_id, day_of_week, weeks_of_month, start_time, end_time, timezone) VALUES
-- SUNDAY
((SELECT id FROM shows WHERE slug = 'sunday-morning-jazz'), 'sunday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'gospel-hour'), 'sunday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'world-music-passport'), 'sunday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'classical-sundays'), 'sunday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'indie-mixtape'), 'sunday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'soul-kitchen'), 'sunday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'acoustic-sessions'), 'sunday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'electronic-sunday'), 'sunday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'reggae-vibes'), 'sunday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'blues-after-dark'), 'sunday', ARRAY[1,2,3,4,5], '18:00', '20:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'experimental-sounds'), 'sunday', ARRAY[1,2,3,4,5], '20:00', '22:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'midnight-ambient'), 'sunday', ARRAY[1,2,3,4,5], '22:00', '24:00', 'America/Los_Angeles'),

-- MONDAY
((SELECT id FROM shows WHERE slug = 'monday-morning-wake-up'), 'monday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'coffee-and-classics'), 'monday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'folk-tales'), 'monday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'midday-mix'), 'monday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'latin-grooves'), 'monday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'rock-solid'), 'monday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'drive-time'), 'monday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'hip-hop-fundamentals'), 'monday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'jazz-lounge'), 'monday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'punk-power-hour'), 'monday', ARRAY[1,2,3,4,5], '18:00', '19:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'metal-madness'), 'monday', ARRAY[1,2,3,4,5], '19:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'late-night-chill'), 'monday', ARRAY[1,2,3,4,5], '21:00', '23:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'graveyard-shift'), 'monday', ARRAY[1,2,3,4,5], '23:00', '24:00', 'America/Los_Angeles'),

-- TUESDAY
((SELECT id FROM shows WHERE slug = 'tuesday-sunrise'), 'tuesday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'morning-brew'), 'tuesday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'singer-songwriter-showcase'), 'tuesday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'world-beat'), 'tuesday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'indie-rock-hour'), 'tuesday', ARRAY[1,2,3,4,5], '08:00', '09:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'soul-stirrers'), 'tuesday', ARRAY[1,2,3,4,5], '09:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'alternative-nation'), 'tuesday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'electronic-evolution'), 'tuesday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'country-roads'), 'tuesday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'funk-sessions'), 'tuesday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'jazz-standards'), 'tuesday', ARRAY[1,2,3,4,5], '18:00', '20:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'noise-and-space'), 'tuesday', ARRAY[1,2,3,4,5], '20:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'deep-night-mix'), 'tuesday', ARRAY[1,2,3,4,5], '21:00', '23:00', 'America/Los_Angeles'),

-- WEDNESDAY
((SELECT id FROM shows WHERE slug = 'midweek-morning'), 'wednesday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'classical-interlude'), 'wednesday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'folk-underground'), 'wednesday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'lunch-hour-favorites'), 'wednesday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'afrobeat-express'), 'wednesday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'psychedelic-journey'), 'wednesday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'hip-hop-heritage'), 'wednesday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'dub-vibrations'), 'wednesday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'shoegaze-and-dream-pop'), 'wednesday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'jazz-fusion'), 'wednesday', ARRAY[1,2,3,4,5], '18:00', '20:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'post-rock-horizons'), 'wednesday', ARRAY[1,2,3,4,5], '20:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'industrial-underground'), 'wednesday', ARRAY[1,2,3,4,5], '21:00', '22:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'night-drones'), 'wednesday', ARRAY[1,2,3,4,5], '22:00', '24:00', 'America/Los_Angeles'),

-- THURSDAY
((SELECT id FROM shows WHERE slug = 'thursday-wake-up-call'), 'thursday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'baroque-and-beyond'), 'thursday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'bluegrass-and-old-time'), 'thursday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'tropical-sounds'), 'thursday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'garage-rock-revival'), 'thursday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'new-wave-nostalgia'), 'thursday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'techno-territory'), 'thursday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'ska-and-rocksteady'), 'thursday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'r-and-b-slow-jams'), 'thursday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'hardcore-punk'), 'thursday', ARRAY[1,2,3,4,5], '18:00', '19:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'doom-and-stoner'), 'thursday', ARRAY[1,2,3,4,5], '19:00', '20:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'witch-house'), 'thursday', ARRAY[1,2,3,4,5], '20:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'late-night-jazz'), 'thursday', ARRAY[1,2,3,4,5], '21:00', '23:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'minimal-techno'), 'thursday', ARRAY[1,2,3,4,5], '23:00', '24:00', 'America/Los_Angeles'),

-- FRIDAY
((SELECT id FROM shows WHERE slug = 'friday-morning-groove'), 'friday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'chamber-music'), 'friday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'desert-rock'), 'friday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'cumbia-and-chicha'), 'friday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'post-punk-power'), 'friday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'house-music-all-night'), 'friday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'weekend-warm-up'), 'friday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'trap-and-bass'), 'friday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'disco-fever'), 'friday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'emo-night'), 'friday', ARRAY[1,2,3,4,5], '18:00', '19:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'black-metal'), 'friday', ARRAY[1,2,3,4,5], '19:00', '20:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'vaporwave-dreams'), 'friday', ARRAY[1,2,3,4,5], '20:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'midnight-madness'), 'friday', ARRAY[1,2,3,4,5], '21:00', '23:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'deep-house'), 'friday', ARRAY[1,2,3,4,5], '23:00', '24:00', 'America/Los_Angeles'),

-- SATURDAY
((SELECT id FROM shows WHERE slug = 'saturday-morning-cartoons'), 'saturday', ARRAY[1,2,3,4,5], '00:00', '02:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'opera-hour'), 'saturday', ARRAY[1,2,3,4,5], '02:00', '04:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'surf-rock'), 'saturday', ARRAY[1,2,3,4,5], '04:00', '06:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'global-bass'), 'saturday', ARRAY[1,2,3,4,5], '06:00', '08:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'britpop-and-beyond'), 'saturday', ARRAY[1,2,3,4,5], '08:00', '10:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'drum-and-bass'), 'saturday', ARRAY[1,2,3,4,5], '10:00', '12:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'saturday-dance-party'), 'saturday', ARRAY[1,2,3,4,5], '12:00', '14:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'lo-fi-hip-hop'), 'saturday', ARRAY[1,2,3,4,5], '14:00', '16:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'motown-and-stax'), 'saturday', ARRAY[1,2,3,4,5], '16:00', '18:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'thrash-metal'), 'saturday', ARRAY[1,2,3,4,5], '18:00', '19:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'synthwave-night'), 'saturday', ARRAY[1,2,3,4,5], '19:00', '21:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'ambient-soundscapes'), 'saturday', ARRAY[1,2,3,4,5], '21:00', '23:00', 'America/Los_Angeles'),
((SELECT id FROM shows WHERE slug = 'late-night-beats'), 'saturday', ARRAY[1,2,3,4,5], '23:00', '24:00', 'America/Los_Angeles');

-- Create validity periods for all schedule templates
-- All templates are effective from 30 days ago and currently active
INSERT INTO schedule_template_validity (template_id, effective_from, effective_until)
SELECT id, CURRENT_DATE - INTERVAL '30 days', NULL
FROM schedule_templates;

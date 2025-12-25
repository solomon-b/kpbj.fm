-- Host Details
-- Bios and social links for hosts (with some randomized presence)

INSERT INTO host_details (user_id, bio, website_url, instagram_handle, twitter_handle, soundcloud_url, bandcamp_url)
SELECT
    u.id,
    CASE s.slug
        WHEN 'sunday-morning-jazz' THEN 'A jazz enthusiast since childhood, I grew up listening to my parents'' vinyl collection. After years of digging through crates and studying the masters, I bring you two hours of smooth jazz every Sunday morning to start your week right.'
        WHEN 'gospel-hour' THEN 'Raised in the church, gospel music has been the soundtrack of my life. I''m honored to share uplifting spirituals and contemporary gospel that feed the soul and lift the spirit.'
        WHEN 'world-music-passport' THEN 'As a first-generation immigrant, I use music to explore my roots and discover new cultures. Join me on a sonic journey across continents, from West African grooves to Eastern European folk.'
        WHEN 'classical-sundays' THEN 'Trained as a classical violinist, I''ve spent 20 years performing and studying the great composers. These Sunday mornings are my chance to share the beauty and complexity of classical music with fellow enthusiasts.'
        WHEN 'indie-mixtape' THEN 'Portland''s indie scene is where I found my musical home. I spend my weeks hunting down the freshest independent releases and underground gems to share with you every Sunday.'
        WHEN 'soul-kitchen' THEN 'Soul, R&B, and funk aren''t just genres—they''re a way of life. I cook up a blend of classics and deep cuts that''ll make you move and groove all afternoon long.'
        WHEN 'acoustic-sessions' THEN 'There''s something magical about stripped-down acoustic performances. As a singer-songwriter myself, I appreciate the raw emotion and craftsmanship that shines through when it''s just voice and instrument.'
        WHEN 'electronic-sunday' THEN 'From Detroit techno to Berlin minimal, electronic music has been my passion for over a decade. I curate sets that take you on a journey through the evolution of electronic sound.'
        WHEN 'reggae-vibes' THEN 'One love, one heart. Reggae taught me about resistance, peace, and unity. Every Sunday I share roots reggae, dancehall, and dub to spread positive vibrations.'
        WHEN 'blues-after-dark' THEN 'The blues is where it all began. Late Sunday nights, I dig deep into the Mississippi Delta, Chicago electric blues, and contemporary blues artists keeping the tradition alive.'
        WHEN 'experimental-sounds' THEN 'Music shouldn''t have boundaries. I''m here to challenge your ears with avant-garde compositions, noise experiments, and sounds that push the limits of what music can be.'
        WHEN 'midnight-ambient' THEN 'For the night owls and insomniacs, I create atmospheric soundscapes that drift between consciousness and dreams. Let the music carry you through the late hours.'
        WHEN 'late-night-jazz' THEN 'There''s something special about jazz after midnight. I''ve been a working jazz musician in Portland for 15 years, and these Thursday nights let me share the sophisticated sounds that define late-night cool.'
        WHEN 'punk-power-hour' THEN 'Fast, loud, and unapologetic. I grew up in the DIY punk scene, and this hour is dedicated to the raw energy and politics of punk rock from 1977 to today.'
        WHEN 'metal-madness' THEN 'Metal isn''t just music—it''s a lifestyle. From classic heavy metal to death and doom, I bring you two hours of crushing riffs and thunderous drums every Monday night.'
        WHEN 'hip-hop-fundamentals' THEN 'Hip-hop is the most important cultural movement of the last 50 years. I break down the history, the culture, and the essential tracks that define this genre.'
        WHEN 'disco-fever' THEN 'Disco never died—it just evolved. Every Friday I celebrate the groove, glamour, and liberation of disco culture with classics and modern takes on the sound.'
        WHEN 'techno-territory' THEN 'Pure, unfiltered techno. No vocals, just hypnotic beats designed for the dance floor. I''ve been DJing techno in Portland clubs for a decade before bringing it to the airwaves.'
        WHEN 'ambient-soundscapes' THEN 'As a sound designer and meditation instructor, I curate ambient music that creates space for reflection, relaxation, and inner exploration.'
        WHEN 'local-legends-portland-music-history-panel-2025-10-11' THEN 'Portland music historian and archivist, documenting our city''s rich musical heritage through interviews, artifacts, and deep research into local scenes past and present.'
        ELSE 'Community radio DJ passionate about sharing diverse music and connecting with Portland listeners. Broadcasting from KPBJ 95.9FM.'
    END as bio,
    CASE
        WHEN random() < 0.3 THEN 'https://' || REPLACE(s.slug, '-', '') || '.com'
        ELSE NULL
    END as website_url,
    CASE
        WHEN random() < 0.5 THEN REPLACE(s.slug, '-', '_')
        ELSE NULL
    END as instagram_handle,
    CASE
        WHEN random() < 0.4 THEN REPLACE(s.slug, '-', '_')
        ELSE NULL
    END as twitter_handle,
    CASE
        WHEN random() < 0.2 THEN 'https://soundcloud.com/' || s.slug
        ELSE NULL
    END as soundcloud_url,
    CASE
        WHEN random() < 0.15 THEN 'https://' || REPLACE(s.slug, '-', '') || '.bandcamp.com'
        ELSE NULL
    END as bandcamp_url
FROM users u
JOIN shows s ON u.email = 'host-' || s.slug || '@kpbj.fm';

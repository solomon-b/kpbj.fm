-- Mock Shows Data for KPBJ 95.9FM
-- 84 unique shows filling 24/7 schedule, each airing once per week
-- Password for all users: "password"

-- First, clear existing data
TRUNCATE TABLE schedule_template_validity, schedule_templates, show_hosts, episode_tracks, episodes, shows, event_tag_assignments, event_tags, events, blog_post_tags, blog_tags, blog_posts RESTART IDENTITY CASCADE;

-- Create admin user (password: "password")
INSERT INTO users (email, password) VALUES
  ('admin@kpbj.fm', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

-- Create admin user_metadata
INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'Admin', 'KPBJ Administrator', 'Admin'
FROM users u WHERE u.email = 'admin@kpbj.fm';

-- Insert 84 diverse radio shows
-- Format: title, slug, description, genre
INSERT INTO shows (title, slug, description, genre, status) VALUES
-- SUNDAY (24 hours = 12 shows of 2hrs each)
('Sunday Morning Jazz', 'sunday-morning-jazz', 'Start your Sunday with smooth jazz classics', 'Jazz', 'active'),
('Gospel Hour', 'gospel-hour', 'Uplifting gospel music and spirituals', 'Gospel', 'active'),
('World Music Passport', 'world-music-passport', 'Global sounds from every continent', 'World', 'active'),
('Classical Sundays', 'classical-sundays', 'Timeless classical compositions', 'Classical', 'active'),
('Indie Mixtape', 'indie-mixtape', 'Fresh independent music discoveries', 'Indie', 'active'),
('Soul Kitchen', 'soul-kitchen', 'Classic soul, R&B, and funk', 'Soul/R&B', 'active'),
('Acoustic Sessions', 'acoustic-sessions', 'Stripped-down acoustic performances', 'Folk', 'active'),
('Electronic Sunday', 'electronic-sunday', 'Electronic beats to end your weekend', 'Electronic', 'active'),
('Reggae Vibes', 'reggae-vibes', 'Roots reggae and dancehall', 'Reggae', 'active'),
('Blues After Dark', 'blues-after-dark', 'Late night blues classics', 'Blues', 'active'),
('Experimental Sounds', 'experimental-sounds', 'Avant-garde and boundary-pushing music', 'Experimental', 'active'),
('Midnight Ambient', 'midnight-ambient', 'Atmospheric soundscapes for late night', 'Ambient', 'active'),

-- MONDAY (24 hours = mix of 1hr and 2hr shows)
('Monday Morning Wake Up', 'monday-morning-wake-up', 'Energetic music to start your week', 'Pop', 'active'),
('Coffee & Classics', 'coffee-and-classics', 'Classical music for your morning routine', 'Classical', 'active'),
('Folk Tales', 'folk-tales', 'Folk music and storytelling', 'Folk', 'active'),
('Midday Mix', 'midday-mix', 'Eclectic midday music mix', 'Variety', 'active'),
('Latin Grooves', 'latin-grooves', 'Salsa, cumbia, and Latin rhythms', 'Latin', 'active'),
('Rock Solid', 'rock-solid', 'Classic rock anthems', 'Rock', 'active'),
('Drive Time', 'drive-time', 'Perfect soundtrack for your commute', 'Eclectic', 'active'),
('Hip-Hop Fundamentals', 'hip-hop-fundamentals', 'Essential hip-hop tracks', 'Hip-Hop', 'active'),
('Jazz Lounge', 'jazz-lounge', 'Sophisticated evening jazz', 'Jazz', 'active'),
('Punk Power Hour', 'punk-power-hour', 'High-energy punk rock', 'Punk', 'active'),
('Metal Madness', 'metal-madness', 'Heavy metal and hard rock', 'Metal', 'active'),
('Late Night Chill', 'late-night-chill', 'Downtempo beats for night owls', 'Downtempo', 'active'),
('Graveyard Shift', 'graveyard-shift', 'Music for the wee hours', 'Eclectic', 'active'),

-- TUESDAY (24 hours)
('Tuesday Sunrise', 'tuesday-sunrise', 'Gentle morning sounds', 'Acoustic', 'active'),
('Morning Brew', 'morning-brew', 'Coffee-fueled eclectic mix', 'Eclectic', 'active'),
('Singer-Songwriter Showcase', 'singer-songwriter-showcase', 'Spotlighting lyrical storytellers', 'Folk', 'active'),
('World Beat', 'world-beat', 'Percussion and rhythm from around the globe', 'World', 'active'),
('Indie Rock Hour', 'indie-rock-hour', 'Independent rock discoveries', 'Indie Rock', 'active'),
('Soul Stirrers', 'soul-stirrers', 'Deep soul and classic R&B', 'Soul/R&B', 'active'),
('Alternative Nation', 'alternative-nation', 'Alternative rock past and present', 'Alternative', 'active'),
('Electronic Evolution', 'electronic-evolution', 'The progression of electronic music', 'Electronic', 'active'),
('Country Roads', 'country-roads', 'Traditional and modern country', 'Country', 'active'),
('Funk Sessions', 'funk-sessions', 'Get down with classic funk', 'Funk', 'active'),
('Jazz Standards', 'jazz-standards', 'Timeless jazz classics', 'Jazz', 'active'),
('Noise & Space', 'noise-and-space', 'Experimental and noise music', 'Experimental', 'active'),
('Deep Night Mix', 'deep-night-mix', 'Music for the deep night hours', 'Ambient', 'active'),

-- WEDNESDAY (24 hours)
('Midweek Morning', 'midweek-morning', 'Hump day starts here', 'Pop/Rock', 'active'),
('Classical Interlude', 'classical-interlude', 'Classical music break', 'Classical', 'active'),
('Folk Underground', 'folk-underground', 'Deep folk and Americana', 'Folk', 'active'),
('Lunch Hour Favorites', 'lunch-hour-favorites', 'Midday music you love', 'Variety', 'active'),
('Afrobeat Express', 'afrobeat-express', 'West African grooves', 'Afrobeat', 'active'),
('Psychedelic Journey', 'psychedelic-journey', 'Mind-expanding psych rock', 'Psychedelic', 'active'),
('Hip-Hop Heritage', 'hip-hop-heritage', 'Golden age to modern hip-hop', 'Hip-Hop', 'active'),
('Dub Vibrations', 'dub-vibrations', 'Dub, roots, and bass', 'Dub/Reggae', 'active'),
('Shoegaze & Dream Pop', 'shoegaze-and-dream-pop', 'Ethereal guitar soundscapes', 'Shoegaze', 'active'),
('Jazz Fusion', 'jazz-fusion', 'Where jazz meets rock and funk', 'Jazz Fusion', 'active'),
('Post-Rock Horizons', 'post-rock-horizons', 'Instrumental post-rock epics', 'Post-Rock', 'active'),
('Industrial Underground', 'industrial-underground', 'Industrial and EBM', 'Industrial', 'active'),
('Night Drones', 'night-drones', 'Drone and dark ambient', 'Ambient', 'active'),

-- THURSDAY (24 hours)
('Thursday Wake-Up Call', 'thursday-wake-up-call', 'Almost Friday energy', 'Rock/Pop', 'active'),
('Baroque & Beyond', 'baroque-and-beyond', 'Early classical music', 'Classical', 'active'),
('Bluegrass & Old Time', 'bluegrass-and-old-time', 'Traditional Americana', 'Bluegrass', 'active'),
('Tropical Sounds', 'tropical-sounds', 'Caribbean and island music', 'Caribbean', 'active'),
('Garage Rock Revival', 'garage-rock-revival', 'Raw, energetic rock & roll', 'Garage Rock', 'active'),
('New Wave Nostalgia', 'new-wave-nostalgia', '80s new wave classics', 'New Wave', 'active'),
('Techno Territory', 'techno-territory', 'Pure techno beats', 'Techno', 'active'),
('Ska & Rocksteady', 'ska-and-rocksteady', 'Upbeat ska grooves', 'Ska', 'active'),
('R&B Slow Jams', 'r-and-b-slow-jams', 'Smooth R&B ballads', 'R&B', 'active'),
('Hardcore Punk', 'hardcore-punk', 'Fast and furious hardcore', 'Hardcore', 'active'),
('Doom & Stoner', 'doom-and-stoner', 'Heavy, slow, and loud', 'Doom Metal', 'active'),
('Witch House', 'witch-house', 'Dark electronic atmospheres', 'Witch House', 'active'),
('Late Night Jazz', 'late-night-jazz', 'Sophisticated late night jazz', 'Jazz', 'active'),
('Minimal Techno', 'minimal-techno', 'Stripped-down electronic beats', 'Minimal', 'active'),

-- FRIDAY (24 hours)
('Friday Morning Groove', 'friday-morning-groove', 'Get ready for the weekend', 'Soul/Funk', 'active'),
('Chamber Music', 'chamber-music', 'Small ensemble classical', 'Classical', 'active'),
('Desert Rock', 'desert-rock', 'Stoner and desert rock', 'Stoner Rock', 'active'),
('Cumbia & Chicha', 'cumbia-and-chicha', 'Latin American grooves', 'Cumbia', 'active'),
('Post-Punk Power', 'post-punk-power', 'Dark and danceable post-punk', 'Post-Punk', 'active'),
('House Music All Night', 'house-music-all-night', 'Classic house beats', 'House', 'active'),
('Weekend Warm-Up', 'weekend-warm-up', 'Eclectic pre-weekend mix', 'Variety', 'active'),
('Trap & Bass', 'trap-and-bass', 'Modern trap and bass music', 'Trap', 'active'),
('Disco Fever', 'disco-fever', 'Classic disco grooves', 'Disco', 'active'),
('Emo Night', 'emo-night', 'Emotional hardcore and emo', 'Emo', 'active'),
('Black Metal', 'black-metal', 'Atmospheric black metal', 'Black Metal', 'active'),
('Vaporwave Dreams', 'vaporwave-dreams', 'Surreal electronic nostalgia', 'Vaporwave', 'active'),
('Midnight Madness', 'midnight-madness', 'Late night party mix', 'Eclectic', 'active'),
('Deep House', 'deep-house', 'Soulful deep house music', 'Deep House', 'active'),

-- SATURDAY (24 hours)
('Saturday Morning Cartoons', 'saturday-morning-cartoons', 'Fun music to start your weekend', 'Pop', 'active'),
('Opera Hour', 'opera-hour', 'Grand opera performances', 'Opera', 'active'),
('Surf Rock', 'surf-rock', 'Reverb-drenched guitar instrumentals', 'Surf Rock', 'active'),
('Global Bass', 'global-bass', 'Bass music from around the world', 'Global Bass', 'active'),
('Britpop & Beyond', 'britpop-and-beyond', '90s British rock revival', 'Britpop', 'active'),
('Drum & Bass', 'drum-and-bass', 'High-energy jungle and D&B', 'Drum & Bass', 'active'),
('Saturday Dance Party', 'saturday-dance-party', 'Dance floor favorites', 'Dance', 'active'),
('Lo-Fi Hip-Hop', 'lo-fi-hip-hop', 'Chill beats to relax to', 'Lo-Fi', 'active'),
('Motown & Stax', 'motown-and-stax', 'Classic soul labels', 'Soul', 'active'),
('Thrash Metal', 'thrash-metal', 'Speed and aggression', 'Thrash Metal', 'active'),
('Synthwave Night', 'synthwave-night', 'Retro-futuristic synth music', 'Synthwave', 'active'),
('Ambient Soundscapes', 'ambient-soundscapes', 'Peaceful ambient textures', 'Ambient', 'active'),
('Late Night Beats', 'late-night-beats', 'Chill beats for late night', 'Beats', 'active');

-- Create host users (one per show)
INSERT INTO users (email, password)
SELECT
    'host-' || slug || '@kpbj.fm',
    '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'
FROM shows
ON CONFLICT (email) DO NOTHING;

-- Create user_metadata for all host users with fun, themed display names
INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT
    u.id,
    CASE s.slug
        -- SUNDAY
        WHEN 'sunday-morning-jazz' THEN 'Smooth Sullivan'
        WHEN 'gospel-hour' THEN 'Sister Joy'
        WHEN 'world-music-passport' THEN 'DJ Wanderlust'
        WHEN 'classical-sundays' THEN 'Maestro Chen'
        WHEN 'indie-mixtape' THEN 'Cassette Kid'
        WHEN 'soul-kitchen' THEN 'Chef Groove'
        WHEN 'acoustic-sessions' THEN 'Willow Hart'
        WHEN 'electronic-sunday' THEN 'Circuit Breaker'
        WHEN 'reggae-vibes' THEN 'Irie Lion'
        WHEN 'blues-after-dark' THEN 'Midnight Willie'
        WHEN 'experimental-sounds' THEN 'The Sound Scientist'
        WHEN 'midnight-ambient' THEN 'Lunar Drift'

        -- MONDAY
        WHEN 'monday-morning-wake-up' THEN 'Sunrise Sam'
        WHEN 'coffee-and-classics' THEN 'Morning Mozart'
        WHEN 'folk-tales' THEN 'Storyteller Sarah'
        WHEN 'midday-mix' THEN 'DJ Kaleidoscope'
        WHEN 'latin-grooves' THEN 'Ritmo Rico'
        WHEN 'rock-solid' THEN 'Rockin'' Ricky'
        WHEN 'drive-time' THEN 'Highway Holly'
        WHEN 'hip-hop-fundamentals' THEN 'DJ Knowledge'
        WHEN 'jazz-lounge' THEN 'Marcus the Velvet Voice'
        WHEN 'punk-power-hour' THEN 'Spike Voltage'
        WHEN 'metal-madness' THEN 'Thor the Destroyer'
        WHEN 'late-night-chill' THEN 'Luna Waves'
        WHEN 'graveyard-shift' THEN 'Nocturnal Nick'

        -- TUESDAY
        WHEN 'tuesday-sunrise' THEN 'Dawn Melody'
        WHEN 'morning-brew' THEN 'Java Jones'
        WHEN 'singer-songwriter-showcase' THEN 'Lyric Lou'
        WHEN 'world-beat' THEN 'Rhythm Runner'
        WHEN 'indie-rock-hour' THEN 'Garage Gary'
        WHEN 'soul-stirrers' THEN 'Deep Soul Davis'
        WHEN 'alternative-nation' THEN 'Alt Alex'
        WHEN 'electronic-evolution' THEN 'Synth Prophet'
        WHEN 'country-roads' THEN 'Honky Tonk Hannah'
        WHEN 'funk-sessions' THEN 'Funky Fred'
        WHEN 'jazz-standards' THEN 'Classic Coltrane'
        WHEN 'noise-and-space' THEN 'Static Storm'
        WHEN 'deep-night-mix' THEN 'Shadow Spinner'

        -- WEDNESDAY
        WHEN 'midweek-morning' THEN 'Hump Day Hero'
        WHEN 'classical-interlude' THEN 'Violina Virtuoso'
        WHEN 'folk-underground' THEN 'Woody Wanderer'
        WHEN 'lunch-hour-favorites' THEN 'Midday Mike'
        WHEN 'afrobeat-express' THEN 'Fela Junior'
        WHEN 'psychedelic-journey' THEN 'Cosmic Carl'
        WHEN 'hip-hop-heritage' THEN 'Old School O''Shea'
        WHEN 'dub-vibrations' THEN 'Bass Commander'
        WHEN 'shoegaze-and-dream-pop' THEN 'Reverb Rachel'
        WHEN 'jazz-fusion' THEN 'Fusion Phil'
        WHEN 'post-rock-horizons' THEN 'Epic Evan'
        WHEN 'industrial-underground' THEN 'Factory Floor Fiona'
        WHEN 'night-drones' THEN 'Drone Master D'

        -- THURSDAY
        WHEN 'thursday-wake-up-call' THEN 'Almost Friday Ali'
        WHEN 'baroque-and-beyond' THEN 'Baroque Baron'
        WHEN 'bluegrass-and-old-time' THEN 'Banjo Betty'
        WHEN 'tropical-sounds' THEN 'Island Izzy'
        WHEN 'garage-rock-revival' THEN 'Fuzz Face'
        WHEN 'new-wave-nostalgia' THEN 'Neon Nancy'
        WHEN 'techno-territory' THEN 'Berlin Bob'
        WHEN 'ska-and-rocksteady' THEN 'Skank Master Steve'
        WHEN 'r-and-b-slow-jams' THEN 'Smooth Operator Sade'
        WHEN 'hardcore-punk' THEN 'Hardcore Harry'
        WHEN 'doom-and-stoner' THEN 'Riff Lord Randy'
        WHEN 'witch-house' THEN 'Hex Hannah'
        WHEN 'late-night-jazz' THEN 'After Hours Marcus'
        WHEN 'minimal-techno' THEN 'Minimal Max'

        -- FRIDAY
        WHEN 'friday-morning-groove' THEN 'Groove Master G'
        WHEN 'chamber-music' THEN 'Quartet Quinn'
        WHEN 'desert-rock' THEN 'Desert Dan'
        WHEN 'cumbia-and-chicha' THEN 'Cumbia Carlos'
        WHEN 'post-punk-power' THEN 'Dark Wave Dave'
        WHEN 'house-music-all-night' THEN 'House Head Henry'
        WHEN 'weekend-warm-up' THEN 'Party Starter Pat'
        WHEN 'trap-and-bass' THEN '808 Tyler'
        WHEN 'disco-fever' THEN 'Disco Donna'
        WHEN 'emo-night' THEN 'Emotional Eric'
        WHEN 'black-metal' THEN 'Frost Fenrir'
        WHEN 'vaporwave-dreams' THEN 'Aesthetic Andy'
        WHEN 'midnight-madness' THEN 'Wild Card Wanda'
        WHEN 'deep-house' THEN 'Deep Space Diana'

        -- SATURDAY
        WHEN 'saturday-morning-cartoons' THEN 'Captain Cartoon'
        WHEN 'opera-hour' THEN 'Opera Oscar'
        WHEN 'surf-rock' THEN 'Wave Rider Wendy'
        WHEN 'global-bass' THEN 'Bassline Boris'
        WHEN 'britpop-and-beyond' THEN 'Britpop Barry'
        WHEN 'drum-and-bass' THEN 'Jungle Jane'
        WHEN 'saturday-dance-party' THEN 'Dance Floor Danny'
        WHEN 'lo-fi-hip-hop' THEN 'Chill Beats Charlie'
        WHEN 'motown-and-stax' THEN 'Motown Mary'
        WHEN 'thrash-metal' THEN 'Thrash Tommy'
        WHEN 'synthwave-night' THEN 'Retro Racer Rita'
        WHEN 'ambient-soundscapes' THEN 'Ambient Aurora'
        WHEN 'late-night-beats' THEN 'Beat Maker Ben'

        ELSE INITCAP(REPLACE(REPLACE(u.email, 'host-', ''), '@kpbj.fm', ''))
    END as display_name,
    CASE s.slug
        -- Generate matching full names that sound real
        WHEN 'sunday-morning-jazz' THEN 'Marcus Sullivan'
        WHEN 'gospel-hour' THEN 'Joyce Williams'
        WHEN 'world-music-passport' THEN 'Amara Okafor'
        WHEN 'classical-sundays' THEN 'Li Chen'
        WHEN 'indie-mixtape' THEN 'Alex Thompson'
        WHEN 'soul-kitchen' THEN 'Jerome Washington'
        WHEN 'acoustic-sessions' THEN 'Willow Hart'
        WHEN 'electronic-sunday' THEN 'Tyler Mitchell'
        WHEN 'reggae-vibes' THEN 'Winston Clarke'
        WHEN 'blues-after-dark' THEN 'Willie Johnson'
        WHEN 'experimental-sounds' THEN 'Dr. Adrian Pierce'
        WHEN 'midnight-ambient' THEN 'Luna Castellanos'
        WHEN 'monday-morning-wake-up' THEN 'Samantha Rodriguez'
        WHEN 'coffee-and-classics' THEN 'Wolfgang Bauer'
        WHEN 'folk-tales' THEN 'Sarah Jennings'
        WHEN 'midday-mix' THEN 'Kai Anderson'
        WHEN 'latin-grooves' THEN 'Ricardo Gonzalez'
        WHEN 'rock-solid' THEN 'Rick Stone'
        WHEN 'drive-time' THEN 'Holly Martinez'
        WHEN 'hip-hop-fundamentals' THEN 'Kenneth Wright'
        WHEN 'jazz-lounge' THEN 'Marcus Williams'
        WHEN 'punk-power-hour' THEN 'Spike Ramirez'
        WHEN 'metal-madness' THEN 'Thor Magnusson'
        WHEN 'late-night-chill' THEN 'Luna Chen'
        WHEN 'graveyard-shift' THEN 'Nick Torres'
        WHEN 'tuesday-sunrise' THEN 'Dawn Melody'
        WHEN 'morning-brew' THEN 'James Jones'
        WHEN 'singer-songwriter-showcase' THEN 'Louise Carter'
        WHEN 'world-beat' THEN 'Kwame Mensah'
        WHEN 'indie-rock-hour' THEN 'Gary Foster'
        WHEN 'soul-stirrers' THEN 'David Davis'
        WHEN 'alternative-nation' THEN 'Alex Morgan'
        WHEN 'electronic-evolution' THEN 'Elijah Prophet'
        WHEN 'country-roads' THEN 'Hannah Brooks'
        WHEN 'funk-sessions' THEN 'Frederick Brown'
        WHEN 'jazz-standards' THEN 'Miles Jefferson'
        WHEN 'noise-and-space' THEN 'Storm Rivers'
        WHEN 'deep-night-mix' THEN 'Shadow Kim'
        WHEN 'midweek-morning' THEN 'Hero Patel'
        WHEN 'classical-interlude' THEN 'Violina Rossi'
        WHEN 'folk-underground' THEN 'Woody Sanders'
        WHEN 'lunch-hour-favorites' THEN 'Michael Peterson'
        WHEN 'afrobeat-express' THEN 'Segun Adeyemi'
        WHEN 'psychedelic-journey' THEN 'Carl Morrison'
        WHEN 'hip-hop-heritage' THEN 'O''Shea Jackson'
        WHEN 'dub-vibrations' THEN 'Commander Bass'
        WHEN 'shoegaze-and-dream-pop' THEN 'Rachel Summers'
        WHEN 'jazz-fusion' THEN 'Phil Henderson'
        WHEN 'post-rock-horizons' THEN 'Evan Roberts'
        WHEN 'industrial-underground' THEN 'Fiona Steel'
        WHEN 'night-drones' THEN 'Darius Moon'
        WHEN 'thursday-wake-up-call' THEN 'Ali Hassan'
        WHEN 'baroque-and-beyond' THEN 'Baron Von Schmidt'
        WHEN 'bluegrass-and-old-time' THEN 'Betty Monroe'
        WHEN 'tropical-sounds' THEN 'Isabella Cruz'
        WHEN 'garage-rock-revival' THEN 'Fuzz Patterson'
        WHEN 'new-wave-nostalgia' THEN 'Nancy Zhang'
        WHEN 'techno-territory' THEN 'Bob Mueller'
        WHEN 'ska-and-rocksteady' THEN 'Steve Campbell'
        WHEN 'r-and-b-slow-jams' THEN 'Sade Johnson'
        WHEN 'hardcore-punk' THEN 'Harry Collins'
        WHEN 'doom-and-stoner' THEN 'Randy Pike'
        WHEN 'witch-house' THEN 'Hannah Black'
        WHEN 'late-night-jazz' THEN 'Marcus Williams'
        WHEN 'minimal-techno' THEN 'Max Klein'
        WHEN 'friday-morning-groove' THEN 'Gerald Green'
        WHEN 'chamber-music' THEN 'Quinn Abbott'
        WHEN 'desert-rock' THEN 'Daniel Coyote'
        WHEN 'cumbia-and-chicha' THEN 'Carlos Vargas'
        WHEN 'post-punk-power' THEN 'David Curtis'
        WHEN 'house-music-all-night' THEN 'Henry Walker'
        WHEN 'weekend-warm-up' THEN 'Patricia Lee'
        WHEN 'trap-and-bass' THEN 'Tyler Young'
        WHEN 'disco-fever' THEN 'Donna Summer'
        WHEN 'emo-night' THEN 'Eric Heartbreak'
        WHEN 'black-metal' THEN 'Fenrir Frost'
        WHEN 'vaporwave-dreams' THEN 'Andy Digital'
        WHEN 'midnight-madness' THEN 'Wanda Wild'
        WHEN 'deep-house' THEN 'Diana Prince'
        WHEN 'saturday-morning-cartoons' THEN 'Captain Carl'
        WHEN 'opera-hour' THEN 'Oscar Pavarotti'
        WHEN 'surf-rock' THEN 'Wendy Mavericks'
        WHEN 'global-bass' THEN 'Boris Petrov'
        WHEN 'britpop-and-beyond' THEN 'Barry Gallagher'
        WHEN 'drum-and-bass' THEN 'Jane Jungler'
        WHEN 'saturday-dance-party' THEN 'Danny Beats'
        WHEN 'lo-fi-hip-hop' THEN 'Charlie Vibes'
        WHEN 'motown-and-stax' THEN 'Mary Ross'
        WHEN 'thrash-metal' THEN 'Tommy Mayhem'
        WHEN 'synthwave-night' THEN 'Rita Neon'
        WHEN 'ambient-soundscapes' THEN 'Aurora Fields'
        WHEN 'late-night-beats' THEN 'Benjamin Producer'
        ELSE INITCAP(REPLACE(REPLACE(REPLACE(u.email, 'host-', ''), '@kpbj.fm', ''), '-', ' '))
    END as full_name,
    'Host' as user_role
FROM users u
JOIN shows s ON u.email = 'host-' || s.slug || '@kpbj.fm';

-- Link hosts to their shows
INSERT INTO show_hosts (show_id, user_id, role, is_primary)
SELECT s.id, u.id, 'host', TRUE
FROM shows s
JOIN users u ON u.email = 'host-' || s.slug || '@kpbj.fm';

-- Add Willow Hart as co-host to indie-mixtape for testing multi-show selector
INSERT INTO show_hosts (show_id, user_id, role, is_primary)
SELECT
    (SELECT id FROM shows WHERE slug = 'indie-mixtape'),
    (SELECT id FROM users WHERE email = 'host-acoustic-sessions@kpbj.fm'),
    'co-host',
    FALSE;

-- Create host details with bios and social links
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

-- Create schedule templates: each show airs once per week at a specific time
-- Distribute shows across the week to fill 24/7
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
-- All templates are effective from 30 days ago and currently active (effective_until = NULL)
INSERT INTO schedule_template_validity (template_id, effective_from, effective_until)
SELECT id, CURRENT_DATE - INTERVAL '30 days', NULL
FROM schedule_templates;

-- Generate episodes for the past 2 weeks based on show schedules
-- Each show gets 2 episodes (one per week for 2 weeks)
INSERT INTO episodes (show_id, title, slug, description, status, scheduled_at, published_at, created_by)
SELECT
    s.id as show_id,
    s.title || ' - Episode ' || (row_number() OVER (PARTITION BY s.id ORDER BY generate_series DESC))::text as title,
    lower(regexp_replace(s.title, '[^a-zA-Z0-9]+', '-', 'g')) || '-' || to_char(generate_series, 'YYYY-MM-DD') as slug,
    'A great episode of ' || s.title || ' from ' || to_char(generate_series, 'FMMonth DD, YYYY') as description,
    'published' as status,
    -- interpret date + time in show's timezone, then convert to UTC
    (generate_series::date::text || ' ' || st.start_time)::timestamp AT TIME ZONE st.timezone as scheduled_at,
    (generate_series::date::text || ' ' || st.start_time)::timestamp AT TIME ZONE st.timezone as published_at,
    u.id as created_by
FROM shows s
JOIN schedule_templates st ON st.show_id = s.id
JOIN schedule_template_validity stv ON stv.template_id = st.id
JOIN users u ON u.email = 'host-' || s.slug || '@kpbj.fm'
CROSS JOIN LATERAL (
    -- Generate dates for the past 2 weeks for this show's day of week
    SELECT generate_series
    FROM generate_series(
        CURRENT_DATE - INTERVAL '14 days',
        CURRENT_DATE - INTERVAL '1 day',
        INTERVAL '1 day'
    ) AS generate_series
    WHERE EXTRACT(DOW FROM generate_series)::INTEGER =
        CASE st.day_of_week::TEXT
            WHEN 'sunday' THEN 0
            WHEN 'monday' THEN 1
            WHEN 'tuesday' THEN 2
            WHEN 'wednesday' THEN 3
            WHEN 'thursday' THEN 4
            WHEN 'friday' THEN 5
            WHEN 'saturday' THEN 6
        END
) dates
WHERE stv.effective_from <= CURRENT_DATE - INTERVAL '14 days'
  AND (stv.effective_until IS NULL OR stv.effective_until > CURRENT_DATE - INTERVAL '1 day')
ORDER BY s.id, generate_series DESC;

-- Create staff user (password: "password")
INSERT INTO users (email, password) VALUES
  ('staff@kpbj.fm', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

-- Create staff user_metadata
INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'Staff Member', 'KPBJ Staff', 'Staff'
FROM users u WHERE u.email = 'staff@kpbj.fm';

-- Create regular users (password: "password")
INSERT INTO users (email, password) VALUES
  ('listener@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('musicfan@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('vinyl.collector@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('portland.local@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'),
  ('nightowl@example.com', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

-- Create regular user_metadata
INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'RadioHead42', 'Alex Thompson', 'User'
FROM users u WHERE u.email = 'listener@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'JazzCat', 'Maria Santos', 'User'
FROM users u WHERE u.email = 'musicfan@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'VinylJunkie', 'Sam Chen', 'User'
FROM users u WHERE u.email = 'vinyl.collector@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'PDXBeats', 'Jordan Rivera', 'User'
FROM users u WHERE u.email = 'portland.local@example.com';

INSERT INTO user_metadata (user_id, display_name, full_name, user_role)
SELECT u.id, 'MidnightListener', 'Casey Park', 'User'
FROM users u WHERE u.email = 'nightowl@example.com';

-- Create event tags
INSERT INTO event_tags (name) VALUES
  ('Live Music'),
  ('DJ Night'),
  ('Fundraiser'),
  ('Community'),
  ('All Ages'),
  ('21+'),
  ('Free'),
  ('Ticketed')
ON CONFLICT (name) DO NOTHING;

-- Create mock events (1 per week for past 2 months on Fridays/Saturdays)
INSERT INTO events (title, slug, description, starts_at, ends_at, location_name, location_address, status, author_id)
SELECT * FROM (VALUES
  (
    'Summer Block Party',
    'summer-block-party-2025-08-08',
    'Join us for an afternoon of live music, local vendors, and community celebration! Featuring performances from local bands and DJs. Free admission, all ages welcome.',
    '2025-08-08 14:00:00-07'::timestamptz,
    '2025-08-08 22:00:00-07'::timestamptz,
    'KPBJ Community Plaza',
    '123 Radio Street, Portland, OR 97201',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Late Night Techno: DJ Aurelia',
    'late-night-techno-dj-aurelia-2025-08-16',
    'Experience cutting-edge techno with special guest DJ Aurelia from Berlin. Dark, hypnotic beats until dawn. 21+ only.',
    '2025-08-16 22:00:00-07'::timestamptz,
    '2025-08-17 03:00:00-07'::timestamptz,
    'The Underground',
    '456 Basement Ave, Portland, OR 97214',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Record Fair & Swap Meet',
    'record-fair-swap-meet-2025-08-22',
    'Dig through crates of vinyl from local collectors and record stores. Buy, sell, trade! Free admission. Rare finds and affordable gems.',
    '2025-08-22 10:00:00-07'::timestamptz,
    '2025-08-22 16:00:00-07'::timestamptz,
    'KPBJ Studio Parking Lot',
    '123 Radio Street, Portland, OR 97201',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Indie Rock Showcase',
    'indie-rock-showcase-2025-08-30',
    'Three local indie rock bands take the stage! Featuring The Static Waves, Moonlight Drivers, and Paper Tigers. $10 at the door.',
    '2025-08-30 19:00:00-07'::timestamptz,
    '2025-08-30 23:30:00-07'::timestamptz,
    'The Velvet Room',
    '789 Music Lane, Portland, OR 97212',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'KPBJ Fundraiser: Jazz Night',
    'kpbj-fundraiser-jazz-night-2025-09-05',
    'Support community radio! Live jazz performances, silent auction, and special guests. All proceeds benefit KPBJ programming. $25 suggested donation.',
    '2025-09-05 18:00:00-07'::timestamptz,
    '2025-09-05 22:00:00-07'::timestamptz,
    'The Crystal Ballroom',
    '1332 W Burnside St, Portland, OR 97209',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'All Ages Punk Matinee',
    'all-ages-punk-matinee-2025-09-13',
    'Afternoon punk rock for all ages! Four fast and loud bands. Safe space, positive vibes. $8 admission, under 16 free with adult.',
    '2025-09-13 14:00:00-07'::timestamptz,
    '2025-09-13 18:00:00-07'::timestamptz,
    'The Firehouse',
    '555 Youth Plaza, Portland, OR 97202',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Open Mic Night',
    'open-mic-night-2025-09-19',
    'Bring your instrument, poetry, or comedy! Sign up starts at 7pm, performances at 8pm. Free admission, donations welcome.',
    '2025-09-19 19:00:00-07'::timestamptz,
    '2025-09-19 23:00:00-07'::timestamptz,
    'KPBJ Live Room',
    '123 Radio Street, Portland, OR 97201',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Autumn DJ Showcase',
    'autumn-dj-showcase-2025-09-27',
    'Celebrate the changing season with five KPBJ DJs spinning their favorite fall vibes. Hip-hop, soul, funk, and more. Free event!',
    '2025-09-27 20:00:00-07'::timestamptz,
    '2025-09-28 01:00:00-07'::timestamptz,
    'Rooftop Garden Bar',
    '999 Sky View Terrace, Portland, OR 97205',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Halloween Costume Party & Dance Night',
    'halloween-costume-party-dance-night-2025-10-04',
    'Kick off spooky season with a costume party featuring DJs spinning goth, darkwave, and Halloween classics. Costume contest with prizes! 21+.',
    '2025-10-04 21:00:00-07'::timestamptz,
    '2025-10-05 02:00:00-07'::timestamptz,
    'The Underground',
    '456 Basement Ave, Portland, OR 97214',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Local Legends: Portland Music History Panel',
    'local-legends-portland-music-history-panel-2025-10-11',
    'Join us for an intimate conversation with Portland music scene veterans discussing the city''s rich musical heritage. Q&A and book signing to follow. Free admission.',
    '2025-10-11 18:00:00-07'::timestamptz,
    '2025-10-11 20:30:00-07'::timestamptz,
    'KPBJ Live Room',
    '123 Radio Street, Portland, OR 97201',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'Acoustic Songwriter Circle',
    'acoustic-songwriter-circle-2025-10-18',
    'An intimate evening featuring five local singer-songwriters performing original material. All ages welcome. $8 suggested donation.',
    '2025-10-18 19:30:00-07'::timestamptz,
    '2025-10-18 22:00:00-07'::timestamptz,
    'The Velvet Room',
    '789 Music Lane, Portland, OR 97212',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  ),
  (
    'KPBJ Fall Fundraiser: Silent Auction & Live Music',
    'kpbj-fall-fundraiser-silent-auction-2025-10-25',
    'Support community radio! Silent auction featuring local art, concert tickets, and vintage gear. Live performances by KPBJ host bands. $20 admission includes appetizers and one drink.',
    '2025-10-25 17:00:00-07'::timestamptz,
    '2025-10-25 21:00:00-07'::timestamptz,
    'KPBJ Community Plaza',
    '123 Radio Street, Portland, OR 97201',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  )
) AS events_data;

-- Assign tags to events
INSERT INTO event_tag_assignments (event_id, tag_id)
SELECT e.id, t.id
FROM events e
JOIN event_tags t ON t.name IN (
  CASE e.slug
    WHEN 'summer-block-party-2025-08-08' THEN 'Live Music'
    WHEN 'summer-block-party-2025-08-08' THEN 'Community'
    WHEN 'summer-block-party-2025-08-08' THEN 'All Ages'
    WHEN 'summer-block-party-2025-08-08' THEN 'Free'
    WHEN 'late-night-techno-dj-aurelia-2025-08-16' THEN 'DJ Night'
    WHEN 'late-night-techno-dj-aurelia-2025-08-16' THEN '21+'
    WHEN 'late-night-techno-dj-aurelia-2025-08-16' THEN 'Ticketed'
    WHEN 'record-fair-swap-meet-2025-08-22' THEN 'Community'
    WHEN 'record-fair-swap-meet-2025-08-22' THEN 'All Ages'
    WHEN 'record-fair-swap-meet-2025-08-22' THEN 'Free'
    WHEN 'indie-rock-showcase-2025-08-30' THEN 'Live Music'
    WHEN 'indie-rock-showcase-2025-08-30' THEN 'Ticketed'
    WHEN 'kpbj-fundraiser-jazz-night-2025-09-05' THEN 'Live Music'
    WHEN 'kpbj-fundraiser-jazz-night-2025-09-05' THEN 'Fundraiser'
    WHEN 'kpbj-fundraiser-jazz-night-2025-09-05' THEN 'Ticketed'
    WHEN 'all-ages-punk-matinee-2025-09-13' THEN 'Live Music'
    WHEN 'all-ages-punk-matinee-2025-09-13' THEN 'All Ages'
    WHEN 'all-ages-punk-matinee-2025-09-13' THEN 'Ticketed'
    WHEN 'open-mic-night-2025-09-19' THEN 'Live Music'
    WHEN 'open-mic-night-2025-09-19' THEN 'Community'
    WHEN 'open-mic-night-2025-09-19' THEN 'All Ages'
    WHEN 'open-mic-night-2025-09-19' THEN 'Free'
    WHEN 'autumn-dj-showcase-2025-09-27' THEN 'DJ Night'
    WHEN 'autumn-dj-showcase-2025-09-27' THEN 'Community'
    WHEN 'autumn-dj-showcase-2025-09-27' THEN 'Free'
    WHEN 'halloween-costume-party-dance-night-2025-10-04' THEN 'DJ Night'
    WHEN 'halloween-costume-party-dance-night-2025-10-04' THEN '21+'
    WHEN 'halloween-costume-party-dance-night-2025-10-04' THEN 'Ticketed'
    WHEN 'local-legends-portland-music-history-panel-2025-10-11' THEN 'Community'
    WHEN 'local-legends-portland-music-history-panel-2025-10-11' THEN 'All Ages'
    WHEN 'local-legends-portland-music-history-panel-2025-10-11' THEN 'Free'
    WHEN 'acoustic-songwriter-circle-2025-10-18' THEN 'Live Music'
    WHEN 'acoustic-songwriter-circle-2025-10-18' THEN 'All Ages'
    WHEN 'acoustic-songwriter-circle-2025-10-18' THEN 'Ticketed'
    WHEN 'kpbj-fall-fundraiser-silent-auction-2025-10-25' THEN 'Live Music'
    WHEN 'kpbj-fall-fundraiser-silent-auction-2025-10-25' THEN 'Fundraiser'
    WHEN 'kpbj-fall-fundraiser-silent-auction-2025-10-25' THEN 'Ticketed'
    WHEN 'kpbj-fall-fundraiser-silent-auction-2025-10-25' THEN 'Community'
  END
);

-- Create blog tags
INSERT INTO blog_tags (name) VALUES
  ('award'),
  ('community'),
  ('underground'),
  ('interviews'),
  ('local-scene'),
  ('fundraising'),
  ('new-shows'),
  ('music-discovery')
ON CONFLICT (name) DO NOTHING;

-- Create mock blog posts (1 per week for past 2 months)
INSERT INTO blog_posts (title, slug, content, excerpt, author_id, status, published_at)
SELECT * FROM (VALUES
  (
    'KPBJ Wins 2025 Community Radio Excellence Award',
    'kpbj-wins-2025-community-radio-excellence-award',
    E'We are thrilled to announce that KPBJ 95.9FM has been recognized with the prestigious **2025 Community Radio Excellence Award** from the National Association of Community Broadcasters!\n\nThis honor recognizes our commitment to serving the Portland community through diverse programming, local artist support, and meaningful community engagement. The award specifically highlighted our innovative approach to combining traditional radio broadcasting with modern digital engagement.\n\n> "This award belongs to our entire community. From our dedicated volunteer hosts to our loyal listeners and supporters, everyone plays a vital role in making KPBJ the vibrant community resource it is today."\n> \n> — Station Manager Sarah Chen\n\nThe judging panel noted KPBJ''s exceptional programming diversity, with 84 unique shows covering everything from jazz and classical to experimental electronic and punk rock. They also praised our commitment to amplifying underrepresented voices in the music industry and providing a platform for emerging local artists.\n\n## What This Means for KPBJ\n\n- National recognition for our programming excellence\n- Increased visibility in the community radio landscape\n- Validation of our community-first approach\n- Inspiration to continue pushing boundaries\n\nThank you to everyone who makes KPBJ possible. Here''s to many more years of community-powered radio!',
    'KPBJ 95.9FM receives the 2025 Community Radio Excellence Award for outstanding programming and community engagement.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-04 10:00:00-07'::timestamptz
  ),
  (
    'Behind the Scenes: A Day in the Life of a KPBJ Host',
    'behind-the-scenes-day-in-life-kpbj-host',
    E'Ever wondered what it takes to produce a radio show? We sat down with **Marcus Williams**, host of "Late Night Jazz" every Thursday at 9pm, to get an inside look at the creative process behind community radio.\n\n## Morning: Digging Through the Crates\n\n> "I usually start my week by listening through new releases and revisiting classics. For a two-hour jazz show, I''ll typically prepare about 3-4 hours of material. You never know when you''ll want to extend a vibe or need to pivot the mood."\n> \n> — Marcus Williams\n\nMarcus spends Monday and Tuesday mornings at local record shops, browsing both new arrivals and used sections. "Some of my best discoveries have come from the dollar bins," he laughs.\n\n## Mid-Week: Building the Flow\n\nBy Wednesday, Marcus starts sequencing his show. "It''s like creating a mixtape for a friend. You want to take them on a journey. I usually start mellow, build energy in the middle hour, then wind down for the late-night crowd."\n\n## Show Night: Going Live\n\nThursday evening, Marcus arrives at the studio **two hours early**:\n\n- Reviews his notes\n- Double-checks that all records are cued up\n- Runs a sound check\n- Prepares backup selections\n\n"Once we go live, it''s just me, the music, and whoever''s listening out there in the Portland night."\n\n## The Reward\n\n> "The best part is the texts and calls we get. Knowing that someone driving home from a late shift or studying for an exam is vibing to the music you selected—that''s what community radio is all about."\n\n*Tune in to Late Night Jazz every Thursday at 9pm on KPBJ 95.9FM.*',
    'Meet Marcus Williams, host of Late Night Jazz, and discover what goes into creating a weekly radio show.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-11 14:00:00-07'::timestamptz
  ),
  (
    'New Shows Premiering This Fall: Expanding the KPBJ Sound',
    'new-shows-premiering-fall-2025',
    E'We''re excited to announce **three brand new shows** joining the KPBJ lineup this September!\n\n## "Queer Frequencies" - Mondays 8-10pm\n\nHosted by **DJ Phoenix**, this show celebrates LGBTQ+ artists and allies across all genres. From underground queer punk to mainstream pop allies, "Queer Frequencies" creates space for authentic voices and stories from the community.\n\n**What to expect:**\n- Diverse LGBTQ+ artists from all decades\n- Interviews with local queer musicians\n- Safe space for authentic expression\n\n## "Immigrant Stories" - Wednesdays 6-7pm\n\nProduced by a collective of first and second-generation immigrants, this show blends music from around the world with personal narratives about identity, belonging, and cultural fusion. Each week focuses on a different community within Portland''s diverse immigrant population.\n\n**Featured communities include:**\n- Vietnamese diaspora\n- East African refugees\n- Latin American immigrants\n- Middle Eastern communities\n\n## "Beats & Rhymes: Hip-Hop History" - Saturdays 3-5pm\n\nDJ Knowledge takes listeners on a **chronological journey through hip-hop**, starting from the Bronx in the 1970s and moving decade by decade. Each episode focuses on a specific year, exploring the cultural context and musical innovations that shaped the genre.\n\n---\n\nAll three shows represent KPBJ''s ongoing commitment to diverse, community-focused programming. **Tune in starting September 8th!**\n\n### Want to Host a Show?\n\nWe''re always looking for passionate community members with unique perspectives and musical knowledge. Visit [kpbj.fm/become-a-host](https://kpbj.fm/become-a-host) to learn more about our training program.',
    'Three exciting new shows join KPBJ this fall, expanding our commitment to diverse community voices.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-18 09:00:00-07'::timestamptz
  ),
  (
    'Portland''s Underground Music Scene: Why Local Venues Matter',
    'portland-underground-music-scene-venues-matter',
    E'In an era of streaming algorithms and viral TikTok sounds, Portland''s independent music venues remain **vital spaces for authentic community** and artistic development. But these venues face ongoing challenges that threaten their survival.\n\n## The Ecosystem of Live Music\n\nLocal venues like **The Firehouse**, **The Underground**, and **Velvet Room** aren''t just concert spaces—they''re community centers where artists develop their craft, fans discover new sounds, and cultural movements take root.\n\n> "Every major Portland band you know started by playing small venues. You can''t skip those steps. The intimacy of a 100-capacity room teaches you how to connect with an audience in ways that streaming stats never will."\n> \n> — Sarah Kim, owner of The Firehouse\n\n## Economic Pressures\n\nRising rents, noise complaints, and increased insurance costs have forced several beloved venues to close in recent years. **The Eastside Music Hall** and **The Underground Station** both shuttered in 2024, leaving significant gaps in the city''s music infrastructure.\n\n### Challenges Facing Venues:\n\n- Rising commercial rent costs\n- Noise complaints from new residential developments\n- Increased liability insurance premiums\n- Competition from larger corporate venues\n- Thin profit margins on ticket sales\n\n## How You Can Help\n\n1. **Buy tickets in advance** - This helps venues plan and pay deposits\n2. **Purchase drinks and merch** - Most venues make minimal profit on tickets\n3. **Respect noise ordinances** - Be considerate to neighbors\n4. **Show up for matinees** - All-ages afternoon shows need support too\n5. **Tip your bartenders and sound techs** - They make it all possible\n\n---\n\nKPBJ partners with Portland venues to promote local shows and support emerging artists. Check our [events calendar](https://kpbj.fm/events) to find shows happening this week!',
    'Exploring the vital role of independent venues in Portland''s music ecosystem and how you can support them.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-25 11:00:00-07'::timestamptz
  ),
  (
    'Fundraising Update: Summer Pledge Drive Results',
    'fundraising-update-summer-2025-pledge-drive',
    E'Thank you, Portland! Our **Summer 2025 Pledge Drive** exceeded all expectations, raising **$87,000** to support KPBJ''s operations and programming.\n\n## By the Numbers\n\n- **1,247** individual donors\n- **$70** average contribution\n- **412** new monthly sustainers\n- **89%** of our annual funding goal reached\n\n## What Your Support Provides\n\n- **Equipment maintenance** - New microphones, mixers, and transmitter upkeep\n- **Host training** - Workshops and technical education for volunteer DJs\n- **Music licensing** - Fees required to legally broadcast music\n- **Studio upgrades** - Improved recording capabilities for interviews and performances\n- **Community events** - Free concerts, workshops, and public gatherings\n\n## Special Thanks\n\nWe want to recognize several exceptional supporters:\n\n- **Sustainer Champion**: The Rodriguez Family, who committed to $100/month for the next year\n- **Business Partner**: Jackpot Records, who matched donations up to $5,000\n- **Volunteer Hero**: DJ Marcus (Late Night Jazz), who hosted 12 hours of live pledge drive programming\n\n## Not Too Late to Contribute\n\nWhile the official drive has ended, KPBJ accepts donations year-round. Monthly sustainers provide crucial stable funding that helps us plan programming and invest in improvements. Even **$10/month** makes a real difference.\n\nVisit [kpbj.fm/donate](https://kpbj.fm/donate) to set up your monthly contribution today. Every dollar directly supports independent, community-powered radio.\n\n---\n\n*Thank you for believing in the power of community media!*',
    'Summer 2025 pledge drive raises $87,000, exceeding goals and securing stable funding for community radio.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-01 10:00:00-07'::timestamptz
  ),
  (
    'Discovering Hidden Gems: KPBJ''s Guide to Vinyl Shopping in Portland',
    'discovering-hidden-gems-vinyl-shopping-portland',
    E'Portland''s vinyl scene offers incredible opportunities for music discovery, from legendary record stores to pop-up sales and estate finds. Our DJs share their favorite spots and strategies for building a collection.\n\n## The Legendary Stores\n\n### Jackpot Records (Hawthorne)\n> "The staff picks here are incredible. They really know their catalog and aren''t afraid to recommend obscure stuff."\n> \n> — DJ Phoenix\n\n### Everyday Music (Multiple locations)\nWith three locations and massive inventory, Everyday Music is where many DJs find rare imports and deleted pressings.\n\n### Mississippi Records\nSpecializing in reissues of global folk music, gospel, and vintage country.\n\n> "This place expanded my entire musical worldview."\n> \n> — DJ Knowledge\n\n## Hidden Gems\n\n- **Exiled Records** (SE Portland) - Punk, hardcore, and metal specialist with knowledgeable staff and regular in-store performances\n- **Tender Loving Empire** (Alberta) - Local artists and Pacific Northwest labels. Great for discovering Portland musicians\n\n## Tips from KPBJ DJs\n\n1. **Shop dollar bins first** - "I''ve found Coltrane and Mingus records for $1," says Marcus Williams. "Never skip the cheap stuff."\n2. **Ask about listening stations** - Most stores let you preview before buying\n3. **Follow store social media** - Many announce new arrivals and sales online\n4. **Bring a list** - But stay open to surprises\n5. **Check condition carefully** - Look at the vinyl itself, not just the sleeve\n6. **Support buying local** - Many Portland artists sell directly to local shops\n\n## Monthly Record Fairs\n\nThe **Portland Record Show** happens the last Sunday of every month at the Doubletree Hotel. Over 50 vendors from across the Pacific Northwest bring crates of vinyl, CDs, and music memorabilia.\n\nKPBJ will be hosting our own **Record Fair & Swap Meet** on August 22nd—check our [events page](https://kpbj.fm/events) for details!',
    'KPBJ DJs share their favorite Portland record stores and vinyl shopping strategies for music collectors.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-08 13:00:00-07'::timestamptz
  ),
  (
    'Community Voices: How KPBJ Listeners Shape Our Programming',
    'community-voices-listeners-shape-programming',
    E'KPBJ isn''t just radio *for* the community—it''s radio *by* the community. Here''s how listener feedback and participation directly influence our programming decisions.\n\n## The Listener Survey\n\nEach year, we survey our audience about their listening habits, favorite shows, and what they''d like to hear more of. This year''s survey received **2,300+ responses** and revealed some fascinating patterns:\n\n- **67%** listen via streaming rather than traditional FM\n- **Most popular time slot**: Weeknight 8-11pm drive home hours\n- **Top requested new genre**: More experimental electronic and ambient\n- **Most appreciated aspect**: "Musical diversity I can''t find anywhere else"\n\n## Rotating Guest DJ Spots\n\nBased on listener requests, we''ve implemented **"Guest DJ Fridays"** where community members can apply to program a one-hour set. Applications open quarterly, and selected DJs receive training and technical support.\n\n> "I never thought I''d be on actual radio. The KPBJ team made it so welcoming and fun."\n> \n> — Priya Sharma, August Guest DJ (Bollywood disco remixes)\n\n## Call-In Requests and Dedications\n\nSeveral shows accept live requests via text and phone. These interactions create real-time dialogue between DJs and listeners:\n\n> "Someone texted during my show requesting an obscure Brazilian psych record. I didn''t have it, but I found something similar. We ended up texting back and forth about the genre. That''s the magic of community radio—you''re not just playing music into the void."\n> \n> — DJ Aurora\n\n## Your Voice Matters\n\nWe read every email, respond to social media comments, and seriously consider all feedback. The next listener survey launches in **November**—your input helps shape KPBJ''s future.\n\n**Have programming ideas?**\n- Email: [feedback@kpbj.fm](mailto:feedback@kpbj.fm)\n- Text during any show\n- Submit suggestions via our website\n\n*Community radio only works when the community participates!*',
    'How KPBJ incorporates listener feedback, surveys, and community participation into programming decisions.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-15 10:30:00-07'::timestamptz
  ),
  (
    'Looking Ahead: KPBJ''s Plans for 2026',
    'looking-ahead-kpbj-plans-2026',
    E'As we approach the end of 2025, we''re excited to share our vision for KPBJ''s growth and evolution in the coming year.\n\n## Technical Upgrades\n\n### New Transmitter\nWe''re investing in upgraded broadcast equipment that will improve signal clarity and expand our coverage area.\n\n### Remote Broadcasting\nNew mobile equipment will allow us to broadcast live from community events, concerts, and festivals throughout Portland.\n\n### Improved Streaming\nWorking with technical partners to enhance our web player and launch a dedicated mobile app.\n\n## Programming Expansions\n\n### Podcast Studio\nConverting an unused storage room into a podcast production space available to community members. This will allow local podcasters to access professional equipment and training.\n\n### Youth Radio Program\nPartnering with **Portland Public Schools** to offer radio production classes and mentorship. Students will produce monthly shows featuring youth perspectives on music, culture, and social issues.\n\n### Live Performance Space\nPlanning a small performance venue for intimate concerts, workshops, and community gatherings. All events will be broadcast live.\n\n## Community Partnerships\n\n- **Local Venues** - Expanding partnerships with Portland music venues to promote local shows and develop co-produced content\n- **Arts Organizations** - Collaborating with museums, theaters, and galleries to create programming that bridges music with other art forms\n- **Nonprofit Network** - Working with other community organizations to amplify important local initiatives and social programs\n\n## How You Can Help\n\nThese ambitious plans require community support:\n\n- **Monthly sustainers** provide stable funding for long-term projects\n- **Volunteer skills** in construction, marketing, or teaching can directly support new initiatives\n- **Community feedback** helps us prioritize which projects matter most\n\n## Stay Involved\n\nWe''ll share regular updates about these projects throughout 2026. Follow us on social media, subscribe to our monthly newsletter, or attend our quarterly community meetings to stay informed and involved.\n\n---\n\n*Thank you for making KPBJ possible. The best is yet to come!*',
    'KPBJ announces ambitious plans for 2026 including technical upgrades, new programming, and expanded community partnerships.',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-22 09:00:00-07'::timestamptz
  )
) AS blog_data;

-- Assign tags to blog posts
INSERT INTO blog_post_tags (post_id, tag_id)
SELECT bp.id, bt.id
FROM blog_posts bp
JOIN blog_tags bt ON bt.name IN (
  CASE bp.slug
    WHEN 'kpbj-wins-2025-community-radio-excellence-award' THEN 'award'
    WHEN 'kpbj-wins-2025-community-radio-excellence-award' THEN 'community'
    WHEN 'behind-the-scenes-day-in-life-kpbj-host' THEN 'interviews'
    WHEN 'new-shows-premiering-fall-2025' THEN 'new-shows'
    WHEN 'new-shows-premiering-fall-2025' THEN 'community'
    WHEN 'portland-underground-music-scene-venues-matter' THEN 'local-scene'
    WHEN 'portland-underground-music-scene-venues-matter' THEN 'underground'
    WHEN 'portland-underground-music-scene-venues-matter' THEN 'community'
    WHEN 'fundraising-update-summer-2025-pledge-drive' THEN 'fundraising'
    WHEN 'fundraising-update-summer-2025-pledge-drive' THEN 'community'
    WHEN 'discovering-hidden-gems-vinyl-shopping-portland' THEN 'music-discovery'
    WHEN 'discovering-hidden-gems-vinyl-shopping-portland' THEN 'local-scene'
    WHEN 'community-voices-listeners-shape-programming' THEN 'community'
    WHEN 'looking-ahead-kpbj-plans-2026' THEN 'community'
    WHEN 'looking-ahead-kpbj-plans-2026' THEN 'new-shows'
  END
);

-- Generate track lists for each episode
-- Each episode gets 8-12 tracks based on show duration and genre
WITH track_library AS (
    SELECT * FROM (VALUES
        -- Jazz tracks
        ('Take Five', 'Dave Brubeck Quartet', 'Time Out', 1959, '5:24', 'Columbia', ARRAY['Jazz', 'Jazz Fusion']),
        ('So What', 'Miles Davis', 'Kind of Blue', 1959, '9:22', 'Columbia', ARRAY['Jazz', 'Jazz Fusion']),
        ('A Love Supreme (Part 1)', 'John Coltrane', 'A Love Supreme', 1965, '7:42', 'Impulse!', ARRAY['Jazz', 'Jazz Fusion']),
        ('Birdland', 'Weather Report', 'Heavy Weather', 1977, '5:59', 'Columbia', ARRAY['Jazz', 'Jazz Fusion']),
        ('Cantaloupe Island', 'Herbie Hancock', 'Empyrean Isles', 1964, '5:32', 'Blue Note', ARRAY['Jazz', 'Jazz Fusion']),
        ('Mercy, Mercy, Mercy', 'Cannonball Adderley', 'Mercy, Mercy, Mercy!', 1966, '6:52', 'Capitol', ARRAY['Jazz', 'Jazz Fusion']),
        ('Blue Train', 'John Coltrane', 'Blue Train', 1957, '10:43', 'Blue Note', ARRAY['Jazz', 'Jazz Fusion']),
        ('Watermelon Man', 'Herbie Hancock', 'Takin'' Off', 1962, '4:07', 'Blue Note', ARRAY['Jazz', 'Jazz Fusion']),

        -- Rock tracks
        ('Stairway to Heaven', 'Led Zeppelin', 'Led Zeppelin IV', 1971, '8:02', 'Atlantic', ARRAY['Rock', 'Garage Rock', 'Psychedelic']),
        ('Bohemian Rhapsody', 'Queen', 'A Night at the Opera', 1975, '5:55', 'EMI', ARRAY['Rock', 'Garage Rock', 'Pop/Rock']),
        ('While My Guitar Gently Weeps', 'The Beatles', 'The Beatles (White Album)', 1968, '4:45', 'Apple', ARRAY['Rock', 'Psychedelic', 'Pop/Rock']),
        ('Kashmir', 'Led Zeppelin', 'Physical Graffiti', 1975, '8:37', 'Swan Song', ARRAY['Rock', 'Stoner Rock', 'Desert Rock']),
        ('Comfortably Numb', 'Pink Floyd', 'The Wall', 1979, '6:23', 'Columbia', ARRAY['Rock', 'Psychedelic']),
        ('Won''t Get Fooled Again', 'The Who', 'Who''s Next', 1971, '8:32', 'Track', ARRAY['Rock', 'Garage Rock']),

        -- Electronic tracks
        ('Windowlicker', 'Aphex Twin', 'Windowlicker EP', 1999, '6:08', 'Warp', ARRAY['Electronic', 'Experimental', 'Techno', 'Minimal']),
        ('Blue Monday', 'New Order', 'Power, Corruption & Lies', 1983, '7:29', 'Factory', ARRAY['Electronic', 'New Wave', 'Synthwave']),
        ('Around the World', 'Daft Punk', 'Homework', 1997, '7:09', 'Virgin', ARRAY['Electronic', 'House', 'Deep House']),
        ('Xtal', 'Aphex Twin', 'Selected Ambient Works 85-92', 1992, '4:51', 'Apollo', ARRAY['Electronic', 'Ambient', 'Experimental']),
        ('Age Of Love', 'Age of Love', 'The Age of Love', 1990, '6:40', 'React', ARRAY['Electronic', 'Techno']),
        ('Papua New Guinea', 'The Future Sound of London', 'Accelerator', 1991, '8:47', 'Jumpin'' & Pumpin''', ARRAY['Electronic', 'Ambient']),

        -- Hip-Hop tracks
        ('N.Y. State of Mind', 'Nas', 'Illmatic', 1994, '4:54', 'Columbia', ARRAY['Hip-Hop', 'Lo-Fi', 'Trap']),
        ('Juicy', 'The Notorious B.I.G.', 'Ready to Die', 1994, '5:02', 'Bad Boy', ARRAY['Hip-Hop', 'Trap']),
        ('The Message', 'Grandmaster Flash and the Furious Five', 'The Message', 1982, '7:11', 'Sugar Hill', ARRAY['Hip-Hop']),
        ('Rapper''s Delight', 'The Sugarhill Gang', 'Sugarhill Gang', 1979, '14:35', 'Sugar Hill', ARRAY['Hip-Hop']),
        ('C.R.E.A.M.', 'Wu-Tang Clan', 'Enter the Wu-Tang', 1993, '4:12', 'Loud', ARRAY['Hip-Hop']),

        -- Indie/Alternative tracks
        ('Just Like Honey', 'The Jesus and Mary Chain', 'Psychocandy', 1985, '3:07', 'Blanco y Negro', ARRAY['Indie', 'Indie Rock', 'Alternative', 'Shoegaze']),
        ('Age of Consent', 'New Order', 'Power, Corruption & Lies', 1983, '5:15', 'Factory', ARRAY['Indie', 'Alternative', 'Post-Punk']),
        ('Only Shallow', 'My Bloody Valentine', 'Loveless', 1991, '4:17', 'Creation', ARRAY['Indie Rock', 'Shoegaze', 'Alternative']),
        ('There Is a Light That Never Goes Out', 'The Smiths', 'The Queen Is Dead', 1986, '4:02', 'Rough Trade', ARRAY['Indie', 'Alternative']),
        ('Ceremony', 'New Order', 'Ceremony', 1981, '4:28', 'Factory', ARRAY['Indie', 'Post-Punk', 'Alternative']),

        -- Soul/R&B/Funk tracks
        ('Superstition', 'Stevie Wonder', 'Talking Book', 1972, '4:26', 'Tamla', ARRAY['Soul/R&B', 'Funk', 'Soul', 'R&B', 'Soul/Funk', 'Disco']),
        ('I Heard It Through the Grapevine', 'Marvin Gaye', 'In the Groove', 1968, '3:17', 'Tamla', ARRAY['Soul/R&B', 'Soul']),
        ('Flash Light', 'Parliament', 'Funkentelechy vs. the Placebo Syndrome', 1977, '5:47', 'Casablanca', ARRAY['Funk', 'Soul/R&B', 'Soul/Funk']),
        ('Le Freak', 'Chic', 'C''est Chic', 1978, '5:23', 'Atlantic', ARRAY['Funk', 'Disco', 'Soul/R&B', 'Soul/Funk']),
        ('Ain''t No Mountain High Enough', 'Marvin Gaye & Tammi Terrell', 'United', 1967, '2:30', 'Tamla', ARRAY['Soul/R&B', 'Soul']),

        -- Punk/Hardcore tracks
        ('Anarchy in the U.K.', 'Sex Pistols', 'Never Mind the Bollocks', 1977, '3:33', 'Virgin', ARRAY['Punk', 'Hardcore', 'Post-Punk']),
        ('Blitzkrieg Bop', 'Ramones', 'Ramones', 1976, '2:12', 'Sire', ARRAY['Punk', 'Hardcore', 'Garage Rock']),
        ('Rise Above', 'Black Flag', 'Damaged', 1981, '2:27', 'SST', ARRAY['Punk', 'Hardcore']),
        ('Minor Threat', 'Minor Threat', 'Minor Threat EP', 1981, '1:41', 'Dischord', ARRAY['Hardcore', 'Punk', 'Emo']),
        ('Complete Control', 'The Clash', 'The Clash (US)', 1977, '3:13', 'CBS', ARRAY['Punk', 'Post-Punk']),

        -- Metal tracks
        ('Master of Puppets', 'Metallica', 'Master of Puppets', 1986, '8:35', 'Elektra', ARRAY['Metal', 'Thrash Metal']),
        ('Paranoid', 'Black Sabbath', 'Paranoid', 1970, '2:48', 'Vertigo', ARRAY['Metal', 'Doom Metal', 'Doom & Stoner']),
        ('Raining Blood', 'Slayer', 'Reign in Blood', 1986, '4:17', 'Def Jam', ARRAY['Metal', 'Thrash Metal']),
        ('Dopethrone', 'Electric Wizard', 'Dopethrone', 2000, '5:45', 'Rise Above', ARRAY['Doom Metal', 'Doom & Stoner', 'Stoner Rock']),
        ('Freezing Moon', 'Mayhem', 'De Mysteriis Dom Sathanas', 1994, '6:23', 'Deathlike Silence', ARRAY['Black Metal', 'Metal']),

        -- Folk/Americana tracks
        ('The Times They Are a-Changin''', 'Bob Dylan', 'The Times They Are a-Changin''', 1964, '3:15', 'Columbia', ARRAY['Folk', 'Acoustic']),
        ('Big Yellow Taxi', 'Joni Mitchell', 'Ladies of the Canyon', 1970, '2:15', 'Reprise', ARRAY['Folk']),
        ('Man of Constant Sorrow', 'The Stanley Brothers', 'For the Good People', 1960, '3:10', 'King', ARRAY['Bluegrass', 'Folk']),
        ('Foggy Mountain Breakdown', 'Earl Scruggs', 'Foggy Mountain Jamboree', 1957, '2:42', 'Columbia', ARRAY['Bluegrass']),

        -- Reggae/Dub tracks
        ('One Love / People Get Ready', 'Bob Marley & The Wailers', 'Exodus', 1977, '2:52', 'Island', ARRAY['Reggae', 'Dub/Reggae', 'Ska']),
        ('King Tubby Meets Rockers Uptown', 'Augustus Pablo', 'King Tubby Meets Rockers Uptown', 1976, '3:45', 'Clocktower', ARRAY['Dub/Reggae', 'Reggae']),
        ('A Message to You Rudy', 'The Specials', 'The Specials', 1979, '2:52', '2 Tone', ARRAY['Ska', 'Reggae']),
        ('Police and Thieves', 'Junior Murvin', 'Police and Thieves', 1976, '6:37', 'Island', ARRAY['Reggae', 'Dub/Reggae']),

        -- World Music tracks
        ('Pata Pata', 'Miriam Makeba', 'Pata Pata', 1967, '2:49', 'Reprise', ARRAY['World', 'Afrobeat', 'Global Bass']),
        ('Zombie', 'Fela Kuti', 'Zombie', 1976, '12:26', 'Coconut', ARRAY['Afrobeat', 'World']),
        ('Mas Que Nada', 'Sergio Mendes', 'Sergio Mendes & Brasil ''66', 1966, '2:35', 'A&M', ARRAY['Latin', 'World', 'Cumbia']),
        ('Oye Como Va', 'Tito Puente', 'El Rey Bravo', 1963, '4:24', 'Tico', ARRAY['Latin', 'World']),

        -- Classical tracks
        ('Clair de Lune', 'Claude Debussy', 'Suite bergamasque', 1905, '4:37', NULL, ARRAY['Classical', 'Opera']),
        ('The Four Seasons: Spring', 'Antonio Vivaldi', 'The Four Seasons', 1725, '10:25', NULL, ARRAY['Classical', 'Opera']),
        ('Symphony No. 5', 'Ludwig van Beethoven', 'Symphony No. 5', 1808, '7:20', NULL, ARRAY['Classical', 'Opera']),

        -- Gospel tracks
        ('Oh Happy Day', 'Edwin Hawkins Singers', 'Let Us Go Into the House of the Lord', 1968, '4:50', 'Pavilion', ARRAY['Gospel']),

        -- Country tracks
        ('Ring of Fire', 'Johnny Cash', 'Ring of Fire: The Best of Johnny Cash', 1963, '2:36', 'Columbia', ARRAY['Country']),

        -- Dance/House tracks
        ('Lady (Hear Me Tonight)', 'Modjo', 'Modjo', 2000, '5:07', 'Soundofbarclay', ARRAY['House', 'Deep House', 'Dance']),

        -- Drum & Bass tracks
        ('Inner City Life', 'Goldie', 'Timeless', 1995, '10:30', 'FFRR', ARRAY['Drum & Bass']),

        -- Britpop tracks
        ('Parklife', 'Blur', 'Parklife', 1994, '3:06', 'Food', ARRAY['Britpop', 'Indie Rock']),

        -- Surf Rock tracks
        ('Misirlou', 'Dick Dale', 'Surfers'' Choice', 1962, '2:14', 'Deltone', ARRAY['Surf Rock']),

        -- Post-Rock tracks
        ('Mogwai Fear Satan', 'Mogwai', 'Young Team', 1997, '16:19', 'Chemikal Underground', ARRAY['Post-Rock']),

        -- Industrial tracks
        ('Head Like a Hole', 'Nine Inch Nails', 'Pretty Hate Machine', 1989, '4:58', 'TVT', ARRAY['Industrial']),

        -- Witch House tracks
        ('†OGETHER (SALEM REDUX)', 'SALEM', 'Yes I Smoke Crack', 2010, '3:48', NULL, ARRAY['Witch House']),

        -- Vaporwave tracks
        ('リサフランク420 / 現代のコンピュー', 'Macintosh Plus', 'Floral Shoppe', 2011, '7:44', NULL, ARRAY['Vaporwave']),

        -- Beats/Lo-Fi tracks
        ('Luv(sic) Part 3', 'Nujabes', 'Luv(sic) Hexalogy', 2008, '5:35', 'Hydeout Productions', ARRAY['Beats', 'Lo-Fi', 'Lo-Fi Hip-Hop']),

        -- Downtempo tracks
        ('Teardrop', 'Massive Attack', 'Mezzanine', 1998, '5:29', 'Circa', ARRAY['Downtempo']),

        -- Caribbean tracks
        ('Hot Hot Hot', 'Arrow', 'Hot Hot Hot', 1982, '3:52', 'London', ARRAY['Caribbean']),

        -- Generic/filler tracks for variety (these match ANY genre)
        ('Sweet Dreams', 'Eurythmics', 'Sweet Dreams', 1983, '3:36', 'RCA', ARRAY['Pop', 'Eclectic', 'Variety']),
        ('Don''t Stop Believin''', 'Journey', 'Escape', 1981, '4:11', 'Columbia', ARRAY['Pop', 'Rock', 'Eclectic', 'Variety']),
        ('Heart of Glass', 'Blondie', 'Parallel Lines', 1978, '5:50', 'Chrysalis', ARRAY['Pop', 'New Wave', 'Eclectic', 'Variety']),
        ('Video Killed the Radio Star', 'The Buggles', 'The Age of Plastic', 1979, '4:12', 'Island', ARRAY['Pop', 'New Wave', 'Eclectic', 'Variety']),
        ('Bizarre Love Triangle', 'New Order', 'Brotherhood', 1986, '4:21', 'Factory', ARRAY['New Wave', 'Electronic', 'Eclectic', 'Variety']),
        ('Tainted Love', 'Soft Cell', 'Non-Stop Erotic Cabaret', 1981, '2:43', 'Some Bizzare', ARRAY['New Wave', 'Electronic', 'Eclectic', 'Variety']),
        ('Common People', 'Pulp', 'Different Class', 1995, '5:50', 'Island', ARRAY['Britpop', 'Indie', 'Eclectic', 'Variety']),
        ('Girls & Boys', 'Blur', 'Parklife', 1994, '4:50', 'Food', ARRAY['Britpop', 'Indie', 'Eclectic', 'Variety']),
        ('Song 2', 'Blur', 'Blur', 1997, '2:02', 'Food', ARRAY['Britpop', 'Alternative', 'Eclectic', 'Variety']),
        ('Bitter Sweet Symphony', 'The Verve', 'Urban Hymns', 1997, '5:58', 'Hut', ARRAY['Britpop', 'Alternative', 'Eclectic', 'Variety'])
    ) AS tracks(title, artist, album, year, duration, label, genres)
)
INSERT INTO episode_tracks (episode_id, track_number, title, artist)
SELECT
    e.id as episode_id,
    track_num as track_number,
    tl.title,
    tl.artist
FROM episodes e
JOIN shows s ON s.id = e.show_id
CROSS JOIN LATERAL (
    SELECT track_num
    FROM generate_series(1, 10) AS track_num
) track_nums
CROSS JOIN LATERAL (
    SELECT *
    FROM track_library tl
    WHERE s.genre = ANY(tl.genres)
    ORDER BY random()
    LIMIT 1
) tl;

-- Display summary
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
    'Events' as metric,
    COUNT(*) as count
FROM events
UNION ALL
SELECT
    'Event Tags' as metric,
    COUNT(*) as count
FROM event_tags
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

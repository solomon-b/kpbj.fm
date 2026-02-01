-- Episode Tracks
-- Track library and generation for episode playlists
-- Each episode gets 10 tracks matched to their show's genre tags

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
    WHERE EXISTS (
        SELECT 1
        FROM show_tag_assignments sta
        JOIN show_tags st ON st.id = sta.tag_id
        WHERE sta.show_id = s.id
        AND st.name = ANY(tl.genres)
    )
    ORDER BY random()
    LIMIT 1
) tl;

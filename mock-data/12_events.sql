-- Events
-- Community events (1 per week for several months)

INSERT INTO events (title, slug, description, starts_at, ends_at, location_name, location_address, poster_image_url, status, author_id)
SELECT * FROM (VALUES
  (
    'Summer Block Party',
    'summer-block-party-2025-08-08',
    'Join us for an afternoon of live music, local vendors, and community celebration! Featuring performances from local bands and DJs. Free admission, all ages welcome.',
    '2025-08-08 14:00:00-07'::timestamptz,
    '2025-08-08 22:00:00-07'::timestamptz,
    'KPBJ Community Plaza',
    '123 Radio Street, Portland, OR 97201',
    'images/event-posters/2025/01/01/summer-block-party-2025-08-08.jpg',
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
    'images/event-posters/2025/01/01/late-night-techno-dj-aurelia-2025-08-16.jpg',
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
    'images/event-posters/2025/01/01/record-fair-swap-meet-2025-08-22.jpg',
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
    'images/event-posters/2025/01/01/indie-rock-showcase-2025-08-30.jpg',
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
    'images/event-posters/2025/01/01/kpbj-fundraiser-jazz-night-2025-09-05.jpg',
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
    'images/event-posters/2025/01/01/all-ages-punk-matinee-2025-09-13.jpg',
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
    'images/event-posters/2025/01/01/open-mic-night-2025-09-19.jpg',
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
    'images/event-posters/2025/01/01/autumn-dj-showcase-2025-09-27.jpg',
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
    'images/event-posters/2025/01/01/halloween-costume-party-dance-night-2025-10-04.jpg',
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
    'images/event-posters/2025/01/01/local-legends-portland-music-history-panel-2025-10-11.jpg',
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
    'images/event-posters/2025/01/01/acoustic-songwriter-circle-2025-10-18.jpg',
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
    'images/event-posters/2025/01/01/kpbj-fall-fundraiser-silent-auction-2025-10-25.jpg',
    'published',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm')
  )
) AS events_data;

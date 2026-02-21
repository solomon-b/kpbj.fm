-- Blog Posts
-- Station blog content (1 per week for past 2 months)

INSERT INTO blog_posts (title, slug, content, excerpt, hero_image_url, author_id, status, published_at)
SELECT * FROM (VALUES
  (
    'KPBJ Wins 2025 Community Radio Excellence Award',
    'kpbj-wins-2025-community-radio-excellence-award',
    E'We are thrilled to announce that KPBJ 95.9FM has been recognized with the prestigious **2025 Community Radio Excellence Award** from the National Association of Community Broadcasters!\n\nThis honor recognizes our commitment to serving the Portland community through diverse programming, local artist support, and meaningful community engagement. The award specifically highlighted our innovative approach to combining traditional radio broadcasting with modern digital engagement.\n\n> "This award belongs to our entire community. From our dedicated volunteer hosts to our loyal listeners and supporters, everyone plays a vital role in making KPBJ the vibrant community resource it is today."\n> \n> — Station Manager Sarah Chen\n\nThe judging panel noted KPBJ''s exceptional programming diversity, with 84 unique shows covering everything from jazz and classical to experimental electronic and punk rock. They also praised our commitment to amplifying underrepresented voices in the music industry and providing a platform for emerging local artists.\n\n## What This Means for KPBJ\n\n- National recognition for our programming excellence\n- Increased visibility in the community radio landscape\n- Validation of our community-first approach\n- Inspiration to continue pushing boundaries\n\nThank you to everyone who makes KPBJ possible. Here''s to many more years of community-powered radio!',
    'KPBJ 95.9FM receives the 2025 Community Radio Excellence Award for outstanding programming and community engagement.',
    'images/blog-heroes/2025/01/01/kpbj-wins-2025-community-radio-excellence-award.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-04 10:00:00-07'::timestamptz
  ),
  (
    'Behind the Scenes: A Day in the Life of a KPBJ Host',
    'behind-the-scenes-day-in-life-kpbj-host',
    E'Ever wondered what it takes to produce a radio show? We sat down with **Marcus Williams**, host of "Late Night Jazz" every Thursday at 9pm, to get an inside look at the creative process behind community radio.\n\n## Morning: Digging Through the Crates\n\n> "I usually start my week by listening through new releases and revisiting classics. For a two-hour jazz show, I''ll typically prepare about 3-4 hours of material. You never know when you''ll want to extend a vibe or need to pivot the mood."\n> \n> — Marcus Williams\n\nMarcus spends Monday and Tuesday mornings at local record shops, browsing both new arrivals and used sections. "Some of my best discoveries have come from the dollar bins," he laughs.\n\n## Mid-Week: Building the Flow\n\nBy Wednesday, Marcus starts sequencing his show. "It''s like creating a mixtape for a friend. You want to take them on a journey. I usually start mellow, build energy in the middle hour, then wind down for the late-night crowd."\n\n## Show Night: Going Live\n\nThursday evening, Marcus arrives at the studio **two hours early**:\n\n- Reviews his notes\n- Double-checks that all records are cued up\n- Runs a sound check\n- Prepares backup selections\n\n"Once we go live, it''s just me, the music, and whoever''s listening out there in the Portland night."\n\n## The Reward\n\n> "The best part is the texts and calls we get. Knowing that someone driving home from a late shift or studying for an exam is vibing to the music you selected—that''s what community radio is all about."\n\n*Tune in to Late Night Jazz every Thursday at 9pm on KPBJ 95.9FM.*',
    'Meet Marcus Williams, host of Late Night Jazz, and discover what goes into creating a weekly radio show.',
    'images/blog-heroes/2025/01/01/behind-the-scenes-day-in-life-kpbj-host.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-11 14:00:00-07'::timestamptz
  ),
  (
    'New Shows Premiering This Fall: Expanding the KPBJ Sound',
    'new-shows-premiering-fall-2025',
    E'We''re excited to announce **three brand new shows** joining the KPBJ lineup this September!\n\n## "Queer Frequencies" - Mondays 8-10pm\n\nHosted by **DJ Phoenix**, this show celebrates LGBTQ+ artists and allies across all genres. From underground queer punk to mainstream pop allies, "Queer Frequencies" creates space for authentic voices and stories from the community.\n\n**What to expect:**\n- Diverse LGBTQ+ artists from all decades\n- Interviews with local queer musicians\n- Safe space for authentic expression\n\n## "Immigrant Stories" - Wednesdays 6-7pm\n\nProduced by a collective of first and second-generation immigrants, this show blends music from around the world with personal narratives about identity, belonging, and cultural fusion. Each week focuses on a different community within Portland''s diverse immigrant population.\n\n**Featured communities include:**\n- Vietnamese diaspora\n- East African refugees\n- Latin American immigrants\n- Middle Eastern communities\n\n## "Beats & Rhymes: Hip-Hop History" - Saturdays 3-5pm\n\nDJ Knowledge takes listeners on a **chronological journey through hip-hop**, starting from the Bronx in the 1970s and moving decade by decade. Each episode focuses on a specific year, exploring the cultural context and musical innovations that shaped the genre.\n\n---\n\nAll three shows represent KPBJ''s ongoing commitment to diverse, community-focused programming. **Tune in starting September 8th!**\n\n### Want to Host a Show?\n\nWe''re always looking for passionate community members with unique perspectives and musical knowledge. Visit [kpbj.fm/become-a-host](https://kpbj.fm/become-a-host) to learn more about our training program.',
    'Three exciting new shows join KPBJ this fall, expanding our commitment to diverse community voices.',
    'images/blog-heroes/2025/01/01/new-shows-premiering-fall-2025.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-18 09:00:00-07'::timestamptz
  ),
  (
    'Portland''s Underground Music Scene: Why Local Venues Matter',
    'portland-underground-music-scene-venues-matter',
    E'In an era of streaming algorithms and viral TikTok sounds, Portland''s independent music venues remain **vital spaces for authentic community** and artistic development. But these venues face ongoing challenges that threaten their survival.\n\n## The Ecosystem of Live Music\n\nLocal venues like **The Firehouse**, **The Underground**, and **Velvet Room** aren''t just concert spaces—they''re community centers where artists develop their craft, fans discover new sounds, and cultural movements take root.\n\n> "Every major Portland band you know started by playing small venues. You can''t skip those steps. The intimacy of a 100-capacity room teaches you how to connect with an audience in ways that streaming stats never will."\n> \n> — Sarah Kim, owner of The Firehouse\n\n## Economic Pressures\n\nRising rents, noise complaints, and increased insurance costs have forced several beloved venues to close in recent years. **The Eastside Music Hall** and **The Underground Station** both shuttered in 2024, leaving significant gaps in the city''s music infrastructure.\n\n### Challenges Facing Venues:\n\n- Rising commercial rent costs\n- Noise complaints from new residential developments\n- Increased liability insurance premiums\n- Competition from larger corporate venues\n- Thin profit margins on ticket sales\n\n## How You Can Help\n\n1. **Buy tickets in advance** - This helps venues plan and pay deposits\n2. **Purchase drinks and merch** - Most venues make minimal profit on tickets\n3. **Respect noise ordinances** - Be considerate to neighbors\n4. **Show up for matinees** - All-ages afternoon shows need support too\n5. **Tip your bartenders and sound techs** - They make it all possible\n\n---\n\nKPBJ partners with Portland venues to promote local shows and support emerging artists. Check our [events calendar](https://kpbj.fm/events) to find shows happening this week!',
    'Exploring the vital role of independent venues in Portland''s music ecosystem and how you can support them.',
    'images/blog-heroes/2025/01/01/portland-underground-music-scene-venues-matter.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-08-25 11:00:00-07'::timestamptz
  ),
  (
    'Fundraising Update: Summer Pledge Drive Results',
    'fundraising-update-summer-2025-pledge-drive',
    E'Thank you, Portland! Our **Summer 2025 Pledge Drive** exceeded all expectations, raising **$87,000** to support KPBJ''s operations and programming.\n\n## By the Numbers\n\n- **1,247** individual donors\n- **$70** average contribution\n- **412** new monthly sustainers\n- **89%** of our annual funding goal reached\n\n## What Your Support Provides\n\n- **Equipment maintenance** - New microphones, mixers, and transmitter upkeep\n- **Host training** - Workshops and technical education for volunteer DJs\n- **Music licensing** - Fees required to legally broadcast music\n- **Studio upgrades** - Improved recording capabilities for interviews and performances\n- **Community events** - Free concerts, workshops, and public gatherings\n\n## Special Thanks\n\nWe want to recognize several exceptional supporters:\n\n- **Sustainer Champion**: The Rodriguez Family, who committed to $100/month for the next year\n- **Business Partner**: Jackpot Records, who matched donations up to $5,000\n- **Volunteer Hero**: DJ Marcus (Late Night Jazz), who hosted 12 hours of live pledge drive programming\n\n## Not Too Late to Contribute\n\nWhile the official drive has ended, KPBJ accepts donations year-round. Monthly sustainers provide crucial stable funding that helps us plan programming and invest in improvements. Even **$10/month** makes a real difference.\n\nVisit [kpbj.fm/donate](https://kpbj.fm/donate) to set up your monthly contribution today. Every dollar directly supports independent, community-powered radio.\n\n---\n\n*Thank you for believing in the power of community media!*',
    'Summer 2025 pledge drive raises $87,000, exceeding goals and securing stable funding for community radio.',
    'images/blog-heroes/2025/01/01/fundraising-update-summer-2025-pledge-drive.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-01 10:00:00-07'::timestamptz
  ),
  (
    'Discovering Hidden Gems: KPBJ''s Guide to Vinyl Shopping in Portland',
    'discovering-hidden-gems-vinyl-shopping-portland',
    E'Portland''s vinyl scene offers incredible opportunities for music discovery, from legendary record stores to pop-up sales and estate finds. Our DJs share their favorite spots and strategies for building a collection.\n\n## The Legendary Stores\n\n### Jackpot Records (Hawthorne)\n> "The staff picks here are incredible. They really know their catalog and aren''t afraid to recommend obscure stuff."\n> \n> — DJ Phoenix\n\n### Everyday Music (Multiple locations)\nWith three locations and massive inventory, Everyday Music is where many DJs find rare imports and deleted pressings.\n\n### Mississippi Records\nSpecializing in reissues of global folk music, gospel, and vintage country.\n\n> "This place expanded my entire musical worldview."\n> \n> — DJ Knowledge\n\n## Hidden Gems\n\n- **Exiled Records** (SE Portland) - Punk, hardcore, and metal specialist with knowledgeable staff and regular in-store performances\n- **Tender Loving Empire** (Alberta) - Local artists and Pacific Northwest labels. Great for discovering Portland musicians\n\n## Tips from KPBJ DJs\n\n1. **Shop dollar bins first** - "I''ve found Coltrane and Mingus records for $1," says Marcus Williams. "Never skip the cheap stuff."\n2. **Ask about listening stations** - Most stores let you preview before buying\n3. **Follow store social media** - Many announce new arrivals and sales online\n4. **Bring a list** - But stay open to surprises\n5. **Check condition carefully** - Look at the vinyl itself, not just the sleeve\n6. **Support buying local** - Many Portland artists sell directly to local shops\n\n## Monthly Record Fairs\n\nThe **Portland Record Show** happens the last Sunday of every month at the Doubletree Hotel. Over 50 vendors from across the Pacific Northwest bring crates of vinyl, CDs, and music memorabilia.\n\nKPBJ will be hosting our own **Record Fair & Swap Meet** on August 22nd—check our [events page](https://kpbj.fm/events) for details!',
    'KPBJ DJs share their favorite Portland record stores and vinyl shopping strategies for music collectors.',
    'images/blog-heroes/2025/01/01/discovering-hidden-gems-vinyl-shopping-portland.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-08 13:00:00-07'::timestamptz
  ),
  (
    'Community Voices: How KPBJ Listeners Shape Our Programming',
    'community-voices-listeners-shape-programming',
    E'KPBJ isn''t just radio *for* the community—it''s radio *by* the community. Here''s how listener feedback and participation directly influence our programming decisions.\n\n## The Listener Survey\n\nEach year, we survey our audience about their listening habits, favorite shows, and what they''d like to hear more of. This year''s survey received **2,300+ responses** and revealed some fascinating patterns:\n\n- **67%** listen via streaming rather than traditional FM\n- **Most popular time slot**: Weeknight 8-11pm drive home hours\n- **Top requested new genre**: More experimental electronic and ambient\n- **Most appreciated aspect**: "Musical diversity I can''t find anywhere else"\n\n## Rotating Guest DJ Spots\n\nBased on listener requests, we''ve implemented **"Guest DJ Fridays"** where community members can apply to program a one-hour set. Applications open quarterly, and selected DJs receive training and technical support.\n\n> "I never thought I''d be on actual radio. The KPBJ team made it so welcoming and fun."\n> \n> — Priya Sharma, August Guest DJ (Bollywood disco remixes)\n\n## Call-In Requests and Dedications\n\nSeveral shows accept live requests via text and phone. These interactions create real-time dialogue between DJs and listeners:\n\n> "Someone texted during my show requesting an obscure Brazilian psych record. I didn''t have it, but I found something similar. We ended up texting back and forth about the genre. That''s the magic of community radio—you''re not just playing music into the void."\n> \n> — DJ Aurora\n\n## Your Voice Matters\n\nWe read every email, respond to social media comments, and seriously consider all feedback. The next listener survey launches in **November**—your input helps shape KPBJ''s future.\n\n**Have programming ideas?**\n- Email: [feedback@kpbj.fm](mailto:feedback@kpbj.fm)\n- Text during any show\n- Submit suggestions via our website\n\n*Community radio only works when the community participates!*',
    'How KPBJ incorporates listener feedback, surveys, and community participation into programming decisions.',
    'images/blog-heroes/2025/01/01/community-voices-listeners-shape-programming.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-15 10:30:00-07'::timestamptz
  ),
  (
    'Looking Ahead: KPBJ''s Plans for 2026',
    'looking-ahead-kpbj-plans-2026',
    E'As we approach the end of 2025, we''re excited to share our vision for KPBJ''s growth and evolution in the coming year.\n\n## Technical Upgrades\n\n### New Transmitter\nWe''re investing in upgraded broadcast equipment that will improve signal clarity and expand our coverage area.\n\n### Remote Broadcasting\nNew mobile equipment will allow us to broadcast live from community events, concerts, and festivals throughout Portland.\n\n### Improved Streaming\nWorking with technical partners to enhance our web player and launch a dedicated mobile app.\n\n## Programming Expansions\n\n### Podcast Studio\nConverting an unused storage room into a podcast production space available to community members. This will allow local podcasters to access professional equipment and training.\n\n### Youth Radio Program\nPartnering with **Portland Public Schools** to offer radio production classes and mentorship. Students will produce monthly shows featuring youth perspectives on music, culture, and social issues.\n\n### Live Performance Space\nPlanning a small performance venue for intimate concerts, workshops, and community gatherings. All events will be broadcast live.\n\n## Community Partnerships\n\n- **Local Venues** - Expanding partnerships with Portland music venues to promote local shows and develop co-produced content\n- **Arts Organizations** - Collaborating with museums, theaters, and galleries to create programming that bridges music with other art forms\n- **Nonprofit Network** - Working with other community organizations to amplify important local initiatives and social programs\n\n## How You Can Help\n\nThese ambitious plans require community support:\n\n- **Monthly sustainers** provide stable funding for long-term projects\n- **Volunteer skills** in construction, marketing, or teaching can directly support new initiatives\n- **Community feedback** helps us prioritize which projects matter most\n\n## Stay Involved\n\nWe''ll share regular updates about these projects throughout 2026. Follow us on social media, subscribe to our monthly newsletter, or attend our quarterly community meetings to stay informed and involved.\n\n---\n\n*Thank you for making KPBJ possible. The best is yet to come!*',
    'KPBJ announces ambitious plans for 2026 including technical upgrades, new programming, and expanded community partnerships.',
    'images/blog-heroes/2025/01/01/looking-ahead-kpbj-plans-2026.jpg',
    (SELECT id FROM users WHERE email = 'staff@kpbj.fm'),
    'published',
    '2025-09-22 09:00:00-07'::timestamptz
  )
) AS blog_data;

-- Assign tags to blog posts
-- Uses a values list of (slug, tag_name) pairs to support multiple tags per post
INSERT INTO blog_post_tags (post_id, tag_id)
SELECT bp.id, bt.id
FROM (VALUES
  ('kpbj-wins-2025-community-radio-excellence-award', 'award'),
  ('kpbj-wins-2025-community-radio-excellence-award', 'community'),
  ('behind-the-scenes-day-in-life-kpbj-host', 'interviews'),
  ('new-shows-premiering-fall-2025', 'new-shows'),
  ('new-shows-premiering-fall-2025', 'community'),
  ('portland-underground-music-scene-venues-matter', 'local-scene'),
  ('portland-underground-music-scene-venues-matter', 'underground'),
  ('portland-underground-music-scene-venues-matter', 'community'),
  ('fundraising-update-summer-2025-pledge-drive', 'fundraising'),
  ('fundraising-update-summer-2025-pledge-drive', 'community'),
  ('discovering-hidden-gems-vinyl-shopping-portland', 'music-discovery'),
  ('discovering-hidden-gems-vinyl-shopping-portland', 'local-scene'),
  ('community-voices-listeners-shape-programming', 'community'),
  ('looking-ahead-kpbj-plans-2026', 'community'),
  ('looking-ahead-kpbj-plans-2026', 'new-shows')
) AS pairs(post_slug, tag_name)
JOIN blog_posts bp ON bp.slug = pairs.post_slug
JOIN blog_tags bt ON bt.name = pairs.tag_name;

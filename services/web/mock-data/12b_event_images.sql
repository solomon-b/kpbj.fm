-- Event photo gallery
-- A few post-event photos attached to the Summer Block Party event so the
-- public event page has a gallery to display. Reuses existing mock poster
-- images so the <img> paths resolve.

INSERT INTO event_images (event_id, image_path, caption, alt_text, sort_order)
VALUES
  ((SELECT id FROM events WHERE slug = 'summer-block-party-2025-08-08'),
   'images/event-posters/2025/01/01/summer-block-party-2025-08-08.jpg',
   'The crowd at the plaza', 'Crowd gathered at the community plaza', 0),
  ((SELECT id FROM events WHERE slug = 'summer-block-party-2025-08-08'),
   'images/event-posters/2025/01/01/indie-rock-showcase-2025-08-30.jpg',
   'Live band on the main stage', 'A band performing on the main stage', 1),
  ((SELECT id FROM events WHERE slug = 'summer-block-party-2025-08-08'),
   'images/event-posters/2025/01/01/record-fair-swap-meet-2025-08-22.jpg',
   'Local vendors along the street', 'Vendor tables lining the street', 2);

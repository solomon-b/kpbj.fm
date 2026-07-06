-- Optional post-event photo gallery, one row per photo, ordered by sort_order.

CREATE TABLE event_images (
    id SERIAL8 PRIMARY KEY,
    event_id INT8 NOT NULL REFERENCES events(id) ON DELETE CASCADE,
    image_path TEXT NOT NULL,
    caption TEXT NOT NULL DEFAULT '',
    alt_text TEXT NOT NULL DEFAULT '',
    sort_order INT8 NOT NULL DEFAULT 0,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now()
);

CREATE INDEX idx_event_images_event_id ON event_images(event_id);

COMMENT ON TABLE event_images IS 'Optional post-event photo gallery, ordered by sort_order';

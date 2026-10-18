-- Add station announcements and show promos to break items.
--
-- An announcement is station news, such as an event, a fund drive, or a call
-- for volunteers. A show promo tells listeners about one show. Both are the
-- station's own content, so they do not use the underwriting category.
--
-- A host can submit a promo for a show. Staff review it before it airs. The
-- other categories come from staff, so they need no review.

ALTER DOMAIN break_item_category DROP CONSTRAINT break_item_category_check;
ALTER DOMAIN break_item_category ADD CONSTRAINT break_item_category_check
  CHECK (VALUE IN ('psa', 'underwriting', 'announcement', 'show_promo'));

ALTER TABLE break_items ADD COLUMN show_id BIGINT REFERENCES shows(id);

CREATE INDEX idx_break_items_show ON break_items (show_id)
    WHERE deleted_at IS NULL;

-- An underwriting row has an underwriter. A show promo has a show. A PSA and
-- an announcement have neither.
ALTER TABLE break_items DROP CONSTRAINT break_items_category_fields;
ALTER TABLE break_items ADD CONSTRAINT break_items_category_fields CHECK (
    (category IN ('psa', 'announcement') AND underwriter_id IS NULL AND show_id IS NULL)
    OR (category = 'underwriting' AND underwriter_id IS NOT NULL AND show_id IS NULL)
    OR (category = 'show_promo' AND show_id IS NOT NULL AND underwriter_id IS NULL)
);

-- A domain over TEXT rather than an enum, for the same reason as
-- break_item_category. Rel8 writes this column and casts the value to text,
-- which an enum column rejects and a text domain accepts.
CREATE DOMAIN break_item_review_status AS TEXT
  CHECK (VALUE IN ('pending', 'approved', 'rejected'));

ALTER TABLE break_items
  ADD COLUMN review_status break_item_review_status NOT NULL DEFAULT 'approved',
  ADD COLUMN review_note TEXT;

-- Only a show promo goes through review. Every other row is approved.
ALTER TABLE break_items ADD CONSTRAINT break_items_review_category CHECK (
    category = 'show_promo' OR review_status = 'approved'
);

-- A note explains a rejection, so only a rejected row has one.
ALTER TABLE break_items ADD CONSTRAINT break_items_review_note CHECK (
    review_note IS NULL OR review_status = 'rejected'
);

ALTER TABLE playback_history DROP CONSTRAINT playback_history_source_type_check;
ALTER TABLE playback_history ADD CONSTRAINT playback_history_source_type_check
  CHECK (source_type IN (
    'episode', 'ephemeral', 'station_id', 'psa', 'underwriting', 'announcement', 'show_promo'
  ));

COMMENT ON TABLE break_items IS 'PSAs, underwriting announcements, station announcements, and show promos that fill breaks';
COMMENT ON COLUMN break_items.underwriter_id IS 'The underwriter this announcement acknowledges. NULL for every other category';
COMMENT ON COLUMN break_items.show_id IS 'The show this promo tells listeners about. NULL for every other category';
COMMENT ON COLUMN break_items.review_status IS 'Staff review of a show promo. Only an approved row airs. Every other category is always approved';
COMMENT ON COLUMN break_items.review_note IS 'Why staff rejected this promo, for the host. NULL unless the promo is rejected';

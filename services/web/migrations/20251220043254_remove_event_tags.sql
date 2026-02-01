-- Remove event tags feature entirely
-- Drop the junction table first (depends on event_tags)
DROP TABLE IF EXISTS event_tag_assignments;

-- Drop the event_tags table
DROP TABLE IF EXISTS event_tags;

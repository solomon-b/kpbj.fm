-- Unscheduled Episodes
-- One unscheduled episode for sunday-morning-jazz (used by e2e schedule tests)

INSERT INTO episodes (show_id, schedule_template_id, description, scheduled_at, published_at, created_by)
SELECT s.id, NULL, 'Unscheduled jazz episode for e2e testing', NULL, NOW(), u.id
FROM shows s
JOIN users u ON u.email = 'host-sunday-morning-jazz@kpbj.fm'
WHERE s.slug = 'sunday-morning-jazz';

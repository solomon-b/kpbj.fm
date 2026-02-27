ALTER TABLE episodes ALTER COLUMN schedule_template_id DROP NOT NULL;
ALTER TABLE episodes ALTER COLUMN scheduled_at DROP NOT NULL;

ALTER TABLE episodes ADD CONSTRAINT episodes_schedule_consistency
  CHECK (
    (schedule_template_id IS NULL AND scheduled_at IS NULL) OR
    (schedule_template_id IS NOT NULL AND scheduled_at IS NOT NULL)
  );

-- Add schedule_template_id to episodes table
-- Links each episode to the schedule template that defined its time slot

-- Add column
ALTER TABLE episodes ADD COLUMN schedule_template_id BIGINT NOT NULL;

-- Make scheduled_at NOT NULL (was previously nullable)
ALTER TABLE episodes ALTER COLUMN scheduled_at SET NOT NULL;

-- Add foreign key constraint
ALTER TABLE episodes ADD CONSTRAINT fk_episodes_schedule_template
    FOREIGN KEY (schedule_template_id) REFERENCES schedule_templates(id);

-- Add index for efficient lookups
CREATE INDEX idx_episodes_schedule_template_id ON episodes(schedule_template_id);

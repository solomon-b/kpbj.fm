-- A soft-deleted episode kept its time slot forever.
--
-- unique_episode_scheduled_at covered every row, deleted ones included. A slot that a
-- deleted episode once held therefore stayed taken. Nothing could reuse it, and a new
-- episode on that date failed the constraint.
--
-- The index below covers only the live rows, so a delete frees the slot. It is weaker
-- than the constraint it replaces, so every row that satisfies the old rule satisfies
-- the new one. It applies with no data repair.
--
-- The index keeps the constraint's name. The rule is the same, and only its scope
-- changes. PostgreSQL has no partial UNIQUE constraint. This is therefore an index
-- rather than a table constraint. No code refers to the name.
--
-- A deleted episode never airs. getCurrentlyAiringEpisodes filters on
-- e.deleted_at IS NULL, so a reused timestamp cannot put two rows on the stream.
--
-- scheduled_at stays nullable, and NULL values still never collide. A show can hold
-- many unscheduled episodes, which is the state a removed slot leaves behind.

ALTER TABLE episodes DROP CONSTRAINT unique_episode_scheduled_at;

CREATE UNIQUE INDEX unique_episode_scheduled_at
  ON episodes (show_id, scheduled_at)
  WHERE deleted_at IS NULL;

COMMENT ON INDEX unique_episode_scheduled_at IS
  'One live episode per show per air time. A soft-deleted episode releases its slot.';

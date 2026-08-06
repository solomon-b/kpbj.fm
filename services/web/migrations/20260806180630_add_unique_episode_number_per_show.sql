-- Two episodes of one show could hold the same episode number.
--
-- set_episode_number() reads MAX(episode_number) + 1 in a BEFORE INSERT trigger and
-- takes no lock. Two uploads for one show that run at the same time read the same MAX
-- and both write N + 1. getEpisodeByShowAndNumber returns an arbitrary one of the two,
-- so an edit or a delete can reach the wrong episode. The episode detail URL carries
-- the number, so the two episodes also share a public URL.
--
-- This migration makes two changes, and both are necessary. The trigger takes a
-- per-show lock before it reads MAX, so the second insert waits and reads the first
-- one's number. The constraint then makes the invariant a property of the table
-- rather than of the trigger. Without the lock, the second insert fails on the
-- constraint after the upload already stored its audio file.

CREATE OR REPLACE FUNCTION set_episode_number()
RETURNS TRIGGER AS $$
BEGIN
    -- Assign a number when the insert carries the column default or NULL.
    IF NEW.episode_number = 1 OR NEW.episode_number IS NULL THEN
        -- Serialize the read and the write below against other inserts for this show.
        -- The key is 64 bits. The high half is a namespace, so this lock cannot
        -- collide with any other advisory lock the application takes later. The low
        -- half is the show id. The lock releases at commit, and it holds no row. It
        -- therefore cannot contend with a show edit or a schedule write.
        PERFORM pg_advisory_xact_lock((1::BIGINT << 32) | NEW.show_id);

        -- Get the next episode number for this show
        SELECT COALESCE(MAX(episode_number), 0) + 1
        INTO NEW.episode_number
        FROM episodes
        WHERE show_id = NEW.show_id;
    END IF;
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

-- A soft-deleted episode keeps its number, and MAX still counts it. Nothing reuses a
-- number. The constraint therefore covers the deleted rows too, and it needs no
-- deleted_at predicate.
ALTER TABLE episodes
  ADD CONSTRAINT unique_episode_number UNIQUE (show_id, episode_number);

COMMENT ON CONSTRAINT unique_episode_number ON episodes IS
  'One episode number per show. set_episode_number assigns it under a per-show advisory lock.';

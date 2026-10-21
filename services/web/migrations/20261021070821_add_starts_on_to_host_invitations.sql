-- Add a start date to host invitations.
--
-- An invitation holds a schedule slot. Before this change the slot started on
-- the date the host claimed the invitation. Staff could not invite a host into
-- a slot that becomes free on a later date. The new show now starts on
-- starts_on, or on the claim date if starts_on has passed.
--
-- Current rows get the date that matches the old behavior. A claimed row gets
-- the Pacific date of claimed_at. Every other row gets the Pacific date of
-- created_at. The app computes "today" in America/Los_Angeles, so the backfill
-- uses the same timezone.

ALTER TABLE host_invitations
  ADD COLUMN starts_on DATE;

UPDATE host_invitations
SET starts_on = (COALESCE(claimed_at, created_at) AT TIME ZONE 'America/Los_Angeles')::date;

ALTER TABLE host_invitations
  ALTER COLUMN starts_on SET NOT NULL;

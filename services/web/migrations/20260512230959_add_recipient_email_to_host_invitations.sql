-- Add recipient_email to host_invitations.
--
-- Adds the column as nullable, backfills existing rows with a placeholder,
-- then enforces NOT NULL. Indexes the column for the existing-user collision
-- check performed on invitation creation.

ALTER TABLE host_invitations
  ADD COLUMN recipient_email TEXT;

UPDATE host_invitations
  SET recipient_email = 'unknown@kpbj.fm'
  WHERE recipient_email IS NULL;

ALTER TABLE host_invitations
  ALTER COLUMN recipient_email SET NOT NULL;

CREATE INDEX idx_host_invitations_recipient_email
  ON host_invitations(recipient_email);

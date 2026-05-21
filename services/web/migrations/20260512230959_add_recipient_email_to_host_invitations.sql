-- Add recipient_email to host_invitations and lock token format.
--
-- The new onboarding flow binds each invitation to a specific email address
-- (server-side equality check on claim), so legacy invitations without a
-- recipient are unclaimable. Wipe them rather than carry a placeholder.
-- Claimed/expired/revoked legacy rows are inert; show ownership history is
-- maintained in shows / show_hosts, not here.
--
-- Also locks the token column to the new INV-XXXX-XXXX format
-- (Crockford-style base32, excluding 0/1/O/I/L for visual disambiguation;
-- 30-char alphabet, dash-separated 4-char groups).

DELETE FROM host_invitations;

ALTER TABLE host_invitations
  ADD COLUMN recipient_email TEXT NOT NULL;

CREATE INDEX idx_host_invitations_recipient_email
  ON host_invitations(recipient_email);

ALTER TABLE host_invitations
  ADD CONSTRAINT host_invitations_token_format
  CHECK (token ~ '^INV-[2-9A-HJKMNP-TV-Z]{4}-[2-9A-HJKMNP-TV-Z]{4}$');

-- Mailchimp sync state for each newsletter subscriber.
-- Captures the per-row Mailchimp identity, last-known status, and last sync time.
-- Existing rows take NULL on all three columns and will be backfilled by the
-- one-time `kpbj-mailchimp-reconcile --bootstrap` job.

ALTER TABLE newsletter_subscribers
  ADD COLUMN mailchimp_member_id TEXT NULL,
  ADD COLUMN mailchimp_status    TEXT NULL,
  ADD COLUMN mailchimp_synced_at TIMESTAMPTZ NULL;

CREATE INDEX idx_newsletter_subscribers_mc_status
  ON newsletter_subscribers(mailchimp_status);

COMMENT ON COLUMN newsletter_subscribers.mailchimp_member_id
  IS 'Mailchimp subscriber hash (md5(lowercase(email))); null until first sync.';
COMMENT ON COLUMN newsletter_subscribers.mailchimp_status
  IS 'Last-known Mailchimp status; null = never synced.';
COMMENT ON COLUMN newsletter_subscribers.mailchimp_synced_at
  IS 'Last successful push or pull timestamp.';

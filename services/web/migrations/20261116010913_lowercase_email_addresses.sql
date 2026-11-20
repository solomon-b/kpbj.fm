-- Change every stored email address to lower case.
--
-- The EmailAddress type of web-server-core now changes an address to lower
-- case when it parses it. The email columns are plain text, so a lookup is
-- case sensitive. A row with a capital letter would not match a lookup.
--
-- users.email and newsletter_subscribers.email are unique. This migration
-- fails if two rows of one table differ only in case. Merge those rows by
-- hand first.
--
-- The three columns that the app reads as an EmailAddress also get a CHECK
-- constraint. The app writes lower case only. The constraint stops a write
-- with a capital letter from psql or from a script.

UPDATE users
SET email = lower(email)
WHERE email <> lower(email);

UPDATE newsletter_subscribers
SET email = lower(email)
WHERE email <> lower(email);

UPDATE host_invitations
SET recipient_email = lower(recipient_email)
WHERE recipient_email <> lower(recipient_email);

UPDATE email_verification_tokens
SET email = lower(email)
WHERE email <> lower(email);

UPDATE password_reset_tokens
SET email = lower(email)
WHERE email <> lower(email);

UPDATE orders
SET email = lower(email)
WHERE email <> lower(email);

UPDATE store_settings
SET order_notification_email = lower(order_notification_email)
WHERE order_notification_email <> lower(order_notification_email);

ALTER TABLE users
  ADD CONSTRAINT users_email_lowercase CHECK (email = lower(email));

ALTER TABLE newsletter_subscribers
  ADD CONSTRAINT newsletter_subscribers_email_lowercase CHECK (email = lower(email));

ALTER TABLE host_invitations
  ADD CONSTRAINT host_invitations_recipient_email_lowercase CHECK (recipient_email = lower(recipient_email));

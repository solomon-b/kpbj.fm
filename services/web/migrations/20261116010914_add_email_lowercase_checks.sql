-- Require lower case in three more email columns.
--
-- The app now reads these columns as an EmailAddress, as it does users.email.
-- The app writes lower case only. The constraint stops a write with a capital
-- letter from psql or from a script. An earlier migration changed the stored
-- values to lower case.

ALTER TABLE orders
  ADD CONSTRAINT orders_email_lowercase CHECK (email = lower(email));

ALTER TABLE email_verification_tokens
  ADD CONSTRAINT email_verification_tokens_email_lowercase CHECK (email = lower(email));

ALTER TABLE password_reset_tokens
  ADD CONSTRAINT password_reset_tokens_email_lowercase CHECK (email = lower(email));

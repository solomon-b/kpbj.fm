-- Verify All Users
-- Marks all mock data user accounts as email verified

UPDATE users
SET email_verified = TRUE,
    email_verified_at = NOW()
WHERE email_verified = FALSE;

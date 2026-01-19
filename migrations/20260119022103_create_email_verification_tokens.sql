-- Create email verification tokens table
--
-- Stores tokens sent to users to verify their email addresses.
-- Tokens expire after 24 hours by default.

-- Domain for verification status
CREATE DOMAIN verification_status AS TEXT
CHECK (VALUE IN ('pending', 'verified', 'expired'));

-- Email verification tokens table
CREATE TABLE email_verification_tokens (
    id SERIAL8 PRIMARY KEY,
    user_id INT8 NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    token VARCHAR(64) NOT NULL UNIQUE,
    email VARCHAR NOT NULL,
    status verification_status NOT NULL DEFAULT 'pending',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL DEFAULT (now() + INTERVAL '24 hours'),
    verified_at TIMESTAMPTZ
);

-- Index for looking up tokens
CREATE INDEX idx_verification_tokens_token ON email_verification_tokens(token);

-- Index for finding tokens by user
CREATE INDEX idx_verification_tokens_user_id ON email_verification_tokens(user_id);

-- Index for cleanup of expired pending tokens
CREATE INDEX idx_verification_tokens_expires_at ON email_verification_tokens(expires_at)
WHERE status = 'pending';

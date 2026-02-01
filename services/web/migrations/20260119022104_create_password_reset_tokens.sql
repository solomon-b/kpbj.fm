-- Password reset tokens table for password recovery functionality.
--
-- Tokens are single-use, expire after 1 hour, and can only be consumed
-- while in 'pending' status. All sessions are invalidated after successful reset.

CREATE TYPE password_reset_status AS ENUM ('pending', 'used', 'expired');

CREATE TABLE password_reset_tokens (
    id SERIAL8 PRIMARY KEY,
    user_id INT8 NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    token VARCHAR(64) NOT NULL UNIQUE,
    email VARCHAR NOT NULL,
    status password_reset_status NOT NULL DEFAULT 'pending',
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL DEFAULT (now() + INTERVAL '1 hour'),
    used_at TIMESTAMPTZ,
    ip_address VARCHAR(45),
    user_agent TEXT
);

-- Index for token lookup (primary query path)
CREATE INDEX idx_password_reset_tokens_token ON password_reset_tokens(token);

-- Index for rate limiting queries (count recent tokens per email)
CREATE INDEX idx_password_reset_tokens_email_created ON password_reset_tokens(email, created_at DESC);

-- Index for cleanup of expired pending tokens
CREATE INDEX idx_password_reset_tokens_expires_at ON password_reset_tokens(expires_at) WHERE status = 'pending';

COMMENT ON TABLE password_reset_tokens IS 'Stores password reset tokens with 1-hour expiration and single-use enforcement';
COMMENT ON COLUMN password_reset_tokens.token IS 'UUID v4 token (hyphen-free) included in reset email link';
COMMENT ON COLUMN password_reset_tokens.status IS 'Token lifecycle: pending (created) -> used (consumed) or expired (timed out)';
COMMENT ON COLUMN password_reset_tokens.ip_address IS 'IP address of requester for audit logging';
COMMENT ON COLUMN password_reset_tokens.user_agent IS 'User agent of requester for audit logging';

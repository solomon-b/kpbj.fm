-- Host invitation tokens for new host onboarding.
--
-- Staff generate invitation links with pre-configured schedules.
-- New hosts use the link to create their account and show together.
-- Tokens expire after 7 days and are single-use.

CREATE TYPE host_invitation_status AS ENUM ('pending', 'claimed', 'expired', 'revoked');

CREATE TABLE host_invitations (
    id SERIAL8 PRIMARY KEY,
    token TEXT NOT NULL UNIQUE,
    status host_invitation_status NOT NULL DEFAULT 'pending',
    schedule_data JSONB NOT NULL,
    created_by INT8 NOT NULL REFERENCES users(id),
    claimed_by INT8 REFERENCES users(id),
    claimed_at TIMESTAMPTZ,
    created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL DEFAULT (now() + INTERVAL '7 days')
);

CREATE INDEX idx_host_invitations_token ON host_invitations(token);
CREATE INDEX idx_host_invitations_status ON host_invitations(status);
CREATE INDEX idx_host_invitations_expires_at ON host_invitations(expires_at) WHERE status = 'pending';

COMMENT ON TABLE host_invitations IS 'Host invitation tokens with 7-day expiration for new host onboarding';

-- Newsletter subscribers table for the homepage mailing-list signup.
-- Replaces the previous Google Forms integration so subscriber data lives
-- alongside the rest of the application data.

CREATE TABLE newsletter_subscribers (
    id BIGSERIAL PRIMARY KEY,
    email TEXT NOT NULL UNIQUE,
    created_at TIMESTAMPTZ NOT NULL DEFAULT NOW()
);

CREATE INDEX idx_newsletter_subscribers_created_at
    ON newsletter_subscribers(created_at DESC);

COMMENT ON TABLE newsletter_subscribers
    IS 'Email subscribers from the homepage newsletter signup form';
COMMENT ON COLUMN newsletter_subscribers.email
    IS 'Subscriber email address; unique to silently swallow duplicate signups';
COMMENT ON COLUMN newsletter_subscribers.created_at
    IS 'When the email was first submitted';

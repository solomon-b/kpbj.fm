-- Host Users
-- Creates one user account per show (password: "password")

INSERT INTO users (email, password)
SELECT
    'host-' || slug || '@kpbj.fm',
    '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I'
FROM shows
ON CONFLICT (email) DO NOTHING;

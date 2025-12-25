-- Admin User
-- Password: "password"

INSERT INTO users (email, password) VALUES
  ('admin@kpbj.fm', '$argon2id$v=19$m=65536,t=2,p=1$IBMrLbPnVTsuf02vfr/jbA$u5UMpeqN0c7BR2L/AlIKpwk6lQ1E4y1j7OGXvRb7X5I')
ON CONFLICT (email) DO NOTHING;

INSERT INTO user_metadata (user_id, display_name, full_name, avatar_url, user_role)
SELECT u.id, 'Admin', 'KPBJ Administrator', 'images/2025/01/01/avatars/admin.jpg', 'Admin'
FROM users u WHERE u.email = 'admin@kpbj.fm';
